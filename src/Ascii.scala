// for disabling compiler warnings about postfixes
import scala.language.postfixOps
import scala.collection.mutable.Set

import scala.io.Source

abstract sealed class Direction(magnitude: Int = 1) {}
object LEFT extends Direction
object RIGHT extends Direction
object UP extends Direction
object DOWN extends Direction

abstract sealed class DoType{}
object WHILE extends DoType
object IF extends DoType

class OBJ {

  def THEN(dir: Direction): OBJ = {
    this
  }

  def DO(dType: DoType) = {
    this
  }

  def DO(times: Int) = {
    this
  }

   def EXISTS(dir: Direction): OBJ = {
    this
  }

  def AND(dir: Direction): OBJ = {
    this
  }

  def OR(dir:Direction): OBJ = {
    this
  }

  def END() = {}

  def TIMES() = {}

  def FALSE() = {}
}

object SWALLOW extends OBJ {}

object ASCII extends OBJ { ascii_obj =>

  var availableCharacters = Array('@', '|', '+', '-', '0')
  var isDrawing = true
  var return_early = false
  var width = 25
  var height = 15

  var grid = Array.fill[Char](height, width) { '0' }
  var cursor = (0,0)
  var character = '@'
  var last_grid = Array.fill[Char](height, width) { '0' }
  var last_cursor = (0,0)
  var last_character = '@'

  var move_path: Array[Direction] = new Array[Direction](0)

  var move_delta = (0, 0)

  // commands
  def WIDTH(w: Int) = {
    if (w <= 0) {
      println("Sorry, width must be greater than 0.")
    } else {
      width = w
      grid = Array.fill[Char](height, width) { '0' }
    }
  }

  def HEIGHT(h: Int) = {
    if (h <= 0) {
      println("Sorry, height must be greater than 0.")
    } else {
      height = h
      grid = Array.fill[Char](height, width) { '0' }
    }

  }

  def RENDER() = {
    for (column <- grid) {
      for (element <- column) {
        print(element)
      }
      print("\n")
    }
  }

  def SET() = {
    isDrawing = true
  }

  def UNSET() = {
    isDrawing = false
  }

  def MARKER(c: Char) = {
    if (availableCharacters contains c) {
      character = c
    } else {
      println("Available characters are '@', '|', '+', '-', '0'.")
    }
  }

  def MOVE(dir: Direction) = {

    reset_last_grid
    last_cursor = cursor
    last_character = character

    COND_MOVE(dir)

    this
  }

  def JUMP(new_pos: (Int, Int)) = {
    if (!in_bounds(new_pos)) {
      println("Jumped out of bounds.")
      return_early = true
    } else {
      cursor = new_pos
    }
    this
  }

  def RECT(size: (Int, Int)) = {
    val (width, height) = size
    for (x <- 1 to width) {
      ascii_obj COND_MOVE RIGHT
    }
    for (y <- 1 to height) {
      ascii_obj COND_MOVE DOWN
    }
    for (x <- 1 to width) {
      ascii_obj COND_MOVE LEFT
    }
    for (y <- 1 to height) {
      ascii_obj COND_MOVE UP
    }
  }

  //  ASCII SELECT_RECT (10, 10) MOVE DOWN 
  //  ASCII SELECT_RECT (10, 10) FILL
  //  ASCII SELECT_REGION MOVE RIGHT THEN UP
  //  Making SELECT_REGION not do rectangles would be pretty hard.
  //  if in selection

  class RectSelection(width: Int, height: Int) {
    def MOVE(dir: Direction): Unit = {
      val (cx, cy) = cursor

      dir match {
        case UP => {
          cursor = (cursor._1 - 1, cursor._2)
          // move_delta = (move_delta._1 - 1, move_delta._2)
        }
        case DOWN => {
          cursor = (cursor._1 + 1, cursor._2)
          // move_delta = (move_delta._1 + 1, move_delta._2)
        }
        case LEFT  =>{
          cursor = (cursor._1, cursor._2 - 1)
          // move_delta = (move_delta._1, move_delta._2 - 1)
        }
        case RIGHT => {
          for (yd <- -1 to height - 1) {
            var last_char = '0'
            for (xd <- -1 to width - 1) {
              val currx = cx + xd
              val curry = cy + yd
              println((currx, curry, last_char))
              if (in_bounds((curry, currx + 1))) {
                last_char = grid(curry)(currx + 1)
                grid(curry)(currx + 1) = last_char
              } else {
                println("Moving selection out of bounds")
                //  could undo here
                return
              }
            }
          }
          //  clear left column
          /*for (yd <- 1 to height) {
            grid(cy + yd)(cx) = '0'
          }*/
          // move_delta = (move_delta._1, move_delta._2 + 1)
        }
      }
    }
  }

  def SELECT_RECT(size: (Int, Int)): RectSelection = {
    val (w, h) = size
    return new RectSelection(w, h)
  }

  def FILL(replace_with: Char) = {
    var visited = Set[(Int, Int)]()
    val (starty, startx) = cursor
    var replace_char = grid(starty)(startx)
    def fill_recurse(y: Int, x: Int): Unit = {
      if (!in_bounds((y, x)) || visited.contains((y, x))) {
        return
      }

      visited += ((y, x))
      if (grid(y)(x) != replace_char) {
        return
      }
      grid(y)(x) = replace_with

      for (delta <- List((1, 0), (0, 1), (-1, 0), (0, -1))) {
        val (yd, xd) = delta
        fill_recurse(y + yd, x + xd)
      }
    }
    fill_recurse(starty, startx)
  }

    class CondExecute(dType: DoType) extends OBJ { cond_execute_obj =>
    var cond_array: Array[(String, Direction)] = new Array[(String, Direction)](0) 
    // all functions must be types of conditionals
    override def EXISTS(dir: Direction): CondExecute = {
      cond_array = cond_array :+ ("", dir)
      cond_execute_obj
    }
   
   //TODO handle other dTypes not just 'EXISTS'
    override def AND(dir: Direction): CondExecute = {
      cond_array = cond_array :+ ("and", dir)
      cond_execute_obj
    }

    override def OR(dir: Direction): CondExecute = {
      cond_array = cond_array :+ ("or", dir)
      cond_execute_obj
    }

    override def END() = {
      println("in the end")
      var old_move_path = move_path
      def does_exist(dir: Direction): Boolean = {
        println(cursor._1)
        dir match {
          case UP => {
            return cursor._1 > 0
          }
          case DOWN => {
            return cursor._1 < height - 1
          }
          case LEFT  =>{
            return cursor._2 > 0
          }
          case RIGHT => {
            return cursor._2 < width - 1
          }
        }
      }

      def cond_success() : Boolean = {
        //TODO figure out how to handle logic of OR
        var success = true
        for (cond_dir <- cond_array) {
          if (!does_exist(cond_dir._2)) {
            success = false
          }
        }
        success
      }

      if (dType == WHILE){
        while (cond_success()) {
          println(old_move_path.length)
          for(step <- old_move_path){
            ascii_obj COND_MOVE step
          }
        }
      } else if (dType == IF) {
        if (cond_success()) {
         for(step <- old_move_path){
           ascii_obj COND_MOVE step
         }
        }
      } else {
        println("Not a valid DO command.")
      }
    move_path = Array()
    }
  }

  override def DO(dType: DoType) = {
    // reset for conditional
    reset_grid
    cursor = last_cursor
    character = last_character
    new CondExecute(dType)
  }

  override def DO(times: Int) = {
    // reset for conditional
    reset_grid
    cursor = last_cursor
    character = last_character
    var old_move_path = move_path
    for( _ <- 1 to times) {
      for(step <- old_move_path){
          ascii_obj MOVE step
        }
    }
    move_path = Array()
    SWALLOW
  }

  override def THEN(dir: Direction): OBJ = {
    if (return_early) {
      return SWALLOW
    } else {
      COND_MOVE(dir)
      return this
    }
  }

  def COND_MOVE(dir: Direction) = {
    val old_cursor = cursor
    move_path = move_path :+ dir
    dir match {
      case UP => {
        cursor = (cursor._1 - 1, cursor._2)
        // move_delta = (move_delta._1 - 1, move_delta._2)
      }
      case DOWN => {
        cursor = (cursor._1 + 1, cursor._2)
        // move_delta = (move_delta._1 + 1, move_delta._2)
      }
      case LEFT  =>{
        cursor = (cursor._1, cursor._2 - 1)
        // move_delta = (move_delta._1, move_delta._2 - 1)
      }
      case RIGHT => {
        cursor = (cursor._1, cursor._2 + 1)
        // move_delta = (move_delta._1, move_delta._2 + 1)
      }
    }

    if (!in_bounds(cursor)) {
      println("Moved out of bounds.")
      cursor = old_cursor
      return_early = true
    } else {
      if (isDrawing) {
        grid(old_cursor._1)(old_cursor._2) = character
      }
      return_early = false
      println(cursor)
    }
    this
  }

  def in_bounds(cursor: (Int, Int)): Boolean = {
    val (y, x) = cursor
    return (y >= 0 && x >= 0 && y < height && x < width)
  }

  def reset_grid = {
    for (y <- 0 to height - 1) {
      for (x <- 0 to width - 1) {
        grid(y)(x) = last_grid(y)(x)
      }
    }
  }

  def reset_last_grid = {
    for (y <- 0 to height - 1) {
      for (x <- 0 to width - 1) {
        last_grid(y)(x) = grid(y)(x)
      }
    }
  }
}
