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

  def EXISTS(dir: Direction) = {}

  def TIMES() = {}
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
    last_grid = grid
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



  // conditionals, loops, complicated shit
  class CondExecute(delta: (Int, Int), dType: DoType) extends OBJ {
    // all functions must be types of conditionals
    override def EXISTS(dir: Direction) = {
      var old_move_path = move_path
      def does_exist(): Boolean = {
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

      if (dType == WHILE){
        while (does_exist()) {
          for(step <- old_move_path){
            ascii_obj COND_MOVE step
          }
        }
      } else if (dType == IF) {
        if (does_exist()) {
          for(step <- old_move_path){
            ascii_obj COND_MOVE step
          }
        }
      } else {
        println("Not a valid DO command.")
      }
      move_path = Array()
    }

    def FALSE() = {
      cursor = (cursor._1 - delta._1, cursor._2 - delta._2)
    }
  }

  override def DO(dType: DoType) = {
    val old_delta = move_delta
    move_delta = (0,0)
    for (column <- last_grid) {
      for (element <- column) {
        print(element)
      }
      print("\n")
    }
    print("\n")
    print("\n")
    print("\n")

    grid = last_grid
    cursor = last_cursor
    character = last_character


    for (column <- grid) {
      for (element <- column) {
        print(element)
      }
      print("\n")
    }
    new CondExecute(old_delta, dType)
  }

  override def DO(times: Int) = {
    // reset cursor for conditional
    // cursor = (cursor._1 - move_delta._1, cursor._2 - move_delta._2)
    // move_delta = (0,0)
    grid = last_grid
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
      MOVE(dir)
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

}
