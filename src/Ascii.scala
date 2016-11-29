// for disabling compiler warnings about postfixes
import scala.language.postfixOps
import scala.collection.mutable.Set

import scala.io.Source
//import Console.{GREEN, RED, YELLOW, MAGENTA, BLACK, CYAN, WHITE}

abstract sealed class Direction() {}
object LEFT extends Direction
object RIGHT extends Direction
object UP extends Direction
object DOWN extends Direction

abstract sealed class Rotation {}
object NINETY extends Rotation
object HUNDRED_EIGHTY extends Rotation
object TWO_HUNDRED_SEVENTY extends Rotation

abstract sealed class DoType{}
object WHILE extends DoType
object IF extends DoType

val BLUE = Console.BLUE
val GREEN = Console.GREEN
val RED = Console.RED
val BLACK = Console.BLACK
val CYAN = Console.CYAN
val WHITE = Console.WHITE
val DEFAULT = Console.RESET

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

  var grid = Array.fill[(Char, String)](height, width) { ('0', DEFAULT) }
  var cursor = (0,0)
  var character = '@'
  var last_grid = Array.fill[(Char, String)](height, width) { ('0', DEFAULT) }
  var last_cursor = (0,0)
  var last_character = '@'

  var move_path: Array[Direction] = new Array[Direction](0)

  var move_delta = (0, 0)

  // val valid_colors = Set(GREEN, RED, YELLOW, MAGENTA, BLACK, CYAN, WHITE, BLUE)
  val valid_colors = Set(GREEN, RED, BLACK, CYAN, WHITE, BLUE)
  var curr_color = DEFAULT

  def RESET() = {
    CLEAR()
    RESET_CURSOR()
  }

  def CLEAR() = {
    grid = Array.fill[(Char, String)](height, width) { ('0', DEFAULT) }
  }

  // commands
  def WIDTH(w: Int) = {
    if (w <= 0) {
      println("Sorry, width must be greater than 0.")
    } else {
      width = w
      CLEAR()
    }
  }

  def HEIGHT(h: Int) = {
    if (h <= 0) {
      println("Sorry, height must be greater than 0.")
    } else {
      height = h
      CLEAR()
    }

  }

  def RENDER() = {
    var current = (0,0)
    for (column <- grid) {
      for ((char, color) <- column) {
        if (current == cursor) {
          print('*');
        }
        else {
          print(color)
          print(color + char)
        }
        current = (current._1, current._2 + 1)
      }
      current = (current._1 + 1, 0)
      print("\n")
    }
  }

  def SET() = {
    isDrawing = true
  }

  def CHANGE_COLOR(col: String) = {
    if (valid_colors(col)) {
      curr_color = col
    } else {
      println("Not a valid color.")
    }
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

  def TEST_SELECT() = {
   JUMP (5, 5)
   RECT (5, 5)
   SELECT_RECT (5, 5) MOVE RIGHT
   RENDER

   print('\n')
   SELECT_RECT (5, 5) MOVE RIGHT
   RENDER

   print('\n')
   SELECT_RECT (5, 5) MOVE DOWN
   RENDER
  }

  def MOVE(dir: Direction) = {

    reset_last_grid
    last_cursor = cursor
    last_character = character

    COND_MOVE(dir)

    this
  }

  def RESET_CURSOR() {
    JUMP(0, 0)
  }

  def JUMP(new_pos: (Int, Int)) = {
    if (!in_bounds(new_pos)) {
      println("Jumped out of bounds.")

    } else {
      cursor = new_pos
    }
    this
  }

  def ROTATE(r: Rotation) {
    var grid_copy = Array.fill[(Char, String)](height, width) { ('0', curr_color) }
    val max_height = height - 1
    val max_width = width - 1
    for (y <- 0 to max_height) {
      for (x <- 0 to max_width) {
        grid_copy(y)(x) = grid(y)(x)
      }
    }

    r match {
      case NINETY => {
        val old_width = width
        width = height
        height = old_width
        grid = Array.fill[(Char, String)](height, width) { ('0', curr_color) }
        var column = 0
        for (column_ <- grid_copy) {
          var row = 0
          for (element <- column_) {
            grid(row)(max_height - column) = element
            row = row + 1
          }
          column = column + 1
        }
      }
      case HUNDRED_EIGHTY => {
        grid = Array.fill[(Char, String)](height, width) { ('0', curr_color) }
        var column = 0
        for (column_ <- grid_copy) {
          var row = 0
          for (element <- column_) {
            grid(max_height - column)(max_width - row) = element
            row = row + 1
          }
          column = column + 1

        }
      }
      case TWO_HUNDRED_SEVENTY => {
        val old_width = width
        width = height
        height = old_width
        grid = Array.fill[(Char, String)](height, width) { ('0', curr_color) }
        var column = 0
        for (column_ <- grid_copy) {
          var row = 0
          for (element <- column_) {
            grid(max_width - row)(column) = element
            row = row + 1
          }
          column = column + 1

        }
      }
    }
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
      var last_char = ('0', DEFAULT)
      val (cy, cx) = cursor

      def shift_char(currx: Int, curry: Int, dx: Int, dy: Int): Boolean = {
        if (in_bounds((curry + dy, currx + dx))) {
          val curr_char = grid(curry + dy)(currx + dx)
          grid(curry + dy)(currx + dx) = last_char
          last_char = curr_char
          return true
        } else {
          println("Moving selection out of bounds")
          //  could undo here
          return false
        }
      }

      dir match {
        case UP => {
          for (xd <- 0 to width) {
            for (yd <- 0 to height + 1) {
              val currx = cx + xd
              val curry = cy + (height + 1 - yd)

              //  If shift_char is false, we've moved out of bounds
              if (!shift_char(currx, curry, 0, -1)){
                return
              }
            }
          }
          cursor = (cy - 1, cx)
        }
        case DOWN => {
          for (xd <- 0 to width) {
            for (yd <- -1 to height) {
              val currx = cx + xd
              val curry = cy + yd

              //  If shift_char is false, we've moved out of bounds
              if (!shift_char(currx, curry, 0, 1)){
                return
              }
            }
          }
          cursor = (cy + 1, cx)
        }
        case LEFT => {
          for (yd <- 0 to height) {
            for (xd <- 0 to width + 1) {
              val currx = cx + (width + 1 - xd)
              val curry = cy + yd

              //  If shift_char is false, we've moved out of bounds
              if (!shift_char(currx, curry, -1, 0)){
                return
              }
            }
          }
          cursor = (cy, cx - 1)
        }
        case RIGHT => {
          for (yd <- 0 to height) {
            for (xd <- -1 to width) {
              val currx = cx + xd
              val curry = cy + yd

              if (!shift_char(currx, curry, 1, 0)){
                return
              }
            }
          }
          cursor = (cy, cx + 1)
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
      grid(y)(x) = (replace_with, curr_color)

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
        var success = true
        for (cond_dir <- cond_array) {
          if (cond_dir._1 == "and") {
            success = success && does_exist(cond_dir._2)
          }
          else if (cond_dir._1 == "or") {
            success = success || does_exist(cond_dir._2)
          }
          else {
            success = does_exist(cond_dir._2)
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
        grid(old_cursor._1)(old_cursor._2) = (character, curr_color)
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
