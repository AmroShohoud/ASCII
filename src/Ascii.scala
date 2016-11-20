// for disabling compiler warnings about postfixes
import scala.language.postfixOps

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

  def EXISTS(dir: Direction) = {}
}

object SWALLOW extends OBJ {}

object ASCII extends OBJ { ascii_obj =>

  var width = 25
  var height = 15
  // var grid = Array.ofDim[Char](height, width)
  var grid = Array.fill[Char](height, width) { '0' }
  var cursor = (0,0)

  var availableCharacters = Array('@', '|', '+', '-', '0')
  var character = '@' // @, |, +, -, 0
  var isDrawing = true
  var return_early = false

  var move_path: Array[Direction] = new Array[Direction](0)

  var move_delta = (0, 0)


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

  def RECT(size: (Int, Int)) = {
    val (width, height) = size
    for (x <- 1 to width) {
      ascii_obj MOVE RIGHT
    }
    for (y <- 1 to height) {
      ascii_obj MOVE DOWN
    }
    for (x <- 1 to width) {
      ascii_obj MOVE LEFT
    }
    for (y <- 1 to height) {
      ascii_obj MOVE UP
    }
  }


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
          println(old_move_path.length)
          for(step <- old_move_path){
            ascii_obj MOVE step
          }
        }
      } else if (dType == IF) {
        if (does_exist()) {
          for(step <- old_move_path){
            ascii_obj MOVE step
          }
        }
      } else {
        println("Not a valid DO command.")
      }
      move_path = Array()
    }
  }

  override def DO(dType: DoType) = {
    // reset cursor for conditional
    cursor = (cursor._1 - move_delta._1, cursor._2 - move_delta._2)
    val old_move_delta = move_delta
    move_delta = (0,0)
    new CondExecute(old_move_delta, dType)
  }

  override def THEN(dir: Direction): OBJ = {
    if (return_early) {
      return SWALLOW
    } else {
      MOVE(dir)
      return this
    }
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

  def MOVE(dir: Direction) = {
    val old_cursor = cursor
    move_path = move_path :+ dir
    dir match {
      case UP => {
        cursor = (cursor._1 - 1, cursor._2)
        move_delta = (move_delta._1 - 1, move_delta._2)
      }
      case DOWN => {
        cursor = (cursor._1 + 1, cursor._2)
        move_delta = (move_delta._1 + 1, move_delta._2)
      }
      case LEFT  =>{
        cursor = (cursor._1, cursor._2 - 1)
        move_delta = (move_delta._1, move_delta._2 - 1)
      }
      case RIGHT => {
        cursor = (cursor._1, cursor._2 + 1)
        move_delta = (move_delta._1, move_delta._2 + 1)
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
