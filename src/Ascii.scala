// for disabling compiler warnings about postfixes
import scala.language.postfixOps

import scala.io.Source

// class Ascii {



//   object ASCII {

//     def hello = "Hello World"
//     def goodbye = "Goodbye, Cruel World"

//     def set() = {

//     }

//     def unset() = {

//     }

//   }


// }
abstract sealed class Direction(magnitude: Int = 1) {

}
object LEFT extends Direction
object RIGHT extends Direction
object UP extends Direction
object DOWN extends Direction

class TEST {
  def THEN(): TEST = {
    this
  }
  def THEN(dir: Direction): TEST = {
    this
  }
}

object SWALLOW extends TEST {

}

object ASCII extends TEST {

  // def move(dir: Direction) = {
  //   println(dir)
  //   this
  // }

  // def LEFT() = {
  //   println("LEFT")
  //   this
  // }

  // def THEN() = {
  //   println("THEN")
  //   this
  // }

  var width = 25
  var height = 15
  // var grid = Array.ofDim[Char](height, width)
  var grid = Array.fill[Char](height, width) { '0' }
  var cursor = (0,0)

  var availableCharacters = Array('@', '|', '+', '-', '0')
  var character = '@' // @, |, +, -, 0
  var isDrawing = false
  var return_early = false


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

  def EXISTS(dir: Direction): Boolean = {
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
    return false
  }



  override def THEN(dir: Direction): TEST = {
    if (return_early) {
      return SWALLOW
    } else {
      MOVE(dir)
      return this
    }
    // this
  }


  // def move(dir: Direction) = (mag: Int = 1) => {
  //   prefix + " " + s
  // }

  def MOVE(dir: Direction) = {
    val old_cursor = cursor
    dir match {
      case UP => {
        cursor = (cursor._1 - 1, cursor._2)
      }
      case DOWN => {
        cursor = (cursor._1 + 1, cursor._2)
      }
      case LEFT  =>{
        cursor = (cursor._1, cursor._2 - 1)
      }
      case RIGHT => {
        cursor = (cursor._1, cursor._2 + 1)
      }
    }

    if (cursor._1 < 0 || cursor._2 < 0 || cursor._1 >= height || cursor._2 >= width) {
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

}
