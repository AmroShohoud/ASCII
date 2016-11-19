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


object ASCII {

  def move(dir: Direction) = {
    println(dir)
    this
  }

  def LEFT() = {
    println("LEFT")
    this
  }

  def THEN() = {
    println("THEN")
    this
  }

  var width = 25
  var height = 15
  var grid = Array.ofDim[Int](height, width)
  var cursor = (0,0)

  var availableCharacters = Array("@", "|", "+", "-", "0")
  var character = "@" // @, |, +, -, 0
  var isDrawing = false


  def WIDTH(w: Int) = {
    if (w <= 0) {
      println("Sorry, width must be greater than 0.")
    } else {
      width = w
      grid = Array.ofDim[Int](height, width)
    }

  }

  def HEIGHT(h: Int) = {
    if (h <= 0) {
      println("Sorry, height must be greater than 0.")
    } else {
      height = h
      grid = Array.ofDim[Int](height, width)
    }

  }

  def render() = {
    for (column <- grid) {
      for (element <- column) {
        print(element)
      }
      print("\n")
    }
  }

  def set() = {
    isDrawing = true
  }

  def unset() = {
    isDrawing = false
  }

  // def move(dir: Direction) = (mag: Int = 1) => {
  //   prefix + " " + s
  // }

  // def move(dir: Direction): (Int) => Void {
  //   val old_cursor = cursor
  //   dir match {
  //     case LEFT => {
  //       def move_left(mag:Int = 1) = {
  //         cursor = (cursor._1 - mag, cursor._2)
  //         println(cursor)
  //       }
  //       return move_left
  //     }
  //     // case RIGHT => {
  //     //   cursor = (cursor._1 + 1, cursor._2)
  //     // }
  //     // case UP =>{
  //     //   cursor = (cursor._1, cursor._2 - 1)
  //     // }
  //     // case DOWN => {
  //     //   cursor = (cursor._1, cursor._2 + 1)
  //     // }
  //   }
  //   // if (cursor._1 < 0 || cursor._2 < 0) {
  //   //   println("Moved out of bounds.")
  //   //   cursor = old_cursor
  //   // }
  //   // println(cursor)

  //   def test(mag:Int = 1) = {}
  //   return test
  // }

}