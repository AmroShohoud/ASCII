import scala.math
:load ../src/ASCII.scala


ASCII HEIGHT 40
ASCII WIDTH 80
ASCII CHANGE_COLOR CYAN

val PI = 3.14159265358979323846264338327
val dataCount = 79.0
val shift = 10
var lastY = 0
for (i <- 0 to dataCount.toInt) {
  ASCII MOVE RIGHT
  val interp = i/dataCount
  print("x: ")
  // print(interp * (4 * PI) - (2 * PI))
  print(i)
  print(" y: ")
  val y = (Math.round((10 * Math.sin(interp * (4 * PI) - (2 * PI))) + 10)).toInt
  val delta = y - lastY
  lastY = y
  println(delta)
  if (delta > 0) {
    ASCII MOVE DOWN DO delta TIMES
  } else if (delta < 0) {
    ASCII MOVE UP DO Math.abs(delta) TIMES
  }
}

ASCII RENDER
