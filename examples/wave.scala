import scala.math
:load ../src/ASCII.scala


ASCII HEIGHT 30
ASCII WIDTH 90
ASCII CHANGE_COLOR CYAN

val PI = 3.14159265358979323846264338327
val dataCount = 79.0
val shift = 10
var lastY = 0
for (i <- 0 to dataCount.toInt) {

  val interp = i/dataCount
  val y = (Math.round((10 * Math.sin(interp * (4 * PI) - (2 * PI))) + 10)).toInt
  val delta = y - lastY
  lastY = y

  if (i == 0) {
    ASCII UNSET
  } else {
    ASCII SET
  }
  ASCII MOVE RIGHT
  if (delta > 0) {
    ASCII MOVE DOWN DO delta TIMES
  } else if (delta < 0) {
    ASCII MOVE UP DO Math.abs(delta) TIMES
  }
}

ASCII RENDER
