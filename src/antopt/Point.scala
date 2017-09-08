package antopt

import scala.math._

case class Point(val x: Int, val y: Int) {
  def distance(p: Point): Long = {
    sqrt(pow(p.x - x, 2) + pow(p.y - y, 2)).round.toLong
  }
}
