package no.kodeworks.septree


case class Line(p0: Point, p1: Point) {
  def a = p0.y - p1.y

  def b = p1.x - p0.x

  def c = p0.x * p1.y - p1.x * p0.y

  def intersects(l: Line): Boolean =
    Line.intersects(p0.x, p0.y, p1.x, p1.y, l.p0.x, l.p0.y, l.p1.x - l.p0.x, l.p1.y - l.p0.y)
}

object Line {
  // caller expected to calc line x y for last line:
  //      val sx = dx - cx
  //      val sy = dy - cy
  def intersects(ax: Double, ay: Double, bx: Double, by: Double, cx: Double, cy: Double, sx: Double, sy: Double): Boolean = {
    val cmpx = cx - ax
    val cmpy = cy - ay
    val rx = bx - ax
    val ry = by - ay
    val cmpXr = cmpx * ry - cmpy * rx
    if (cmpXr == 0d) {
      // Lines are collinear, and so intersect if they have any overlap
      ((cmpx < 0d) != (cx - bx < 0d)) ||
        ((cmpy < 0d) != (cy - by < 0d))
    } else {

      val rXs = rx * sy - ry * sx
      if (rXs == 0d) {
        // Lines are parallel
        false
      } else {
        val cmpXs = cmpx * sy - cmpy * sx
        val rXsr = 1d / rXs
        val t = cmpXs * rXsr
        val u = cmpXr * rXsr
        0d <= t && t <= 1d && 0d <= u && u <= 1d
      }
    }
  }
}