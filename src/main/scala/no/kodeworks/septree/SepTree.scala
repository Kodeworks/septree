package no.kodeworks.septree

import SepTree._

case class SepTree(
                    space: Space,
                    depth: Int
                  ) {
  def hex(): SepHex = {
    val R = calcR(space)
    val r = calcr(R)
    val c = calcCenter(space)
    val corners = Array(
      Point(-.5 * R, r),
      Point(.5 * R, r),
      Point(R, 0d),
      Point(.5 * R, -r),
      Point(-.5 * R, -r),
      Point(-R, 0d)
    ).map(p => Point(c.x + p.x, c.y + p.y))
    SepHex(R, c, 0d, 1, corners)
  }
}

case class SepHex(
                   R: Double,
                   center: Point,
                   rotation: Double = 0d,
                   level: Int = 1,
                   corners: Array[Point],
                   children: Array[SepHex] = Array.empty
                 ) {
  override def toString: String =
    s"SepHex($R,$center,$rotation,$level,${corners.toList},${children.toList})"
}

case class Space(
                  lowerLeft: Point,
                  upperRight: Point
                )

case class Point(x: Double, y: Double)

/**
 * Subhexes are numbered from 1 to 7 and starts in upper left. Then upper right, left, mid, right, lower left, lower right.
 * R = big radius
 * r = little radius, R times sine 30 degrees
 * s = little diameter, two times r
 * c = center point
 * c1 - c7 = center points of subhexes 1 - 7
 * baseline = lower horizontal line of hex
 * rot = rotation in radians, compared to when the baseline lies flat on ground
 * corners = viewed from baseline: upper left, upper right, mid right, lower right, lower left, mid left
 */
object SepTree {
  val arctan2div5 = math.atan(2d / 5d)
  val piDiv6 = math.Pi / 6d
  val piDiv6SubArctan2div5 = piDiv6 - arctan2div5
  val sqrt7 = math.sqrt(7d) // factor of R for each level
  val piDiv3 = math.Pi / 3d // 60 deg in rads, angle from hex 5 to hex 2 etc
  val sinPiDiv3 = math.sin(piDiv3)
  // Rotations to the center of each subhex.
  // rot1 = rots(0) etc
  // these are base rotations without the adjustment for 1 level deeper.
  val rots = Array(
    2d * piDiv3,
    piDiv3,
    3d * piDiv3,
    0d,
    0d,
    4d * piDiv3,
    5d * piDiv3
  ) //.map(_ - piDiv6SubArctan2div5)

  val surelyInside = 0.8
  val surelyInside2 = surelyInside * surelyInside
  val surelyOutside = 1.2
  val surelyOutside2 = surelyOutside * surelyOutside

  val centers: Array[(Double, Double, Double, Int) => Array[Double]] = rots.map { r =>
    (px: Double, py: Double, subS: Double, level: Int) =>
      val rotLevel = calcCenterRotation(level) + r
      Array(px + math.cos(rotLevel) * subS, py + math.sin(rotLevel) * subS)
  }
  centers(3) = (px: Double, py: Double, _: Double, _: Int) => Array(px, py)

  /*
   * 'Space' is an area wrapped in an initial hex.
   * It is snapped to the center of the hex in case it does not fill the entire width or height.
   * Point is within this area.
   *       _________________                _________________
   *      /    (n)onspace   \              /   |         |   \
   *     /|_____(s)pace_____|\            /|   |         |   |\
   *    / |                 | \          / |  (s)       (s)  | \
   *   /  |                 |  \        /  |   |         |   |  \
   *  /   |  (p)oint        |   \      /   |   |         |   |   \
   * /    |                 |    \    /    |(n)|         |(n)|    \
   * \    |                 |    /    \    |   |         |   |    /
   *  \   |                 |   /      \   |   |         |   |   /
   *   \  |                 |  /        \  |   |         |   |  /
   *    \ |_____(s)pace_____| /          \ |   |         |   | /
   *     \|    (n)onspace   |/            \|   |     (p) |   |/
   *      \_________________/              \___|_________|___/
   *         Width limited                   Height limited
   *
   * The center of the space always aligns with the center of the initial wrapping hex.
   */

  def calcR(s: Space): Double = {
    val w = s.upperRight.x - s.lowerLeft.x
    val h = s.upperRight.y - s.lowerLeft.y
    val maxH = 2d * sinPiDiv3 * w
    if (maxH < h) {
      //height limited
      h / 2d / sinPiDiv3
    } else {
      //width limited
      w
    }
  }

  def calcr(R: Double): Double =
    sinPiDiv3 * R

  def calcCenter(s: Space) =
    Point(
      (s.lowerLeft.x + s.upperRight.x) / 2d,
      (s.lowerLeft.y + s.upperRight.y) / 2d
    )

  def calcCenterRotation(level: Int) =
    calcRotation(level) - arctan2div5

  def calcRotation(level: Int) =
    (level - 1) * piDiv6SubArctan2div5

  // s = 2*r = sin(pi/3)*d = sin(pi/3)*2*R
  // diminished by 1/sqrt(7) for each level
  def sByLevel(R: Double, level: Int) =
    sinPiDiv3 * 2d * R * math.pow(1d / sqrt7, level)

  def indexPoint(
                  point: Point,
                  space: Space = Space(Point(-1d, -1d), Point(1d, 1d)),
                  depth: Int = 1,
                  level: Int = 1
                ): SepIndex = {
    val px = point.x
    val py = point.y
    val sx0 = space.lowerLeft.x
    val sy0 = space.lowerLeft.y
    val sx1 = space.upperRight.x
    val sy1 = space.upperRight.y
    assume(sx0 <= px && px <= sx1 && sy0 <= py && py <= sy1, "point must be within space")
    assume(sx0 < sx1 && sy0 < sy1, "space must consist of both vertical and horizontal positive distance")
    assume(0 < depth, "depth must be positive")
    val R = calcR(space)
    println(s"px:$px, py:$py")
    if (level == depth) {
      SepIndex.depthOne
    } else {
      val Point(cx, cy) = calcCenter(space)
      val (_, d4) = centerAndDistance2(cx, cy, px, py, 4, level, 0d)
      val surelyInside2SubHex = surelyInside2 * R / sqrt7
      val surelyOutside2SubHex = surelyOutside2 * R / sqrt7
      if (d4 <= surelyInside2SubHex) {
        SepIndex(4 :: indexPoint(point, space, depth, level + 1).keys)
      } else {
        // (px: Double, py: Double, s: Double, level: Double)
        val subS = 2d * sinPiDiv3 * R / sqrt7
        val ((c1x, c1y), d1) = centerAndDistance2(cx, cy, px, py, 1, level, subS)
        if (d1 < surelyInside2SubHex) {
          SepIndex(1 :: indexPoint(point, space, depth, level + 1).keys)
        } else {
          val ((c2x, c2y), d2) = centerAndDistance2(cx, cy, px, py, 2, level, subS)
          if (d2 < surelyInside2SubHex) {
            SepIndex(2 :: indexPoint(point, space, depth, level + 1).keys)
          } else {
            val ((c3x, c3y), d3) = centerAndDistance2(cx, cy, px, py, 3, level, subS)
            if (d3 < surelyInside2SubHex) {
              SepIndex(3 :: indexPoint(point, space, depth, level + 1).keys)
            } else {
              val ((c5x, c5y), d5) = centerAndDistance2(cx, cy, px, py, 5, level, subS)
              if (d5 < surelyInside2SubHex) {
                SepIndex(5 :: indexPoint(point, space, depth, level + 1).keys)
              } else {
                val ((c6x, c6y), d6) = centerAndDistance2(cx, cy, px, py, 6, level, subS)
                if (d6 < surelyInside2SubHex) {
                  SepIndex(6 :: indexPoint(point, space, depth, level + 1).keys)
                } else {
                  val ((c7x, c7y), d7) = centerAndDistance2(cx, cy, px, py, 7, level, subS)
                  if (d7 < surelyInside2SubHex) {
                    SepIndex(7 :: indexPoint(point, space, depth, level + 1).keys)
                  } else {
                    SepIndex.depthOne
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  def centerAndDistance2(cx: Double, cy: Double, px: Double, py: Double, subNumber: Int, level: Int, subS: Double) = {
    val Array(c1x, c1y) = centers(subNumber - 1)(0d, 0d, subS, level)
    val d1x = c1x - px
    val d1y = c1y - py
    ((c1x, c1y), d1x * d1x + d1y * d1y)
  }

  def normalizePlusMinus1(x: Double, min: Double, max: Double): Double =
    2d * (x - min) / (max - min) - 1d

  def denormalizePlusMinus1(x: Double, min: Double, max: Double): Double =
    (max - min) * (x + 1d) / 2d + min
}
