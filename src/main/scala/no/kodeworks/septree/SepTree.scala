package no.kodeworks.septree

import SepTree._

case class SepTree(
                    space: Space,
                    depth: Int
                  ) {
  def hex: SepHex = {
    val R = calcR(space)
    val c = calcCenter(space)
    val lis = levelInfos(R, depth)
    SepHex(lis.head, lis, c)
  }

  def indexPoint(p: Point): SepIndex = {
    assume(space.lowerLeft.x <= p.x &&
      space.lowerLeft.y <= p.y &&
      p.x <= space.upperRight.x &&
      p.y <= space.upperRight.y, "point must be contained in space")
    hex.indexPoint(p)
  }
}

case class SepHex(
                   levelInfo: SepLevelInfo,
                   levelInfos: Array[SepLevelInfo],
                   center: Point,
                   index: Int = 7
                 ) {

  import levelInfo._

  var corners0: Array[Point] = _
  var subHexes0: Array[SepHex] = _

  def depth: Int = levelInfos.size

  override def toString: String =
    s"SepHex($R,$center,$rotation,$level,${corners.toList})"

  def corners: Array[Point] = {
    if (null == corners0)
      corners0 = rotations.map { r =>
        val rot = rotation + r
        Point(center.x + math.cos(rot) * R, center.y + math.sin(rot) * R)
      }
    corners0
  }

  def subHexes: Array[SepHex] = {
    if (null == subHexes0)
      subHexes0 = {
        val li = levelInfos(level)
        var subIndex = 0
        centers.map { calcCenter =>
          val subCenter = calcCenter(center, li.s, li.rotation - piDiv6)
          subIndex += 1
          SepHex(li, levelInfos, subCenter, subIndex)
        } ++ Array(SepHex(li, levelInfos, center))
      }
    subHexes0
  }

  def toList(depth: Int): List[SepHex] =
    this :: (
      if (level < depth) subHexes.toList.flatMap(_.toList(depth)).sortBy(_.levelInfo.level)
      else Nil)

  def select(sel: SepSelector): List[SepHex] =
    if (index == sel.index) {
      var i = -1
      (this :: subHexes.toList.flatMap { sh =>
        i += 1
        sh.select(sel.subSelectors(i))
      }).sortBy(_.levelInfo.level)
    } else Nil

  def indexPoint(p: Point): SepIndex = {
    if(level == depth) {
      //TODO think
    }
    val List((s0, d0), (s1, d1), (s2, d2)) = shortestThreeDistanceSquareds(p)
    val surelyInside2Subhex = surelyInside2 * R / sqrt7
    val surelyOutside2Subhex = surelyOutside2 * R / sqrt7
    if (d0 < surelyInside2Subhex) {
      SepIndex(s0.index :: s0.indexPoint(p).keys)
    } else if (d1 < surelyInside2Subhex) {
      SepIndex(s1.index :: s1.indexPoint(p).keys)
    } else if (d2 < surelyInside2Subhex) {
      SepIndex(s1.index :: s1.indexPoint(p).keys)
    }
    else throw new RuntimeException("not impl, not surely inside")
  }

  //TODO distances can be severely optimized.
  def shortestThreeDistanceSquareds(p: Point): List[(SepHex, Double)] = {
    val distanceSquareds = subHexes.map { sh =>
      val x = p.x - sh.center.x
      val y = p.y - sh.center.y
      (sh, x * x + y * y)
    }
    distanceSquareds.sortInPlaceBy(_._2).take(3).toList
  }
}

case class SepLevelInfo(
                         level: Int,
                         R: Double,
                         s: Double,
                         rotation: Double
                       )

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
 * c1 - c7 = center points of subhexes 1 - 7, around the clock from top left, then center
 * baseline = lower horizontal line of hex
 * rot = rotation in radians, compared to when the baseline lies flat on ground
 * corners = viewed from baseline: upper left, upper right, mid right, lower right, lower left, mid left
 */
object SepTree {
  val sqrt7 = math.sqrt(7d) // factor of R for each level
  val acos5div2sqrt7 = math.acos(5d / (2d * sqrt7))
  val piDiv6 = math.Pi / 6d
  val todoDeprecated = piDiv6 - acos5div2sqrt7
  val piDiv3 = math.Pi / 3d // 60 deg in rads, angle from hex 5 to hex 2 etc
  val sinPiDiv3 = math.sin(piDiv3)
  val surelyInside = 0.8
  val surelyInside2 = surelyInside * surelyInside
  val surelyOutside = 1.2
  val surelyOutside2 = surelyOutside * surelyOutside
  // Rotations to the center of each subhex.
  // rot1 = rots(0) etc
  // these are base rotations without the adjustment for 1 level deeper.
  val rotations = Array(
    2d * piDiv3,
    piDiv3,
    0d,
    5d * piDiv3,
    4d * piDiv3,
    3d * piDiv3
  )
  val centers: Array[(Point, Double, Double) => Point] = rotations.map { r =>
    (p: Point, subS: Double, rotLevel: Double) =>
      val rot = rotLevel + r
      Point(p.x + math.cos(rot) * subS, p.y + math.sin(rot) * subS)
  }

  def levelInfos(R: Double, depth: Int) =
    (1 to depth).map { level =>
      val levelD = level.toDouble
      val subR = R * math.pow(1d / sqrt7, levelD - 1d)
      val subs = 2d * calcr(subR)
      val subRot = (levelD - 1d) * acos5div2sqrt7
      SepLevelInfo(level, subR, subs, subRot)
    }.toArray

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
    calcRotation(level) - acos5div2sqrt7

  def calcRotation(level: Int) =
    (level - 1) * todoDeprecated

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
    val Point(c1x, c1y) = centers(subNumber - 1)(Point(0d, 0d), subS, calcCenterRotation(level))
    val d1x = c1x - px
    val d1y = c1y - py
    ((c1x, c1y), d1x * d1x + d1y * d1y)
  }

  def normalizePlusMinus1(x: Double, min: Double, max: Double): Double =
    2d * (x - min) / (max - min) - 1d

  def denormalizePlusMinus1(x: Double, min: Double, max: Double): Double =
    (max - min) * (x + 1d) / 2d + min
}
