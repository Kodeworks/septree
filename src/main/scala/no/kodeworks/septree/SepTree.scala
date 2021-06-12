package no.kodeworks.septree

import SepTree._

case class SepTree(
                    space: Space,
                    depth: Int
                  ) {
  assume(0 < depth, "depth must be positive")

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
    hex.indexPoint(p).get
  }
}

case class SepHex(
                   levelInfo: SepLevelInfo,
                   levelInfos: Array[SepLevelInfo],
                   center: Point,
                   index: Int = 7
                 ) {

  import levelInfo._

  private[this] var corners0: Array[Point] = _
  private[this] var subHexes0: Array[SepHex] = _

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

  //first make it work, then make it optimal.
  def indexPoint(p: Point): Option[SepIndex] = {
    if (depth == level) {
      if (exactlyInsideThisLevel(p))
        Some(SepIndex(index))
      else None
    } else {
      val List((s0, d0), (s1, d1), (s2, d2)) = shortestThreeDistanceSquareds(p)
      val subLevel = levelInfos(level)
      val surelyInsideHex =
        if (d0 < subLevel.surelyInsideSquared) s0
        else if (d1 < subLevel.surelyInsideSquared) s1
        else if (d2 < subLevel.surelyInsideSquared) s2
        else null

      if (null != surelyInsideHex) {
        Some(SepIndex(index :: surelyInsideHex.indexPoint(p).get.keys))
      } else {
        indexUnlessSurelyOutside(s0, p, d0, subLevel.surelyOutsideSquared)
          .orElse(indexUnlessSurelyOutside(s1, p, d1, subLevel.surelyOutsideSquared))
          .orElse(indexUnlessSurelyOutside(s2, p, d2, subLevel.surelyOutsideSquared))
      }
    }
  }

  def indexUnlessSurelyOutside(hex: SepHex, p: Point, distanceSquared: Double, surelyOutsideSquared: Double) =
    if (distanceSquared < surelyOutsideSquared) {
      hex.indexPoint(p)
    } else None

  //TODO distances can be severely optimized.
  def shortestThreeDistanceSquareds(p: Point): List[(SepHex, Double)] = {
    val distanceSquareds = subHexes.map { sh =>
      val x = p.x - sh.center.x
      val y = p.y - sh.center.y
      println(s"p - c = $p - ${sh.center}, d = ${math.sqrt(x * x + y * y)}, d2 = ${x * x + y * y}")
      (sh, x * x + y * y)
    }
    distanceSquareds.sortInPlaceBy(_._2).take(3).toList
  }

  def exactlyInsideThisLevel(p: Point): Boolean = {
    //move to origo
    val tx = p.x - center.x
    val ty = p.y - center.y
    //guaranteed inside outside test
    val t2 = tx * tx + ty * ty
    if (t2 < r * r) true
    else if (R * R < t2) false
    else {
      //rotate back to baseline
      val rx = tx * rotationCosine + ty * rotationSine
      val ry = -tx * rotationSine + ty * rotationCosine
      //scale to unit
      val sx = rx / R
      val sy = ry / R
      //stretch y and check
      val dy = sy * twoDivSqrt3
      if (1d < dy || dy < -1d) false
      else {
        //skew x and check
        val dx =.5d * dy + sx
        if (1d < dx || dx < -1d) false
        else {
          val d = dy - dx
          if (1d < d || d < -1d) false
          else true
        }
      }
    }
  }
}

case class SepLevelInfo(
                         level: Int,
                         R: Double,
                         r: Double,
                         s: Double,
                         rotation: Double,
                         rotationCosine: Double,
                         rotationSine: Double,
                         surelyInsideSquared: Double,
                         surelyOutsideSquared: Double
                       )

case class Space(
                  lowerLeft: Point,
                  upperRight: Point
                ) {
  assume(lowerLeft.x < upperRight.x && lowerLeft.y < upperRight.y, "Space must be unwarped and nonempty")
}

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
  val twoDivSqrt3 = 2d / math.sqrt(3d)
  val sqrt7 = math.sqrt(7d) // factor of R for each level
  val acos5div2sqrt7 = math.acos(5d / (2d * sqrt7))
  val piDiv6 = math.Pi / 6d
  val todoDeprecated = piDiv6 - acos5div2sqrt7
  val piDiv3 = math.Pi / 3d // 60 deg in rads, angle from hex 5 to hex 2 etc
  val sinPiDiv3 = math.sin(piDiv3)
  val surelyInside = 0.8
  val surelyOutside = 1.2
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

  def levelInfos(R: Double, depth: Int): Array[SepLevelInfo] =
    (1 to depth).map { level =>
      val levelD = level.toDouble
      val subR = R * math.pow(1d / sqrt7, levelD - 1d)
      val subr = calcr(subR)
      val subs = 2d * subr
      val subRot = (levelD - 1d) * acos5div2sqrt7
      val subRotCos = math.cos(subRot)
      val subRotSin = math.sin(subRot)
      val subSurelyInside = surelyInside * subR
      val subSurelyOutside = surelyOutside * subR
      SepLevelInfo(level, subR, subr, subs, subRot, subRotCos, subRotSin, subSurelyInside * subSurelyInside, subSurelyOutside * subSurelyOutside)
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

  // returns true if point is left of line. returns false otherwise
  def leftOfLine(p: Point, a: Point, b: Point) =
    0 < (b.x - a.x) * (p.y - a.y) - (b.y - a.y) * (p.x - a.x)

  //todo investigate shear mapped rotated and scaled point instead
  //  def insideHex(p: Point, corners: Array[Point]) = {
  //    val Array(a, b, c, d, e, f) = corners
  //    val ad = leftOfLine(p, a, d)
  //    val be = leftOfLine(p, b, e)
  //    val cf = leftOfLine(p, c, f)
  //    if(ad && be && cf && leftOfLine)
  //  }
}
