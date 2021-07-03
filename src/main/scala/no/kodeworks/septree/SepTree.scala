package no.kodeworks.septree

import SepTree._

case class SepTree(
                    space: Space,
                    depth: Int
                  ) {
  assume(0 < depth, "depth must be positive")

  //todo cache hex?
  def hex: SepHex = {
    val R = calcR(space)
    val c = calcCenter(space)
    val lis = levelInfos(R, depth)
    SepHex(lis.head, lis, c)
  }

  def indexPoint(p: Point): SepIndex = {
    assumePointContainedInSpace(p)
    hex.indexPoint(p).get
  }

  def indexLine(l: Line): List[SepIndex] = {
    assumePointContainedInSpace(l.p0)
    assumePointContainedInSpace(l.p1)
    hex.indexLine(l)
  }

  def assumePointContainedInSpace(p: Point): Unit =
    assume(space.lowerLeft.x <= p.x &&
      space.lowerLeft.y <= p.y &&
      p.x <= space.upperRight.x &&
      p.y <= space.upperRight.y, "point must be contained in space")
}

case class SepHex(
                   levelInfo: SepLevelInfo,
                   levelInfos: Array[SepLevelInfo],
                   center: Point,
                   index: Int = 0
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
        if (level == depth) Array.empty
        else {
          val li = levelInfos(level)
          var subIndex = 0
          centers.map { calcCenter =>
            val subCenter = calcCenter(center, li.s, li.rotation - piDiv6)
            subIndex += 1
            SepHex(li, levelInfos, subCenter, subIndex)
          } ++ Array(SepHex(li, levelInfos, center, 7))
        }
      }
    subHexes0
  }

  def toList(depth: Int): List[SepHex] =
    this :: (
      if (level < depth) subHexes.toList.flatMap(_.toList(depth)).sortBy(_.levelInfo.level)
      else Nil)

  def select(sel: SepSelector): List[SepHex] =
    if (sel != SepSelector.empty &&
      (index == sel.index || 0 == index)) {
      var i = -1
      (this :: subHexes.toList.flatMap { sh =>
        i += 1
        sh.select(sel.subSelectors(i))
      }).sortBy(_.levelInfo.level)
    } else Nil

  //first make it work, then make it optimal.
  def indexPoint(p: Point): Option[SepIndex] = {
    //        println(s"level $level, index $index")
    if (depth == level) {
      if (exactlyInsideThisLevel(p))
        Some(if (0 != index) SepIndex(index) else SepIndex())
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
        val subKeys = surelyInsideHex.indexPoint(p).get.keys
        Some(SepIndex(if (0 != index) index :: subKeys else subKeys))
      } else {
        indexUnlessSurelyOutside(s0, p, d0, subLevel.surelyOutsideSquared)
          .orElse(indexUnlessSurelyOutside(s1, p, d1, subLevel.surelyOutsideSquared))
          .orElse(indexUnlessSurelyOutside(s2, p, d2, subLevel.surelyOutsideSquared))
          .map(subIndex => SepIndex(
            if (0 != index) index :: subIndex.keys else subIndex.keys))
      }
    }
  }

  def indexLine(l: Line): List[SepIndex] =
    indexLineByList(l).map(SepIndex(_))

  def indexLineByList(l: Line): List[List[Int]] = {
    //move to origo
    val tx0 = l.p0.x - center.x
    val ty0 = l.p0.y - center.y
    val tx1 = l.p1.x - center.x
    val ty1 = l.p1.y - center.y

    if (lineMaybeInsideCentralized(tx0, ty0, tx1, ty1)) {
      if (level == depth) {
        if (intersectsCentralized(tx0, ty0, tx1, ty1) && 0 != index) {
          List(List(index))
        } else {
          Nil
        }
      } else {
        subHexes.toList
          .map(_.indexLineByList(l))
          .filter(_.nonEmpty)
          .flatMap(_.map(si =>
            if (0 != index) index :: si
            else si
          ))
      }
    } else {
      Nil
    }
  }

  def intersects(l: Line): Boolean = {
    //move to origo
    val tx0 = l.p0.x - center.x
    val ty0 = l.p0.y - center.y
    val tx1 = l.p1.x - center.x
    val ty1 = l.p1.y - center.y
    lineMaybeInsideCentralized(tx0, ty0, tx1, ty1) &&
      intersectsCentralized(tx0, ty0, tx1, ty1)
  }

  def lineMaybeInsideCentralized(tx0: Double, ty0: Double, tx1: Double, ty1: Double) = {
    //line two-point equation
    val ta = ty0 - ty1
    val tb = tx1 - tx0
    val tc = tx0 * ty1 - tx1 * ty0
    //squared distance from line to origo
    val td = tc * tc / (ta * ta + tb * tb)
    td < levelInfo.surelyOutsideSquared
  }

  def intersectsCentralized(tx0: Double, ty0: Double, tx1: Double, ty1: Double): Boolean = {
    val y0 = stretchY(rotateYToBaselineScaleToUnit(tx0, ty0))
    val x0 = skewX(rotateXToBaselineScaleToUnit(tx0, ty0), y0)
    val y1 = stretchY(rotateYToBaselineScaleToUnit(tx1, ty1))
    val x1 = skewX(rotateXToBaselineScaleToUnit(tx1, ty1), y1)
    val sx = x1 - x0
    val sy = y1 - y0
    //the vertices of the centered, scaled, skewed and stretched hex at the center:
    /*
              |
              +-----+
            / |     |
          /   |     |
     ---+-----o-----+---
        |     |   /
        |     | /
        +-----+
              |
     */
    val h0x = 0d
    val h0y = 1d
    val h1x = 1d
    val h1y = 1d
    val h2x = 1d
    val h2y = 0d
    val h3x = 0d
    val h3y = -1d
    val h4x = -1d
    val h4y = -1d
    val h5x = -1d
    val h5y = 0d
    Line.intersects(h0x, h0y, h1x, h1y, x0, y0, sx, sy) ||
      Line.intersects(h1x, h1y, h2x, h2y, x0, y0, sx, sy) ||
      Line.intersects(h2x, h2y, h3x, h3y, x0, y0, sx, sy) ||
      Line.intersects(h3x, h3y, h4x, h4y, x0, y0, sx, sy) ||
      Line.intersects(h4x, h4y, h5x, h5y, x0, y0, sx, sy) ||
      Line.intersects(h5x, h5y, h0x, h0y, x0, y0, sx, sy) ||
      (transformedInside(x0, y0) && transformedInside(x1, y1))
  }

  private def indexUnlessSurelyOutside(hex: SepHex, p: Point, distanceSquared: Double, surelyOutsideSquared: Double) =
    if (distanceSquared < surelyOutsideSquared) {
      hex.indexPoint(p)
    } else None

  //TODO distances can be severely optimized.
  private def shortestThreeDistanceSquareds(p: Point): List[(SepHex, Double)] = {
    val distanceSquareds = subHexes.map { sh =>
      val x = p.x - sh.center.x
      val y = p.y - sh.center.y
      //      println(s"p - c = $p - ${sh.center}, d = ${math.sqrt(x * x + y * y)}, d2 = ${x * x + y * y}")
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
      //stretch rotated scaled y and check
      val dy = stretchY(rotateYToBaselineScaleToUnit(tx, ty))
      transformedInside(skewX(rotateXToBaselineScaleToUnit(tx, ty), dy), dy)
    }
  }

  private def transformedInside(dx: => Double, dy: Double) =
    transformedIsInside(dy) && transformedXIsInside(dx, dy)

  private def transformedXIsInside(dx: Double, dy: Double) =
    transformedIsInside(dx) && transformedIsInside(dy - dx)

  private def transformedIsInside(d: Double) =
    !(1d < d || d < -1d)

  def rotateXToBaselineScaleToUnit(x: Double, y: Double) =
    (x * rotationCosine + y * rotationSine) / R

  def rotateYToBaselineScaleToUnit(x: Double, y: Double) =
    (-x * rotationSine + y * rotationCosine) / R

  def skewX(sx: Double, dy: Double) =
    .5d * dy + sx

  def stretchY(sy: Double) =
    sy * twoDivSqrt3
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

case class Point(x: Double, y: Double)

case class Space(
                  lowerLeft: Point,
                  upperRight: Point
                ) {
  assume(lowerLeft.x < upperRight.x && lowerLeft.y < upperRight.y, "Space must be unwarped and nonempty")
}

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
  val rectInside = 0.87
  val surelyInside = 0.71
  val surelyOutside = 1.05
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
   * Some buffer is appended around the space so we can be sure it fits.
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
    val unbufferedR = if (maxH < h) {
      //height limited
      h / 2d / sinPiDiv3
    } else {
      //width limited
      w
    }
    unbufferedR / rectInside
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
}
