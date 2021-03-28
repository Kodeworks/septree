package no.kodeworks.septree

object SepTree {
  val piDiv6SubArctan2div5 = math.Pi / 6d - math.atan(2d / 5d) // negative rotation for each level
  val sqrt7 = math.sqrt(7d) // factor of R for each level
  val piDiv3 = math.Pi / 3d // 60 deg in rads, angle from hex 4 to hex 2 etc
  val sinPiDiv3 = math.sin(piDiv3)
  // Rotations to the center of each subhex.
  // rot1 = rots(0) etc
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

  val centers: Array[(Double, Double, Double, Double) => Array[Double]] = rots.map { rot =>
    (px: Double, py: Double, s: Double, level: Double) =>
      val rotLevel = rot - level * piDiv6SubArctan2div5
      Array(px + math.cos(rotLevel) * s, py + math.sin(rotLevel) * s)
  }
  centers(3) = (px: Double, py: Double, _: Double, _: Double) => Array(px, py)

  /*
   * Space is an area wrapped in an initial hex.
   * It is snapped to the center of the hex in case it does not fill the entire width or height.
   * Point is within this area.
   *       _________________                _________________
   *      /    (n)ospace    \              /   |         |   \
   *     /|_____(s)pace_____|\            /|   |         |   |\
   *    / |                 | \          / |  (s)       (s)  | \
   *   /  |                 |  \        /  |   |         |   |  \
   *  /   |  (p)oint        |   \      /   |   |         |   |   \
   * /    |                 |    \    /    |(n)|         |(n)|    \
   * \    |                 |    /    \    |   |         |   |    /
   *  \   |                 |   /      \   |   |         |   |   /
   *   \  |                 |  /        \  |   |         |   |  /
   *    \ |_____(s)pace_____| /          \ |   |         |   | /
   *     \|    (n)ospace    |/            \|   |     (p) |   |/
   *      \_________________/              \___|_________|___/
   *         Width limited                   Height limited
   *
   * Thus the center of the space always aligns with the center of the initial wrapping hex.
   */

  def calcR(sx0: Double, sy0: Double, sx1: Double, sy1: Double): Double = {
    val w = sx1 - sx0
    val h = sy1 - sy0
    val maxH = 2d * sinPiDiv3 * w
    if (maxH < h) {
      //height limited
      h / 2d / sinPiDiv3
    } else {
      //width limited
      w
    }
  }

  // s = 2*r = sin(pi/3)*d = sin(pi/3)*2*R
  // diminished by 1/sqrt(7) for each level
  def sByLevel(R: Double, level: Double) =
    sinPiDiv3 * 2d * R * math.pow(1d / sqrt7, level)

  def indexPoint(
                  point: (Double, Double),
                  space: ((Double, Double), (Double, Double)) = ((-1d, -1d), (1d, 1d)),
                  depth: Int = 1,
                  level: Int = 1
                ): SepIndex = {
    val (px, py) = point
    val (s0@(sx0, sy0), s1@(sx1, sy1)) = space
    assume(sx0 <= px && px <= sx1 && sy0 <= py && py <= sy1, "point must be within space")
    assume(sx0 < sx1 && sy0 < sy1, "space must consist of both vertical and horizontal positive distance")
    assume(0 < depth, "depth must be positive")
    val R = calcR(sx0, sy0, sx1, sy1)
    println(s"px:$px, py:$py")
    if (level == depth) {
      SepIndex.depthOne
    } else {
      val px2 = px * px
      val py2 = py * py
      val p2sum = px2 + py2
      val d4 = math.sqrt(p2sum)
      val surelyInsideSubHex = surelyInside * R / sqrt7
      //      println(s"sqrtP2sum $sqrtP2sum < inside4 $inside4")
      println(s"distance4 $d4 < surelyInsideSubHex $surelyInsideSubHex")
      if (d4 < surelyInsideSubHex) {
        SepIndex(4 :: indexPoint(point, space, depth, level + 1).keys)
      } else {
        // (px: Double, py: Double, s: Double, level: Double)
        val s = sByLevel(R, level)
        val Array(c1x, c1y) = centers(0)(0d, 0d, s, level)
        println(s"c1: $c1x, $c1y")
        val d1x = c1x - px
        val d1y = c1y - py
        val d1x2 = d1x * d1x
        val d1y2 = d1y * d1y
        val d1 = math.sqrt(d1x2 + d1y2)
        println(s"distance1 $d1 < surelyInsideSubHex $surelyInsideSubHex")
        if (d1 < surelyInsideSubHex) {
          SepIndex(1 :: indexPoint(point, space, depth, level + 1).keys)
        } else {
          //TODO
          SepIndex.depthOne
        }
      }
    }
  }


  def normalizePlusMinus1(x: Double, min: Double, max: Double): Double =
    2d * (x - min) / (max - min) - 1d

  def denormalizePlusMinus1(x: Double, min: Double, max: Double): Double =
    (max - min) * (x + 1d) / 2d + min
}
