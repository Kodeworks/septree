package no.kodeworks.septree

object SepTree {
  val piDiv6SubArctan2div5 = math.Pi / 6d - math.atan(2d / 5d) // negative rotation for each level
  val sqrt7 = math.sqrt(7d) // factor of R for each level
  val piDiv3 = math.Pi / 3d // 60 deg in rads, angle from hex 4 to hex 2 etc
  val r = math.sin(piDiv3)
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
  ).map(_ - piDiv6SubArctan2div5)
  val surelyInside = 0.8
  val surelyInside2 = surelyInside * surelyInside
  val surelyOutside = 1.2
  val surelyOutside2 = surelyOutside * surelyOutside

  // Unit (r = 1) center of each subhex.
  // center1 = centers(0) etc.
  // Multiply px,py with real r to get real px,py
  val centers = rots.map(rot =>
    (math.cos(rot) * 2d * r / sqrt7, math.sin(rot) * 2d * r / sqrt7)
  )
  centers(3) = (0d, 0d) // center hex is rotated, but not offset

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
  def indexPoint(
                  point: (Double, Double),
                  space: ((Double, Double), (Double, Double)) = ((-1d, -1d), (1d, 1d)),
                  depth: Int = 1,
                  levels: Int = 1
                ): SepIndex = {
    val (px, py) = point
    val (s0@(sx0, sy0), s1@(sx1, sy1)) = space
    assume(sx0 <= px && px <= sx1 && sy0 <= py && py <= sy1, "point must be within space")
    assume(sx0 < sx1 && sy0 < sy1, "space must consist of both vertical and horizontal positive distance")
    assume(0 < depth, "depth must be positive")

    if (levels == depth) {
      SepIndex.depthOne
    } else {
      val px2 = px * px
      val py2 = py * py
      if (px2 + py2 < surelyInside2) {
        SepIndex(4 :: indexPoint(point, space, depth, levels + 1).keys)
      } else {
        //TODO
        SepIndex.depthOne
      }
    }
  }
}
