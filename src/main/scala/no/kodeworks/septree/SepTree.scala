package no.kodeworks.septree

object SepTree {
  /*
   * Space is an area wrapped in an initial hex.
   * It is snapped to the bottom of the hex i case it does not fill the entire height.
   * Point is within this area.
   *      __________
   *     /          \
   *    /| not space|\
   *   / |          | \
   *  /  |---space--|  \
   * /   |          |   \
   * \   | (p)oint  |   /
   *  \  |          |  /
   *   \ |          | /
   *    \|          |/
   *     \___space__/
   */
  def indexPoint(
                  point: (Double, Double),
                  space: (Double, Double) = (1d, 1d),
                  depth: Int = 1
                ): SepIndex = {
    val (px, py) = point
    val (sx, sy) = space
    assume(0d <= px && 0d <= py && 0d <= sx && 0d <= sy && px <= sx && py <= sy && 0 < depth,
      "point and space must be nonnegative, point must be inside space, depth must be positive")

    if (1 == depth) {
      SepIndex.depthOne
    } else {
      SepIndex.depthOne
    }
  }
}
