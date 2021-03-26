package no.kodeworks.septree

object SepTree {
  /*
   * Space is an area wrapped in an initial hex like this:
   *      __________
   *     /          \
   *    /|          |\
   *   / |          | \
   *  /  |          |  \
   * /   |          |   \
   * \   |          |   /
   *  \  |          |  /
   *   \ |          | /
   *    \|          |/
   *     \__________/
   */
  def indexPoint(
                  point: (Double, Double),
                  space: (Double, Double),
                  depth: Int = 1
                ): SepIndex = {
    val (px, py) = point
    val (sx, sy) = space
    assume(0d <= px && 0d <= py && 0d <= sx && 0d <= sy && px <= sx && py <= sy && 0 < depth,
      "point and space must be nonnegative, point must be inside space, depth must be positive")
    ???
  }
}
