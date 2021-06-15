package no.kodeworks.septree

import no.kodeworks.septree.SepTree._
import org.junit.Assert._
import org.junit.Test

class SepTreeTest {
  @Test(expected = classOf[AssertionError])
  def testBoundsPx(): Unit = {
    SepTree(Space(Point(-1d, -1d), Point(1d, 1d)), 1)
      .indexPoint(Point(-2d, -1d))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsPy(): Unit = {
    SepTree(Space(Point(-1d, -1d), Point(1d, 1d)), 1)
      .indexPoint(Point(-1d, -2d))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsSx(): Unit = {
    SepTree(Space(Point(1d, -1d), Point(-1d, 1d)), 1)
      .indexPoint(Point(0d, 0d))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsSy(): Unit = {
    SepTree(Space(Point(-1d, 1d), Point(1d, -1d)), 1)
      .indexPoint(Point(0d, 0d))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsDepth(): Unit = {
    SepTree(Space(Point(-1d, -1d), Point(1d, 1d)), 0)
  }

  @Test
  def testDepthOne(): Unit = {
    val tree = SepTree(Space(Point(-1d, -1d), Point(1d, 1d)), 1)
    val min = tree.indexPoint(Point(0d, 0d))
    val max = tree.indexPoint(Point(1d, 1d))
    assertEquals(SepIndex.depthOne, min)
    assertEquals(SepIndex.depthOne, max)
  }

  @Test
  def testDepthTwoSurelyInside(): Unit = {
    val tree = SepTree(Space(Point(-1d, -1d), Point(1d, 1d)), 2)
    val center1 = Point(-.5d, .7d)
    val index1 = tree.indexPoint(center1)
    assertEquals(SepIndex(1), index1)

    val center2 = Point(.7d, .7d)
    val index2 = tree.indexPoint(center2)
    assertEquals(SepIndex(2), index2)

    val center3 = Point(-.9d, .1d)
    val index3 = tree.indexPoint(center3)
    assertEquals(SepIndex(6), index3)

    val center5 = Point(.9d, -.1d)
    val index5 = tree.indexPoint(center5)
    assertEquals(SepIndex(3), index5)

    val center6 = Point(-.7d, -.7d)
    val index6 = tree.indexPoint(center6)
    assertEquals(SepIndex(5), index6)

    val center7 = Point(.5d, -.7d)
    val index7 = tree.indexPoint(center7)
    assertEquals(SepIndex(4), index7)
  }

  @Test
  def testDepthTwoMaybeInside(): Unit = {
    val tree = SepTree(Space(Point(-1d, -1d), Point(1d, 1d)), 2)
    val center1 = Point(-.5d, .7d)
    val index1 = tree.indexPoint(center1)
    assertEquals(SepIndex(1), index1)
  }

  @Test
  def testLeftOfLine(): Unit = {
    assertTrue(SepTree.leftOfLine(Point(3, 3), Point(1, 1), Point(5, 3)))
    assertTrue(SepTree.leftOfLine(Point(2, 3), Point(1, 1), Point(5, 5)))
    assertFalse(SepTree.leftOfLine(Point(3, 3), Point(1, 1), Point(5, 5)))
    assertFalse(SepTree.leftOfLine(Point(3, 2), Point(1, 1), Point(5, 5)))
  }

  @Test
  def testExactlyInsideThisLevel: Unit = {
    val tree = SepTree(Space(Point(100, 100), Point(500, 500)), 3)
    val hex = tree.hex
    assertCornersWithMarginInside(hex)
    hex.subHexes.foreach(assertCornersWithMarginInside(_))
  }

  def assertCornersWithMarginInside(hex: SepHex, margin: Double = 0.000000000001) = {
    val Array(a, b, c, d, e, f) = hex.corners
    val justInsideCorners = List(
      a.x + margin,
      a.y - margin,
      b.x - margin,
      b.y - margin,
      c.x - margin,
      c.y,
      d.x - margin,
      d.y + margin,
      e.x + margin,
      e.y + margin,
      f.x + margin,
      f.y
    ).grouped(2).map { case List(x, y) => Point(x, y) }.toArray
    val insides = justInsideCorners.map(hex.exactlyInsideThisLevel)
    insides.zipWithIndex.foreach { case (inside, i) =>
      assertTrue(s"Corner ${i + 1} not inside", inside)
    }
  }
}
