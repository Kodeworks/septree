package no.kodeworks.septree

import no.kodeworks.septree.SepTree._
import org.junit.Assert._
import org.junit.Test

class SepTreeTest {
  @Test(expected = classOf[AssertionError])
  def testBoundsPx(): Unit = {
    indexPoint(Point(-2d, -1d))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsPy(): Unit = {
    indexPoint(Point(-1d, -2d))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsSx(): Unit = {
    indexPoint(Point(0d, 0d), Space(Point(1d, -1d), Point(-1d, 1d)))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsSy(): Unit = {
    indexPoint(Point(0d, 0d), Space(Point(-1d, 1d), Point(1d, -1d)))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsDepth(): Unit = {
    indexPoint(Point(0d, 0d), depth = 0)
  }

  @Test
  def testDepthOne(): Unit = {
    val min = indexPoint(Point(0d, 0d))
    val max = indexPoint(Point(1d, 1d))
    assertEquals(SepIndex.depthOne, min)
    assertEquals(SepIndex.depthOne, max)
  }

  @Test
  def testNormalizeDenormalize: Unit = {
    val px = 0d
    val py = 80d
    val sx0 = -30d
    val sx1 = 50d
    val sy0 = 60d
    val sy1 = 100d
    val pnx = SepTree.normalizePlusMinus1(px, sx0, sx1)
    val pny = SepTree.normalizePlusMinus1(py, sy0, sy1)
    assertEquals(-0.25, pnx, 0d)
    assertEquals(0d, pny, 0d)

    val pdx = SepTree.denormalizePlusMinus1(pnx, sx0, sx1)
    val pdy = SepTree.denormalizePlusMinus1(pny, sy0, sy1)
    assertEquals(px, pdx, 0d)
    assertEquals(py, pdy, 0d)
  }

  @Test
  def testDepthTwoSurelyInside(): Unit = {
    val center1 = Point(-.5d, .7d)
    val index1 = indexPoint(center1, depth = 2)
    assertEquals(SepIndex(1), index1)

    val center2 = Point(.7d, .7d)
    val index2 = indexPoint(center2, depth = 2)
    assertEquals(SepIndex(2), index2)

    val center3 = Point(-.9d, .1d)
    val index3 = indexPoint(center3, depth = 2)
    assertEquals(SepIndex(3), index3)

    val center5 = Point(.9d, -.1d)
    val index5 = indexPoint(center5, depth = 2)
    assertEquals(SepIndex(5), index5)

    val center6 = Point(-.7d, -.7d)
    val index6 = indexPoint(center6, depth = 2)
    assertEquals(SepIndex(6), index6)

    val center7 = Point(.5d, -.7d)
    val index7 = indexPoint(center7, depth = 2)
    assertEquals(SepIndex(7), index7)
  }

  @Test
  def testDepthTwoMaybeInside(): Unit = {
    val center1 = Point(-.5d, .7d)
    val index1 = indexPoint(center1, depth = 2)
    assertEquals(SepIndex(1), index1)
  }

  @Test
  def testLeftOfLine(): Unit = {
    assertTrue(SepTree.leftOfLine(Point(3, 3), Point(1, 1), Point(5, 3)))
    assertTrue(SepTree.leftOfLine(Point(2, 3), Point(1, 1), Point(5, 5)))
    assertFalse(SepTree.leftOfLine(Point(3, 3), Point(1, 1), Point(5, 5)))
    assertFalse(SepTree.leftOfLine(Point(3, 2), Point(1, 1), Point(5, 5)))
  }
}
