package no.kodeworks.septree

import no.kodeworks.septree.SepTree._
import org.junit.Assert._
import org.junit.Test

class SepTreeTest {
  @Test(expected = classOf[AssertionError])
  def testBoundsPx(): Unit = {
    indexPoint((-2d, -1d))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsPy(): Unit = {
    indexPoint((-1d, -2d))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsSx(): Unit = {
    indexPoint((0d, 0d), ((1d, -1d), (-1d, 1d)))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsSy(): Unit = {
    indexPoint((0d, 0d), ((-1d, 1d), (1d, -1d)))
  }

  @Test(expected = classOf[AssertionError])
  def testBoundsDepth(): Unit = {
    indexPoint((0d, 0d), depth = 0)
  }

  @Test
  def testDepthOne(): Unit = {
    val min = indexPoint((0d, 0d))
    val max = indexPoint((1d, 1d))
    assertEquals(SepIndex.depthOne, min)
    assertEquals(SepIndex.depthOne, max)
  }

  @Test
  def testDepthTwo(): Unit = {
    val index4 = indexPoint((0.0d, 0.0d), depth = 2)
    val expect = SepIndex(4)
    assertEquals(expect, index4)
  }
}
