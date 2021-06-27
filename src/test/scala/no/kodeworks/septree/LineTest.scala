package no.kodeworks.septree

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

class LineTest {
  @Test
  def testIntersect(): Unit = {
    val l0 = Line(Point(0d, 0d), Point(10d, 10d))
    val l1 = Line(Point(0d, 10d), Point(10d, 0d))
    assertTrue(l0.intersects(l1))

    val l2 = Line(Point(0d, 0d), Point(9d, 10d))
    val l3 = Line(Point(0d, 0d), Point(10d, 9d))
    assertTrue(l2.intersects(l3))

    val l4 = Line(Point(0d, 0d), Point(4d, 4d))
    val l5 = Line(Point(5d, 5d), Point(10d, 10d))
    assertFalse(l4.intersects(l5))
  }
}
