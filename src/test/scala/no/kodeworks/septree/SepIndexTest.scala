package no.kodeworks.septree

import org.junit.Assert.assertEquals
import org.junit.Test

class SepIndexTest {
  @Test
  def test(): Unit = {
    val l0 = (1 to 7).toList
    val si0 = SepIndex(l0)
    assertEquals(1, si0.indices.size)
    assertEquals("SepIndex(1,2,3,4,5,6,7)", si0.toString)
    val si1 = SepIndex(1, 2, 3, 4, 5, 6, 7)
    assertEquals(si0.indices, si1.indices)
    assertEquals(l0, si0.keys)
    val l1 = (1 to 3).flatMap(_ => 1 to 7).toList
    val si2 = SepIndex(l1)
    assertEquals(1, si2.indices.size)
    val l2 = l1 ::: List(1)
    val si3 = SepIndex(l2)
    assertEquals(2, si3.indices.size)
  }
}
