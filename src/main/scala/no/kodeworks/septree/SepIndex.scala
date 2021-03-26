package no.kodeworks.septree

import no.kodeworks.septree.SepIndex.keysPerShortIndex

class SepIndex(indices: List[Long]) {
  def keys: List[Int] =
    indices.flatMap { index =>
      val keys0 = (0 until keysPerShortIndex).map(s => 7 & (index >> 3 * s).toInt)
      if (index == Long.MaxValue) keys0
      else keys0.takeWhile(0 !=)
    }

  override def toString: String =
    s"SepIndex(${keys.mkString(",")})"
}

object SepIndex {
  /*
  0 means empty key. There are no more non empty keys to the left. Any keys to the right are non-empty.
  1-7 means non-empty key
   */
  val keysPerShortIndex = 64 / 3 //there are 21 keys per long (63 bits)

  def apply(sepKeys: Iterable[Int]): SepIndex = {
    assume(sepKeys.forall(sk => 1 <= sk && sk < 8))
    new SepIndex(sepKeys
      .grouped(keysPerShortIndex)
      .map(longify)
      .toList)
  }

  def apply(sepKeys: Int*): SepIndex = apply(sepKeys)

  def longify(sepKeys: Iterable[Int]): Long =
    sepKeys
      .zipWithIndex.map { case (sk, i) =>
      sk.toLong << 3 * i
    }.reduce(_ | _)

}
