package no.kodeworks.septree

import no.kodeworks.septree.SepIndex.keysPerShortIndex

case class SepIndex(indices: List[Long]) {
  def keys: List[Int] =
    indices.flatMap { index =>
      val keys0 = (0 until keysPerShortIndex).map(s => 7 & (index >> 3 * s).toInt)
      if (index == Long.MaxValue) keys0
      else keys0.takeWhile(0 !=)
    }

  def toSelector = keys.reverse match {
    case head :: tail =>
      SepSelector(0, tail.foldLeft(SepSelector(head))((sel, key) => SepSelector(key, sel)))
  }

  override def toString: String =
    s"SepIndex(${keys.mkString(",")})"
}

object SepIndex {
  val depthOne = new SepIndex(List(longify(List(7))))

  /*
  0 means empty key. There are no more non empty keys to the left. Any keys to the right are non-empty.
  1-7 means non-empty key. The top hex is always index 7.
   */
  val keysPerShortIndex = 64 / 3 //there are 21 keys per long (63 bits)

  def apply(sepKeys: Iterable[Int]): SepIndex = {
    assume(sepKeys.forall(sk => 1 <= sk && sk < 8), s"sepKeys must be from 1 to 7. Was: ${sepKeys.toList.mkString(",")}")
    if (sepKeys.isEmpty) depthOne
    else new SepIndex(sepKeys
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
