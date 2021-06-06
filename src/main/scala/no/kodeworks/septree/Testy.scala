package no.kodeworks.septree

object Testy extends App {
  val sepTree = SepTree(Space(Point(5750d, 10100d), Point(15250, 19600d)), 1)
  val tree = sepTree.hex
  val gg = tree.toList(3)
  println(gg.map(_.level))
  println(gg.size)
}
