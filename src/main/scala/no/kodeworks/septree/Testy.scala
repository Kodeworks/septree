package no.kodeworks.septree

object Testy extends App {
  val sepTree = SepTree(Space(Point(5750d, 10100d), Point(15250, 19600d)), 7)
  val gg: SepIndex = sepTree.indexPoint(Point(10500d, 14850d))
  println(gg)
}
