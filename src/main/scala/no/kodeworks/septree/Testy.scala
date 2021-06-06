package no.kodeworks.septree

object Testy extends App {
  val sepTree = SepTree(Space(Point(-1d, -1d), Point(1d, 1d)), 1)
  val tree = sepTree.tree()
  println(tree)
}
