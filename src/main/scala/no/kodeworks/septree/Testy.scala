package no.kodeworks.septree

object Testy extends App {
  /*
  999 - 19998
  847 - 28847
  xdiff: 19000 => 9500
  ydiff: 28000 => 14000
  cx: 10500
  cy: 14850
  sx0: 999 + (10500 - 999) / 2 = 5750
  sx1: 10500 + (19998 - 10500) / 2 = 15250
  sy0: 14850 - 4750 = 10100
  sy1: 14850 + 4750 = 19600

*/
  val sepTree = SepTree(Space(Point(5750d, 10100d), Point(15250, 19600d)), 1)
  val tree = sepTree.hex()
  println(tree)

  println(SvgRenderer.render(sepTree))
}
