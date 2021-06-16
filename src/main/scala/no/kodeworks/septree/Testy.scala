package no.kodeworks.septree

//TODO fails - investigate
object Testy extends App {
  val sepTree = SepTree(Space(Point(5500d, 9850d), Point(15500, 19850d)), 5)
  val points = (1 to 100).toList.map(_ =>
    Point(
      math.random * (sepTree.space.upperRight.x - sepTree.space.lowerLeft.x) + sepTree.space.lowerLeft.x,
      math.random * (sepTree.space.upperRight.y - sepTree.space.lowerLeft.y) + sepTree.space.lowerLeft.y)
  )

  val indexes = points.map(p => try {
    sepTree.indexPoint(p)
  } catch {
    case e => println(s"Could not index point $p")
      throw e
  })
  println(indexes.mkString("\n"))
}