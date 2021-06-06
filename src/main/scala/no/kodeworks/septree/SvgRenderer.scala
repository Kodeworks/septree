package no.kodeworks.septree

object SvgRenderer {
  def render(sepTree: SepTree) = {
    val hex = sepTree.hex
    //    <path fill="none" stroke="rgb(128,128,64)" d="M 1024,14000 L 4096,7000 8192,3000 12000,1250 Z" />
    val cornerStrings = hex.corners.toList.map(p =>
      s"${p.x.toInt},${p.y.toInt}"
    )

    val x0 = sepTree.space.lowerLeft.x.toInt
    val y0 = sepTree.space.lowerLeft.y.toInt
    val x1 = sepTree.space.upperRight.x.toInt
    val y1 = sepTree.space.upperRight.y.toInt
    Array(
      s"""<path fill="none" stroke="rgb(128,128,64)" d="M $x0,$y1 L $x1,$y1 $x1,$y0 $x0,$y0 Z" />""",
      s"""<path fill="none" stroke="rgb(128,128,64)" d="M ${cornerStrings.head} L ${cornerStrings.tail.mkString(" ")} Z" />"""
    ).mkString("\n")
  }
}
