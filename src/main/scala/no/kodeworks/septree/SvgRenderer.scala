package no.kodeworks.septree

import java.nio.file.{Files, Path}
import scala.io.Source

object SvgRenderer extends App {
  def renderToFile(sepTree: SepTree, transformY: Double => Double = identity) = {
    val paths = renderPaths(sepTree, transformY)
    val gg = Source.fromResource("graphpaper.svg").getLines.toVector.dropRight(2).appended(s"\n$paths\n</g>\n</svg>").mkString("\n")
    Files.writeString(Path.of("graphpaper_with_septree.svg"), gg)
  }

  def renderPaths(sepTree: SepTree, transformY: Double => Double = identity) = {
    val hex = sepTree.hex

    val hexPaths = hex.toList(sepTree.depth).map { sh =>
      val css = sh.corners.toList.map(p =>
        s"${p.x},${transformY(p.y)}"
      )
      s"""<path fill="none" stroke="rgb(128,128,128)" d="M ${css.head} L ${css.tail.mkString(" ")} Z" />"""
    }
    val x0 = sepTree.space.lowerLeft.x
    val y0 = sepTree.space.lowerLeft.y
    val x1 = sepTree.space.upperRight.x
    val y1 = sepTree.space.upperRight.y
    val rect = s"""<path fill="none" stroke="rgb(128,128,128)" d="M $x0,$y1 L $x1,$y1 $x1,$y0 $x0,$y0 Z" />"""
    (rect :: hexPaths).mkString("\n")
  }

  val sepTree = SepTree(Space(Point(5750d, 10100d), Point(15250, 19600d)), 7)
  renderToFile(sepTree, 29702d -)
}
