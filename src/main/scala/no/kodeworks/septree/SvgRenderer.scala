package no.kodeworks.septree

import java.nio.file.{Files, Path}
import scala.io.Source

object SvgRenderer extends App {
  val minBaseLineWidth = 6

  def renderToFile(sepTree: SepTree, transformY: Double => Double = identity) = {
    val paths = renderPaths(sepTree, transformY)
    val gg = Source.fromResource("graphpaper.svg").getLines.toVector.dropRight(2).appended(s"\n$paths\n</g>\n</svg>").mkString("\n")
    Files.writeString(Path.of("graphpaper_with_septree.svg"), gg)
  }

  def renderPaths(sepTree: SepTree, transformY: Double => Double = identity) = {
    val hex = sepTree.hex
    val baseLineWidth = math.max(minBaseLineWidth, sepTree.depth)
    var curLevel = 0

    val hexPaths = hex.toList(sepTree.depth).flatMap { sh =>
      val maybeStroke = if (curLevel != sh.level) {
        val stroke = baseLineWidth - curLevel
        val maybeEndStroke = if (curLevel != 0) List("</g>") else Nil
        curLevel = sh.level
        maybeEndStroke ++ List(s"""<g stroke-width="$stroke">""")
      } else Nil
      val css = sh.corners.toList.map(p =>
        s"${p.x},${transformY(p.y)}"
      )
      maybeStroke ++
        List(s"""<path d="M ${css.head} L ${css.tail.mkString(" ")} Z" />""")
    } ++ List("</g>")
    val x0 = sepTree.space.lowerLeft.x
    val y0 = sepTree.space.lowerLeft.y
    val x1 = sepTree.space.upperRight.x
    val y1 = sepTree.space.upperRight.y
    val rect = s"""<path stroke-width="11" d="M $x0,$y1 L $x1,$y1 $x1,$y0 $x0,$y0 Z" />"""
    (rect :: hexPaths).mkString("\n")
  }

  val sepTree = SepTree(Space(Point(5750d, 10100d), Point(15250, 19600d)), 5)
  renderToFile(sepTree, 29702d -)
}
