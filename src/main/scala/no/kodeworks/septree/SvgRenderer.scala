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

    val lines = Array(
      Line(Point(sepTree.space.lowerLeft.x, sepTree.space.upperRight.y), sepTree.space.upperRight),
      Line(sepTree.space.upperRight, Point(sepTree.space.upperRight.x, sepTree.space.lowerLeft.y)),
      Line(Point(sepTree.space.upperRight.x, sepTree.space.lowerLeft.y), sepTree.space.lowerLeft),
      Line(sepTree.space.lowerLeft, Point(sepTree.space.lowerLeft.x, sepTree.space.upperRight.y))
    )
    var s = System.currentTimeMillis()
    val lineIndices = lines.map(hex.indexLine)
    println(s"lineIndices took ${System.currentTimeMillis - s} millis")
    s = System.currentTimeMillis()
    val lineSelector = lineIndices.map(SepSelector.fromIndices).reduce(_ merge _)
    println(s"lineSelector took ${System.currentTimeMillis - s} millis")
    s = System.currentTimeMillis()
    val hexes = hex.select(lineSelector)
    println(s"select took ${System.currentTimeMillis - s} millis")
    //        val hexes = hex.toList(sepTree.depth)
    //    val hexes = hex.select(
    //      SepSelector(0,
    //        SepSelector(3,
    //          SepSelector(6,
    //            SepSelector(2),
    //            SepSelector(3,
    //              SepSelector(1),
    //              SepSelector(7,
    //                SepSelector(4,
    //                  SepSelector(5),
    //                  SepSelector(7)
    //                ),
    //                SepSelector(7)
    //              )
    //            )
    //          ),
    //          SepSelector(7)
    //        ),
    //        SepSelector(7)
    //      )
    //    )

    val hexPaths = {
      if (hexes.isEmpty) Nil else
        hexes.flatMap { sh =>
          val maybeStroke = if (curLevel != sh.levelInfo.level) {
            val stroke = baseLineWidth - curLevel
            val maybeEndStroke = if (curLevel != 0) List("</g>") else Nil
            curLevel = sh.levelInfo.level
            maybeEndStroke ++ List(s"""<g stroke-width="$stroke">""")
          } else Nil
          val css = sh.corners.toList.map(p =>
            s"${p.x},${transformY(p.y)}"
          )
          maybeStroke ++
            List(s"""<path d="M ${css.head} L ${css.tail.mkString(" ")} Z" />""")
        } ++ List("</g>")
    }
    val x0 = sepTree.space.lowerLeft.x
    val y0 = sepTree.space.lowerLeft.y
    val x1 = sepTree.space.upperRight.x
    val y1 = sepTree.space.upperRight.y
    val rect = s"""<path stroke-width="11" d="M $x0,$y1 L $x1,$y1 $x1,$y0 $x0,$y0 Z" />"""
    (rect :: hexPaths).mkString("\n")
  }

  val sepTree = SepTree(Space(Point(5500d, 9850d), Point(15500, 19850d)), 6)
  renderToFile(sepTree, 29702d -)
}
