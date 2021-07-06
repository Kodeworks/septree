package no.kodeworks.septree

import org.junit.Test

import scala.io.Source

class SvgTest {

  @Test
  def testy(): Unit = {
    val svgContent = Source.fromResource("graphpaper.svg").mkString
    val space = Space(Point(0d, 0d), Point(100d, 100d))
    val gg = SvgParser.svgStringToLinesScaledToSpace(svgContent, Some(space), .9)
    println(gg.take(5).mkString("\n"))
  }
}
