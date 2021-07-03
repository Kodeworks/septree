package no.kodeworks.septree

import org.junit.Test

import java.io.File
import scala.io.Source

class SvgTest {

  @Test
  def testy(): Unit = {
    val svg = Source.fromResource("graphpaper.svg").mkString
    println(SvgCodec.wspStr)
    println(svg)
    val parsed = SvgCodec.toSvg(svg)
    println(parsed)
  }
}
