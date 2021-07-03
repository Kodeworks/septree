package no.kodeworks.septree

import fastparse.NoWhitespace._
import fastparse.Parsed.{Failure, Success}
import fastparse._

case class Svg()

sealed trait PathCommand

sealed trait DrawToCommand extends PathCommand

case class MoveTo(coords: Seq[Point], relative: Boolean) extends PathCommand

case class ClosePath() extends DrawToCommand

case class LineTo(coords: Seq[Point], relative: Boolean) extends DrawToCommand

case class HorizontalLineTo(xs: Seq[Double], relative: Boolean) extends DrawToCommand

case class VerticalLineTo(ys: Seq[Double], relative: Boolean) extends DrawToCommand

case class CurveTo(coords: Seq[(Point, Point, Point)], relative: Boolean) extends DrawToCommand

case class SmoothCurveTo(coords: Seq[(Point, Point)], relative: Boolean) extends DrawToCommand

case class QuadraticBezierCurveTo(coords: Seq[(Point, Point)], relative: Boolean) extends DrawToCommand

case class SmoothQuadraticBezierCurveTo(coords: Seq[Point], relative: Boolean) extends DrawToCommand

case class EllipticalArc(coords: Seq[(Point, Double, Boolean, Boolean, Point)], relative: Boolean) extends DrawToCommand

object SvgCodec {
  def svg[_: P] = P("").map(_ => Svg())

  // https://www.w3.org/TR/SVG/paths.html#PathData
  def `svg-path`[_: P] = P(wsp.rep ~ `moveto-drawto-command-groups`.?.map(_.getOrElse(Seq.empty)) ~ wsp.rep)

  def `moveto-drawto-command-groups`[_: P] = P(`moveto-drawto-command-group`.rep(min = 1, sep = wsp.rep))

  def `moveto-drawto-command-group`[_: P] = P(moveto ~ wsp.rep ~ `drawto-commands`.?.map(_.getOrElse(Seq.empty)))

  def `drawto-commands`[_: P] = P(`drawto-command`.rep(min = 1, sep = wsp.rep))

  def `drawto-command`[_: P] = P(
    closepath
      | lineto
      | `horizontal-lineto`
      | `vertical-lineto`
      | curveto
      | `smooth-curveto`
      | `quadratic-bezier-curveto`
      | `smooth-quadratic-bezier-curveto`
      | `elliptical-arc`
  )

  def `moveto`[_: P] = P(CharIn("Mm").! ~/ wsp.rep ~ `moveto-argument-sequence`).map {
    case (c, coords) => MoveTo(coords, c.head.isLower)
  }

  def `moveto-argument-sequence`[_: P] = P(`coordinate-pair`.rep(min = 1, sep = `comma-wsp`.?))

  def `closepath`[_: P] = P(CharIn("Zz").!).map(_ => ClosePath())

  def `lineto`[_: P] = P(CharIn("Ll").! ~/ wsp.rep ~ `lineto-argument-sequence`).map {
    case (c, coords) => LineTo(coords, c.head.isLower)
  }

  def `lineto-argument-sequence`[_: P] = P(`coordinate-pair`.rep(min = 1, sep = `comma-wsp`.?))

  def `horizontal-lineto`[_: P] = P(CharIn("Hh").! ~/ wsp.rep ~ `horizontal-lineto-argument-sequence`).map {
    case (c, coords) => HorizontalLineTo(coords, c.head.isLower)
  }

  def `horizontal-lineto-argument-sequence`[_: P] = P(coordinate.rep(min = 1, sep = `comma-wsp`.?))

  def `vertical-lineto`[_: P] = P(CharIn("Vv").! ~/ wsp.rep ~ `vertical-lineto-argument-sequence`).map {
    case (c, coords) => VerticalLineTo(coords, c.head.isLower)
  }

  def `vertical-lineto-argument-sequence`[_: P] = P(coordinate.rep(min = 1, sep = `comma-wsp`.?))

  def `curveto`[_: P] = P(CharIn("Cc").! ~/ wsp.rep ~ `curveto-argument-sequence`).map {
    case (c, coords) => CurveTo(coords, c.head.isLower)
  }

  def `curveto-argument-sequence`[_: P] = P(`curveto-argument`.rep(min = 1, sep = `comma-wsp`.?))

  def `curveto-argument`[_: P] = P(`coordinate-pair` ~ `comma-wsp`.? ~ `coordinate-pair` ~ `comma-wsp`.? ~ `coordinate-pair`)

  def `smooth-curveto`[_: P] = P(CharIn("Ss").! ~/ wsp.rep ~ `smooth-curveto-argument-sequence`).map {
    case (c, coords) => SmoothCurveTo(coords, c.head.isLower)
  }

  def `smooth-curveto-argument-sequence`[_: P] = P(`smooth-curveto-argument`.rep(min = 1, sep = `comma-wsp`.?))

  def `smooth-curveto-argument`[_: P] = P(`coordinate-pair` ~ `comma-wsp`.? ~ `coordinate-pair`)

  def `quadratic-bezier-curveto`[_: P] = P(CharIn("Qq").! ~/ wsp.rep ~ `quadratic-bezier-curveto-argument-sequence`).map {
    case (c, coords) => QuadraticBezierCurveTo(coords, c.head.isLower)
  }

  def `quadratic-bezier-curveto-argument-sequence`[_: P] = P(`quadratic-bezier-curveto-argument`.rep(min = 1, sep = `comma-wsp`.?))

  def `quadratic-bezier-curveto-argument`[_: P] = P(`coordinate-pair` ~ `comma-wsp`.? ~ `coordinate-pair`)

  def `smooth-quadratic-bezier-curveto`[_: P] = P(CharIn("Tt").! ~/ wsp.rep ~ `smooth-quadratic-bezier-curveto-argument-sequence`).map {
    case (c, coords) => SmoothQuadraticBezierCurveTo(coords, c.head.isLower)
  }

  def `smooth-quadratic-bezier-curveto-argument-sequence`[_: P] = P(`coordinate-pair`.rep(min = 1, sep = `comma-wsp`.?))

  def `elliptical-arc`[_: P] = P(CharIn("Aa").! ~/ wsp.rep ~ `elliptical-arc-argument-sequence`).map {
    case (c, coords) => EllipticalArc(coords, c.head.isLower)
  }

  def `elliptical-arc-argument-sequence`[_: P] = P(`elliptical-arc-argument`.rep(min = 1, sep = `comma-wsp`.?))

  def `elliptical-arc-argument`[_: P] = P(
    (`nonnegative-number` ~ `comma-wsp`.? ~ `nonnegative-number`).map { case (x, y) => Point(x, y) } ~ `comma-wsp`.?
      ~ number ~ `comma-wsp` ~ flag ~ `comma-wsp`.? ~ flag ~ `comma-wsp`.? ~ `coordinate-pair`
  )

  def `coordinate-pair`[_: P] = P(coordinate ~ `comma-wsp`.? ~ coordinate).map { case (x, y) => Point(x, y) }

  def coordinate[_: P] = number

  def `nonnegative-number`[_: P] = P(`integer-constant` | `floating-point-constant`).!.map(_.toDouble)

  // https://www.w3.org/TR/SVG/coords.html#TransformAttribute
  def `transform-list`[_: P] = P(wsp.rep ~ transforms.?.map(_.getOrElse(Mat33.identity)) ~ wsp.rep)

  def transforms[_: P] = P(transform.rep(1, `comma-wsp`.rep(1))).map { ms =>
    ms.foldLeft(Mat33.identity) { (m, o) => m * o }
  }

  def transform[_: P] = P(matrix | translate | scale | rotate | skewX | skewY)

  def matrix[_: P] = P("matrix" ~/ wsp.rep ~ "(" ~/ wsp.rep
    ~ number ~ `comma-wsp`
    ~ number ~ `comma-wsp`
    ~ number ~ `comma-wsp`
    ~ number ~ `comma-wsp`
    ~ number ~ `comma-wsp`
    ~ number ~ wsp.rep ~ ")").map {
    case (a, b, c, d, e, f) => Mat33(a, b, c, d, e, f, 0, 0, 1)
  }

  def translate[_: P] = P("translate" ~/ wsp.rep ~ "(" ~/ wsp.rep ~ number ~ (`comma-wsp` ~ number).? ~ wsp.rep ~ ")").map {
    case (tx, ty) => Mat33.translate(tx, ty.getOrElse(0))
  }

  def scale[_: P] = P("scale" ~/ wsp.rep ~ "(" ~/ wsp.rep ~ number ~ (`comma-wsp` ~ number).? ~ wsp.rep ~ ")").map {
    case (sx, sy) => Mat33.scale(sx, sy.getOrElse(sx))
  }

  def rotate[_: P] = P("rotate" ~/ wsp.rep ~ "(" ~/ wsp.rep ~ number ~ (`comma-wsp` ~ number ~ `comma-wsp` ~ number).? ~ wsp.rep ~ ")").map {
    case (angle, Some((cx, cy))) => Mat33.translate(-cx, -cy) * Mat33.rotate(angle) * Mat33.translate(cx, cy) // TODO: not sure if this should be flipped?
    case (angle, None) => Mat33.rotate(angle)
  }

  def skewX[_: P] = P("skewX" ~/ wsp.rep ~ "(" ~/ wsp.rep ~ number ~ wsp.rep ~ ")").map { angle => Mat33.skewX(angle) }

  //TODO skewX when it's skewY ???
  def skewY[_: P] = P("skewY" ~/ wsp.rep ~ "(" ~/ wsp.rep ~ number ~ wsp.rep ~ ")").map { angle => Mat33.skewX(angle) }

  def number[_: P] = P((sign.? ~ `integer-constant`) | (sign.? ~ `floating-point-constant`)).!.map(_.toDouble)

  def flag[_: P] = P(CharIn("01")).!.map(_ == "1")

  def `comma-wsp`[_: P] = P((wsp.rep(1) ~ comma.? ~ wsp.rep) | (comma ~ wsp.rep))

  def comma[_: P] = P(",")

  def `integer-constant`[_: P] = P(`digit-sequence`)

  def `floating-point-constant`[_: P] = P((`fractional-constant` ~ exponent.?) | (`digit-sequence` ~ exponent))

  def `fractional-constant`[_: P] = P((`digit-sequence`.? ~ "." ~ `digit-sequence`) | (`digit-sequence` ~ "."))

  def exponent[_: P] = P(CharIn("eE") ~ sign.? ~ `digit-sequence`)

  def sign[_: P] = P(CharIn("+\\-"))

  def `digit-sequence`[_: P] = P(digit.rep(1))

  def digit[_: P] = P(CharIn("0123456789"))

  def wsp[_: P] = P(CharIn("\u0020\u0009\u000D\u000A"))

  def toSvg(s: String): Svg = {
    parse(s, svg(_)) match {
      case Success(ok, _) => ok
      case Failure(str, i, extra) => throw new RuntimeException(str)
    }
  }
}