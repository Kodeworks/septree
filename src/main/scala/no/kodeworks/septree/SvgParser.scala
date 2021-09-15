package no.kodeworks.septree


import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.batik.bridge.{BridgeContext, DocumentLoader, GVTBuilder, UserAgentAdapter}
import org.apache.batik.ext.awt.geom.ExtendedGeneralPath
import org.apache.batik.gvt.{CompositeGraphicsNode, GraphicsNode, ShapeNode}
import org.apache.batik.util.XMLResourceDescriptor

import java.awt.Shape
import java.awt.geom.PathIterator
import java.io.StringReader
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.mutable

object SvgParser {
  def svgStringToLinesScaledToSpace(svgString: String, space: Option[Space] = None, scale: Double = 1d): List[Line] = {
    checkScale(scale)
    svgToLinesScaledToSpace(stringToSvg(svgString), space, scale)
  }

  def stringToSvg(s: String) = {
    val parser = XMLResourceDescriptor.getXMLParserClassName
    val f = new SAXSVGDocumentFactory(parser)
    val document = f.createSVGDocument("", new StringReader(s))
    val userAgent = new UserAgentAdapter
    val loader = new DocumentLoader(userAgent)
    val bridgeContext = new BridgeContext(userAgent, loader)
    bridgeContext.setDynamic(true)
    val builder = new GVTBuilder
    builder.build(bridgeContext, document)
  }

  def svgToLinesScaledToSpace(svg: GraphicsNode, space: Option[Space] = None, scale: Double = 1d): List[Line] = {
    checkScale(scale)
    val lines = svgToLines(svg: GraphicsNode)
    val mm = linesSpace(lines)
    val spaceOrMm = space.getOrElse(mm)
    val mmSpan = mm.span
    val spaceSpan = spaceOrMm.span
    val ratioX = spaceSpan.x / mmSpan.x
    val ratioY = spaceSpan.y / mmSpan.y
    val realScale = math.min(ratioX, ratioY) * scale

    val buf = (mmSpan.x * realScale - mmSpan.y * realScale) / 2d
    var (bufX, bufY) = if (0 < buf) (0d, buf) else (-buf, 0d)
    bufX += (1d - scale) * spaceSpan.x / 2d
    bufY += (1d - scale) * spaceSpan.y / 2d

    def transScaleTransFlipYPoint(p: Point) = Point(
      (p.x - mm.lowerLeft.x) * realScale + spaceOrMm.lowerLeft.x + bufX,
      spaceOrMm.upperRight.y - ((p.y - mm.lowerLeft.y) * realScale + spaceOrMm.lowerLeft.y + bufY))

    lines.map(l => Line(transScaleTransFlipYPoint(l.p0), transScaleTransFlipYPoint(l.p1)))
  }

  private def checkScale(scale: Double) = {
    assume(0d < scale && scale <= 1d, "Scale")
  }

  def svgToLines(svg: GraphicsNode): List[Line] = svg match {
    case n: CompositeGraphicsNode =>
      n.iterator.asScala.asInstanceOf[Iterator[GraphicsNode]].flatMap(svgToLines).toList
    case n: ShapeNode =>
      shapeToLines(n.getShape)
  }

  def shapeToLines(shape: Shape): List[Line] = {
    val ds = Array.ofDim[Double](2)
    shape match {
      case s: ExtendedGeneralPath =>
        val epi = s.getExtendedPathIterator
        val lb = mutable.ListBuffer[Point]()
        while (!epi.isDone) {
          epi.currentSegment(ds) match {
            case PathIterator.SEG_MOVETO | PathIterator.SEG_LINETO =>
              val cs = epi.currentSegment()
              val Array(px, py) = ds
              lb += Point(px, py)
            case _ =>
          }
          epi.next
        }
        if (1 < lb.size) {
          lb.toList.sliding(2).map { case List(a, b) => Line(a, b) }.toList
        } else Nil
    }
  }

  def linesSpace(lines: List[Line]): Space =
    lines.flatMap(l => List(l.p0, l.p1)) match {
      case Nil => Space(Point(0d, 0d), Point(0d, 0d))
      case ps =>
        val (xs, ys) = ps.flatMap(Point.unapply).unzip
        Space(Point(xs.min, ys.min), Point(xs.max, ys.max))
    }
}