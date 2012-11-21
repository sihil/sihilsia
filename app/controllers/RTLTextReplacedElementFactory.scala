package controllers

import com.itextpdf.text.{Font, BaseColor, DocumentException, Phrase}
import com.itextpdf.text.pdf.ColumnText
import com.itextpdf.text.pdf.PdfContentByte
import com.itextpdf.text.pdf.PdfWriter
import org.w3c.dom.Element
import org.xhtmlrenderer.css.parser.FSColor
import org.xhtmlrenderer.css.parser.FSRGBColor
import org.xhtmlrenderer.css.style.CalculatedStyle
import org.xhtmlrenderer.css.style.derived.RectPropertySet
import org.xhtmlrenderer.extend.ReplacedElement
import org.xhtmlrenderer.extend.ReplacedElementFactory
import org.xhtmlrenderer.extend.UserAgentCallback
import org.xhtmlrenderer.layout.LayoutContext
import org.xhtmlrenderer.pdf.ITextFSFont
import org.xhtmlrenderer.pdf.ITextOutputDevice
import org.xhtmlrenderer.pdf.ITextReplacedElement
import org.xhtmlrenderer.pdf.ITextReplacedElementFactory
import org.xhtmlrenderer.render.BlockBox
import org.xhtmlrenderer.render.PageBox
import org.xhtmlrenderer.render.RenderingContext
import org.xhtmlrenderer.simple.extend.FormSubmissionListener
import java.awt.{Point, Color}

class RTLTextReplacedElementFactory(outputDevice: ITextOutputDevice, val cssClassName: String) extends ReplacedElementFactory {
  defaultFactory = new ITextReplacedElementFactory(outputDevice)

  def createReplacedElement(c: LayoutContext, box: BlockBox, uac: UserAgentCallback, cssWidth: Int, cssHeight: Int): ReplacedElement = {
    val element: Element = box.getElement
    if (element == null) {
      null
    }
    if (element.getAttribute("class").contains(cssClassName)) {
      val text: String = element.getTextContent.replaceAll("(?m)\\s+", " ")
      new RTLText(c, box, uac, cssWidth, cssHeight, text)
    }
    else {
      defaultFactory.createReplacedElement(c, box, uac, cssWidth, cssHeight)
    }
  }

  def reset() {}

  def remove(e: Element) {}

  def setFormSubmissionListener(listener: FormSubmissionListener) {}

  private var defaultFactory: ITextReplacedElementFactory = null
}

class RTLText extends ITextReplacedElement with Logging {
  private[controllers] def this(c: LayoutContext, box: BlockBox, uac: UserAgentCallback, cssWidth: Int, cssHeight: Int, text: String) {
    this()
    this.text = text
    initDimensions(c, box, cssWidth, cssHeight)
    val element: Element = box.getElement
    align = element.getAttribute("align").toLowerCase match {
      case "left" => com.itextpdf.text.Element.ALIGN_LEFT
      case "center" => com.itextpdf.text.Element.ALIGN_CENTER
      case "right" => com.itextpdf.text.Element.ALIGN_RIGHT
      case _ => com.itextpdf.text.Element.ALIGN_LEFT
    }
    direction = element.getAttribute("direction").toLowerCase match {
      case "ltr" => PdfWriter.RUN_DIRECTION_LTR
      case "default" => PdfWriter.RUN_DIRECTION_DEFAULT
      case "no-bidi" => PdfWriter.RUN_DIRECTION_NO_BIDI
      case "rtl" => PdfWriter.RUN_DIRECTION_RTL
    }
    val ecolor: String = element.getAttribute("color")
    if (!ecolor.isEmpty) {
      color = new BaseColor(Color.decode(ecolor).getRGB)
    }
    val efontSize: String = element.getAttribute("font-size")
    if (!efontSize.isEmpty) {
      this.fontSize = Some(efontSize.toFloat)
    }
  }

  def getIntrinsicWidth = width
  def getIntrinsicHeight = height


  def getLocation = location

  def setLocation(x: Int, y: Int) {
    location.x = x
    location.y = y
  }

  def detach(c: LayoutContext) {
  }

  def isRequiresInteractivePaint = false
  def hasBaseline = false
  def getBaseline = 0

  def paint(c: RenderingContext, outputDevice: ITextOutputDevice, box: BlockBox) {
    try {
      val writer: PdfWriter = outputDevice.getWriter
      val cb: PdfContentByte = writer.getDirectContent
      val font: ITextFSFont = box.getStyle.getFSFont(c).asInstanceOf[ITextFSFont]
      val pdfFontSize: Float = fontSize.getOrElse(outputDevice.getDeviceLength(font.getSize2D))
      val color: FSColor = box.getStyle.getColor
      var bc: BaseColor = null
      if (this.color != null) {
        bc = this.color
      }
      else if (color.isInstanceOf[FSRGBColor]) {
        val cc: FSRGBColor = color.asInstanceOf[FSRGBColor]
        bc = new BaseColor(cc.getRed, cc.getGreen, cc.getBlue)
      }
      val ct: ColumnText = new ColumnText(cb)
      setupColumnCoordinates(c, outputDevice, box)
      ct.setSimpleColumn(llx, lly, urx, ury)
      ct.setSpaceCharRatio(PdfWriter.NO_SPACE_CHAR_RATIO)
      ct.setLeading(0, 1)
      ct.setRunDirection(direction)
      ct.setAlignment(align)
      if (bc == null) {
        ct.addText(new Phrase(text, new Font(font.getFontDescription.getFont, pdfFontSize)))
      }
      else {
        ct.addText(new Phrase(text, new Font(font.getFontDescription.getFont, pdfFontSize, 0, bc)))
      }
      ct.go
    }
    catch {
      case e: DocumentException => {
        log.warn("error while processing rtl text", e)
      }
    }
  }

  private def setupColumnCoordinates(c: RenderingContext, outputDevice: ITextOutputDevice, box: BlockBox) {
    val page: PageBox = c.getPage
    val dotsPerPoint: Float = outputDevice.getDotsPerPoint
    val marginBorderPaddingLeft: Float = page.getMarginBorderPadding(c, CalculatedStyle.LEFT)
    val marginBorderPaddingBottom: Float = page.getMarginBorderPadding(c, CalculatedStyle.BOTTOM)
    val margin: RectPropertySet = box.getMargin(c)
    val padding: RectPropertySet = box.getPadding(c)
    val dist: Float = (page.getBottom - box.getAbsY + marginBorderPaddingBottom)
    llx = ((margin.left + padding.left + box.getAbsX + marginBorderPaddingLeft) / dotsPerPoint).asInstanceOf[Int]
    lly = ((dist - box.getHeight) / dotsPerPoint).asInstanceOf[Int]
    urx = ((box.getAbsX + box.getWidth + marginBorderPaddingLeft) / dotsPerPoint).asInstanceOf[Int]
    ury = ((dist + margin.bottom + padding.bottom) / dotsPerPoint).asInstanceOf[Int]
  }

  protected def initDimensions(c: LayoutContext, box: BlockBox, cssWidth: Int, cssHeight: Int) {
    val style: CalculatedStyle = box.getStyle
    val element: Element = box.getElement
    var scalex: Float = 0.1f
    var scaley: Float = 0.06f
    var lines: Int = 1
    val lines1: String = element.getAttribute("lines")
    if (!lines1.isEmpty) {
      lines = Integer.parseInt(lines1)
    }
    val sx: String = element.getAttribute("scale-x")
    if (!sx.isEmpty) {
      try {
        scalex = sx.toFloat
      }
      catch {
        case e: Exception => {
          System.err.println("Bad scale-x attribute value: " + sx)
        }
      }
    }
    val sy: String = element.getAttribute("scale-y")
    if (!sy.isEmpty) {
      try {
        scaley = sy.toFloat
      }
      catch {
        case e: Exception => {
          System.err.println("Bad scale-y attribute value: " + sx)
        }
      }
    }
    val ewidth: String = element.getAttribute("width")
    if (!ewidth.isEmpty) {
      width = Integer.parseInt(ewidth) * c.getDotsPerPixel
    }
    else if (cssWidth != -1) {
      width = cssWidth
    }
    else {
      width = (c.getTextRenderer.getWidth(c.getFontContext, style.getFSFont(c), text) / 2)
    }
    val eheight: String = element.getAttribute("height")
    if (!eheight.isEmpty) {
      height = Integer.parseInt(eheight) * c.getDotsPerPixel
    }
    else if (cssHeight != -1) {
      height = cssHeight
    }
    else {
      height = ((style.getLineHeight(c) * lines).asInstanceOf[Int])
    }
    width = ((width * c.getDotsPerPixel) * scalex).toInt
    height = ((height * c.getDotsPerPixel) * scaley).toInt
  }

  private var width: Int = 0
  private var height: Int = 0
  private var text: String = null
  private var align: Int = 0
  private var color: BaseColor = null
  private var fontSize: Option[Float] = None
  private var direction: Int = PdfWriter.RUN_DIRECTION_LTR
  private val location: Point = new Point
  private var llx: Int = 0
  private var lly: Int = 0
  private var urx: Int = 0
  private var ury: Int = 0
}