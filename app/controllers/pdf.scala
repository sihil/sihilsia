package controllers

import play.api.mvc.{Results, Result}
import play.api.templates.Html
import org.w3c.tidy.Tidy
import java.io._
import org.xhtmlrenderer.pdf.{ITextFSImage, ITextOutputDevice, ITextUserAgent, ITextRenderer}
import play.api.Play
import org.xhtmlrenderer.resource.{CSSResource, ImageResource, XMLResource}
import java.net.URL
import com.itextpdf.text.pdf.BaseFont
import com.itextpdf.text.Image

object Pdf extends Logging {

  var imageCache = Map.empty[URL,Option[Image]]

  def cachedImage(url: URL): Option[Image] = {
    imageCache.get(url).getOrElse{
      val img = image(url)
      imageCache += url -> img
      img
    }
  }

  def image(url: URL): Option[Image] = {
    try {
      log.info("Getting image from URL %s" format url.toString)
      Some(Image.getInstance(url))
    } catch {
      case e:Throwable => None
    }
  }


    class MyUserAgent(outputDevice: ITextOutputDevice) extends ITextUserAgent(outputDevice) {
    override def getImageResource(uri: String): ImageResource = {
      log.info("Getting image from %s" format uri)
      val stream = Play.current.resourceAsStream(uri)
      val image = stream.map { realStream =>
        val image = Image.getInstance(getData(realStream))
        scaleToOutputResolution(image)
        new ImageResource(uri, new ITextFSImage(image))
      } getOrElse(super.getImageResource(uri))
      log.info("Returning image: %s" format image.getImage)
      image
    }

    override def getCSSResource(uri :String): CSSResource = {
      val path = new URL(uri).getPath
      log.info(path)
      val stream = Play.current.resourceAsStream(path)
      stream.map(new CSSResource(_)).getOrElse(super.getCSSResource(uri))
    }

    override def getBinaryResource(uri: String): Array[Byte] = {
      val stream = Play.current.resourceAsStream(uri)
      stream.map(getData(_)).getOrElse(super.getBinaryResource(uri))
    }

    override def getXMLResource(uri: String): XMLResource = {
      val stream = Play.current.resourceAsStream(uri)
      stream.map(XMLResource.load(_)).getOrElse(super.getXMLResource(uri))
    }

    def scaleToOutputResolution(image: Image) {
      val factor: Float = getSharedContext.getDotsPerPixel
      image.scaleAbsolute(image.getPlainWidth * factor, image.getPlainHeight * factor);
    }

  }

  def getData(stream: InputStream): Array[Byte] = {
    val bis = new BufferedInputStream(stream)
    Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
  }

  def apply(html: Html): Result = {
    val pdf = toBytes(tidify(html.body))
    return Results.Ok(pdf).as("application/pdf");
  }

  def toBytes(html: Html): Array[Byte] = {
    toBytes(tidify(html.body))
  }

  def toBytes(string: String): Array[Byte] = {
    toStream(string).toByteArray
  }

  def tidify(body: String): String = {
    val tidy = new Tidy()
    tidy.setXHTML(true)
    val writer = new StringWriter()
    tidy.parse(new StringReader(body), writer)
    writer.getBuffer.toString
  }

  def toStream(string: String): ByteArrayOutputStream = {
    val os = new ByteArrayOutputStream()
    val reader = new StringReader(string)
    val renderer = new ITextRenderer()
    renderer.getFontResolver.addFontDirectory(Play.current.path + "/conf/fonts", BaseFont.EMBEDDED)
    val myUserAgent = new MyUserAgent(renderer.getOutputDevice)
    myUserAgent.setSharedContext(renderer.getSharedContext)
    renderer.getSharedContext.setUserAgentCallback(myUserAgent)
    renderer.getSharedContext.
    val document = XMLResource.load(reader).getDocument()
    renderer.setDocument(document, "http://localhost:9000")
    renderer.layout()
    renderer.createPDF(os)
    os
  }

}
