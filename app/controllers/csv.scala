package controllers

import au.com.bytecode.opencsv.CSVReader
import java.net.{URLDecoder, HttpURLConnection, URLConnection, URL}
import java.io.{InputStream, BufferedReader, InputStreamReader}
import scala.collection.JavaConversions._

case class CSVData(url: URL, separator: Char = ',', quote: Char = '"', escape: Char = '\\' ) extends Logging {

  val reader = new BufferedReader(new InputStreamReader(makeConnection(url)))
  val csvReader = new CSVReader(reader, separator, quote, escape)
  val allLines = try {
    csvReader.readAll()
  } finally {
    csvReader.close()
  }

  val header = allLines.head
  val data = allLines.tail
  val fieldCount = header.length

  def getRows: Seq[Map[String,String]] = {
    data.map(row => header.zip(row).toMap)
  }

  private def openConnectionCheckRedirects(conn: URLConnection): InputStream = {
    var redir: Boolean = false
    var redirects: Int = 0
    var in: InputStream = null
    var c = conn
    var cookieData = ""
    do {
      if (c.isInstanceOf[HttpURLConnection]) {
        val conn = c.asInstanceOf[HttpURLConnection]
        conn.setInstanceFollowRedirects(false)
        if (cookieData != "") conn.setRequestProperty("Cookie", cookieData)
      }
      in = c.getInputStream
      redir = false
      if (c.isInstanceOf[HttpURLConnection]) {
        val http: HttpURLConnection = c.asInstanceOf[HttpURLConnection]
        val stat: Int = http.getResponseCode
        if (stat >= 300 && stat <= 307 && stat != 306 && stat != HttpURLConnection.HTTP_NOT_MODIFIED) {
          val base: URL = http.getURL
          val loc: String = http.getHeaderField("Location")
          var target: URL = null
          if (loc != null) {
            val decodedURL = URLDecoder.decode(loc,"utf-8")
            target = new URL(base, loc)
            log.info("Got redirect location %s, following %s" format (decodedURL, target.toString))
          }
          val cookies = if (http.getHeaderFields.containsKey("Set-Cookie")) {
            http.getHeaderFields.get("Set-Cookie").toList
          } else Nil
          cookies.foreach{ cookie: String =>
            val spacer = if (cookieData != "") "; "  else ""
            cookieData = cookieData + spacer + cookie.split(";")(0)
            log.info("new cookie data %s" format cookieData)
          }
          http.disconnect
          if (target == null || !((target.getProtocol == "http") || (target.getProtocol == "https")) || redirects >= 5) {
            throw new SecurityException("illegal URL redirect")
          }
          redir = true
          c = target.openConnection
          redirects += 1
        }
      }
    } while (redir)
    in
  }

  def makeConnection(url: URL): InputStream = {
    val conn: URLConnection = url.openConnection
    openConnectionCheckRedirects(conn)
  }

}