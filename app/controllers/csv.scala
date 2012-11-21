package controllers

import au.com.bytecode.opencsv.CSVReader
import scala.collection.JavaConversions._
import java.net.URL
import java.io.{BufferedReader, InputStreamReader}

case class CSVData(url: URL, separator: Char = ',', quote: Char = '"', escape: Char = '\\' ) {

  val reader = new BufferedReader(new InputStreamReader(url.openStream()))
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

}