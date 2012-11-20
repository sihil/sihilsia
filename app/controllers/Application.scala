package controllers

import play.api._
import play.api.mvc._
import templates.Html
import org.joda.time.{ReadableInstant, DateTime, DateMidnight}
import io.Source
import java.net.URL
import org.joda.time.format.DateTimeFormat
import java.util.Locale
import java.security.MessageDigest

object Pdf {
  def apply(html: Html) = {
    Results.Ok(PDF.toBytes(html)).as("application/pdf")
  }
}

object Data {
  val url = "https://docs.google.com/spreadsheet/pub?key=0Al4Ude7rKp72dC0xcVkwdE5zN3dCd1NXWmlqQURReFE&single=true&gid=0&output=csv"
  val source = Source.fromURL(url)
}

case class PassportDetails(
  familyName: String,
  givenName: String,
  sex: String,
  placeOfBirth: String,
  dateOfBirth: DateMidnight,
  nationality: String,
  picture: URL,
  t: PassportType,
  dateOfIssue: DateMidnight = new DateMidnight()
) {
  val expiry = new DateMidnight(2012,11,25)

  val englishFormatter = DateTimeFormat.forPattern("dd MMM /%'s' yy")
  val shortFormatter = DateTimeFormat.forPattern("yyMMdd")
  val frenchMonthFormatter = DateTimeFormat.forPattern("MMM").withLocale(Locale.FRENCH)

  def englishFrenchDateFormat(date: ReadableInstant): String = {
    englishFormatter.print(date).format(frenchMonthFormatter.print(date).replace(".",""))
  }

  lazy val efDateOfBirth = englishFrenchDateFormat(dateOfBirth)
  lazy val efDateOfIssue = englishFrenchDateFormat(dateOfIssue)
  lazy val efDateOfExpiry = englishFrenchDateFormat(expiry)
  lazy val shortDateOfBirth = shortFormatter.print(dateOfBirth)
  lazy val shortExpiry = shortFormatter.print(expiry)

  lazy val hash = familyName + givenName + sex + placeOfBirth + nationality
  lazy val passportNumber = {
    val hashBytes = MessageDigest.getInstance("MD5").digest(hash.getBytes)
    hashBytes.take(9).map(_.toInt).map(math.abs).map(_ % 10).mkString("")
  }

  lazy val shortNationality = nationality.take(3)

  def check(digits: String): String = {
    val hashBytes = MessageDigest.getInstance("MD5").digest(digits.getBytes)
    val digit = hashBytes.take(1).map(_.toInt).map(math.abs).map(_ % 10).mkString("")
    "%s%s" format (digits, digit)
  }

  val machineSex = {
    if (sex == "Male") "M"
    else if (sex == "Female") "F"
    else "<"
  }

  def padMachineLine(line: String): String = "%s%s" format(line.take(44), "<"*(44-line.length))

  lazy val machineLine1 = {
    val line = "P<%s%s<<%s" format (t.issuingState, familyName, givenName.replace(" ","<"))
    padMachineLine(line)
  }
  lazy val machineLine2 = {
    val line = "%s%s%s%s%s" format (check(passportNumber),shortNationality,check(shortDateOfBirth),machineSex,check(shortExpiry))
    padMachineLine(line)
  }


}

case class PassportType(
  cssClass: String,
  title: String,
  issuingState: String
)
object PassportType {
  val siheria = PassportType("siheria", "", "SHR")
}

object Application extends Controller {

  val testPassport = PassportDetails(
    "Al-Saidi",
    "Cristi",
    "Female",
    "Siuda Arabia",
    new DateMidnight(1993,1,1),
    "Siuda Arabian",
    new URL("http://sphotos-h.ak.fbcdn.net/hphotos-ak-ash4/392106_124361957681404_995414998_n.jpg"),
    PassportType.siheria
  )


  def index = Action {
    val result = Ok(views.html.index("Your new application is ready."))
    result
  }

  def pdf = Action {
    Pdf(views.html.verybasic())
  }

  def passport = Action {
    Ok(views.html.passport(testPassport))
  }

  def passportPdf = Action {
    Pdf(views.html.passport(testPassport))
  }


}