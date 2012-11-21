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

trait Logging {
  implicit val log = Logger(getClass)
}

object Data {
  val url = new URL("https://docs.google.com/spreadsheet/pub?key=0Al4Ude7rKp72dC0xcVkwdE5zN3dCd1NXWmlqQURReFE&single=true&gid=0&output=csv")
  //val url = new URL("file:///Users/shildrew/Downloads/SihilsiaPassportAgency.csv")
  def withSource[T](block: Source => T): T = {
    val source = Source.fromURL(url)
    try block(source)
    finally source.close
  }
}

case class PassportDetails(
  applicationId: Int,
  familyName: String,
  givenName: String,
  sex: String,
  placeOfBirth: String,
  dateOfBirth: ReadableInstant,
  nationality: String,
  picture: Option[URL],
  t: PassportType,
  email: String,
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
    val line = ("P<%s%s<<%s" format (t.issuingState, familyName, givenName)).replace(" ","<")
    padMachineLine(line)
  }
  lazy val machineLine2 = {
    val line = "%s%s%s%s%s" format (check(passportNumber),shortNationality,check(shortDateOfBirth),machineSex,check(shortExpiry))
    padMachineLine(line)
  }

  lazy val errors: Option[String] = if (validPhoto) None else Some("Photo not valid")

  lazy val validPhoto: Boolean = {
    picture.map{ url =>
      Pdf.image(url).map { image =>
        image.getWidth != 0.0
      }.getOrElse(false)
    }.getOrElse(false)
  }
}

trait CsvKeys {
  val FAMILY_NAME = "Family Name"
  val GIVEN_NAMES = "Given names"
  val SEX = "Sex"
  val PLACE_OF_BIRTH = "Place of birth"
  val PASSPORT_TYPE = "For which Sihilsian country do you want to apply to for a passport?"
  val PHOTO_URL = "Photo URL"
  val DATE_OF_BIRTH = "Date of birth (DD/MM/YYYY)"
  val NATIONALITY = "Nationality"
  val EMAIL = "e-mail address"
  val ADDRESS = "postal address"
}

object PassportDetails extends Logging with CsvKeys {
  val dobParser = DateTimeFormat.forPattern("dd/MM/yyyy")

  var allPassportApplications: Seq[PassportDetails] = parseLivePassportApplications

  def parseLivePassportApplications: Seq[PassportDetails] = {
    val csvRows = CSVData(Data.url).getRows
    log.info(csvRows.length.toString())
    csvRows.zipWithIndex.map(entry => PassportDetails(entry._2+2, entry._1))
  }

  def apply(id: Int): Option[PassportDetails] = {
    allPassportApplications.find(_.applicationId == id)
  }

  def reloadApplications() {
    allPassportApplications = parseLivePassportApplications
  }

  def urlOption(url: String): Option[URL] = {
    try {
      Some(new URL(url))
    } catch {
      case e:Throwable => None
    }
  }
  def apply(id: Int, csvLine: Map[String,String]): PassportDetails = {
    log.info(csvLine.toString())
    val dob = dobParser.parseDateTime(csvLine(DATE_OF_BIRTH))
    PassportDetails(
      id,
      csvLine(FAMILY_NAME),
      csvLine(GIVEN_NAMES),
      csvLine(SEX),
      csvLine(PLACE_OF_BIRTH),
      dob,
      csvLine(NATIONALITY),
      urlOption(csvLine(PHOTO_URL)),
      PassportType(csvLine(PASSPORT_TYPE),csvLine),
      csvLine(EMAIL)
    )
  }
}

case class PassportType(
  cssClass: String,
  title: String,
  issuingState: String,
  csvName: String
)
object PassportType extends CsvKeys {
  val siheria = PassportType("siheria", "Siheria", "SHR", "Siheria")
  val siuda = PassportType("siuda-arabia", "Kingdom of Siuda Arabia", "SAB", "Siuda Arabia")
  val siychelle = PassportType("siychelle", "République des Seychelles", "SIY", "Siychelle")
  val types = Seq(siheria, siuda, siychelle)

  def randomPassport(context:Map[String,String]): PassportType = {
    lazy val hash = Seq(FAMILY_NAME,GIVEN_NAMES,SEX,PLACE_OF_BIRTH,NATIONALITY).map(context).mkString
    lazy val index = {
      val hashBytes = MessageDigest.getInstance("MD5").digest(hash.getBytes)
      math.abs(hashBytes.head.toInt) % 3
    }
    types(index)
  }

  def apply(name: String, csvRecord:Map[String,String]): PassportType = {
    types.find(_.csvName == name).getOrElse(randomPassport(csvRecord))
  }
}

object Application extends Controller with Logging {

  val testPassport = PassportDetails(
    500,
    "Al-Saidi",
    "Cristi",
    "Female",
    "Siuda Arabia",
    new DateMidnight(1993,1,1),
    "Siuda Arabian",
    Some(new URL("http://sphotos-h.ak.fbcdn.net/hphotos-ak-ash4/392106_124361957681404_995414998_n.jpg")),
    PassportType.siuda,
    "email@email.com"
  )

  def dumpApplications = Action {
    Data.withSource { source =>
      Ok(views.html.dumpApplications(source.getLines()))
    }
  }

  def applicationsList = Action {
    Ok(views.html.detailsList(PassportDetails.allPassportApplications))
  }

  def index = Action {
    val result = Ok(views.html.index("Your new application is ready."))
    result
  }

  def pdf = Action {
    Pdf(views.html.verybasic())
  }

  def passport(id: Int) = Action {
    Ok(views.html.passport(PassportDetails(id).get))
  }

  def passportPdf(id: Int) = Action {
    Pdf(views.html.passport(PassportDetails(id).get))
  }

}