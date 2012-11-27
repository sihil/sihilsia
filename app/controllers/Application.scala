package controllers

import play.api._
import play.api.mvc._
import org.joda.time.{ReadableInstant, DateMidnight}
import io.Source
import java.net.URL
import org.joda.time.format.DateTimeFormat
import java.util.Locale
import java.security.MessageDigest
import templates.Html
import java.io.{FileOutputStream, File}
import sbt.Process._

trait Logging {
  implicit val log = Logger(getClass)
}

trait MenuItem {
  def title:String
  def target:Call
  def isActive(request:Request[AnyContent]):Boolean
}

case class SingleMenuItem(title: String, target: Call, activeInSubPaths: Boolean = false, enabled: Boolean = true) extends MenuItem{
  def isActive(request: Request[AnyContent]): Boolean = {
    activeInSubPaths && request.path.startsWith(target.url) || request.path == target.url
  }
}

case class DropDownMenuItem(title:String, items: Seq[SingleMenuItem], target: Call = Call("GET", "#")) extends MenuItem {
  def isActive(request: Request[AnyContent]) = items.exists(_.isActive(request))
}

object Menu {
  lazy val menuItems = Seq(
    SingleMenuItem("Application List", routes.Application.applicationsList(showAll = false)),
    SingleMenuItem("Application List (ALL)", routes.Application.applicationsList(showAll = true)),
    SingleMenuItem("Reload from Google Sheet", routes.Application.reloadApplications())
  )
}


object Data {
  //val url = new URL("https://docs.google.com/spreadsheet/pub?key=0Al4Ude7rKp72dC0xcVkwdE5zN3dCd1NXWmlqQURReFE&single=true&gid=0&output=csv")
  val url = new URL("https://docs.google.com/a/hildrew.net/spreadsheet/ccc?key=0Al4Ude7rKp72dC0xcVkwdE5zN3dCd1NXWmlqQURReFE&output=csv")
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
  dateOfBirthRaw: String,
  nationality: String,
  picture: Option[URL],
  t: PassportType,
  email: String,
  dateOfIssue: DateMidnight = new DateMidnight()
) {
  lazy val dateOfBirth: Option[ReadableInstant] = try {
    Some(PassportDetails.dobParser.parseDateTime(dateOfBirthRaw))
  } catch {
    case e: Throwable => None
  }

  val expiry = new DateMidnight(2012,11,25)

  val shortFormatter = DateTimeFormat.forPattern("yyMMdd")

  lazy val dateOfBirthString = t.dateRenderer.print(dateOfBirth.get)
  lazy val dateOfIssueString = t.dateRenderer.print(dateOfIssue)
  lazy val dateOfExpiryString = t.dateRenderer.print(expiry)
  lazy val shortDateOfBirth = shortFormatter.print(dateOfBirth.get)
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

  lazy val errors: List[String] = {
    val photoError = if (validPhoto) None else Some("Photo not valid")
    val dobError = if (dateOfBirth.isEmpty) Some("DOB not valid") else None
    List(photoError, dobError).filter(_.isDefined).map(_.get)
  }

  lazy val validPhoto: Boolean = {
    picture.map{ url =>
      Pdf.cachedImage(url).map { image =>
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
    PassportDetails(
      id,
      csvLine(FAMILY_NAME),
      csvLine(GIVEN_NAMES),
      csvLine(SEX),
      csvLine(PLACE_OF_BIRTH),
      csvLine(DATE_OF_BIRTH),
      csvLine(NATIONALITY),
      urlOption(csvLine(PHOTO_URL)),
      PassportType(csvLine(PASSPORT_TYPE),csvLine),
      csvLine(EMAIL)
    )
  }
}

case class PassportType(
  cssClass: String,
  title: Html,
  issuingState: String,
  csvName: String,
  header: Map[String,Html],
  dateRenderer: DateRenderer
)

trait DateRenderer {
  def print(date: ReadableInstant): String
}

object PassportType extends CsvKeys {
  val englishFrenchHeaders = Map(
    "type" -> Html("Type/Type"),
    "passportNumber" -> Html("Passport No./Passeport No."),
    "issuingState" -> Html("Code of Issuing State/Code de l'Etar émetteur"),
    "familyName" -> Html("Surname/Nom (1)"),
    "givenName" -> Html("Given names/Prénoms (2)"),
    "nationality" -> Html("Nationality/Nationalité (3)"),
    "dateOfBirth" -> Html("Date of birth/Date de naissance (4)"),
    "sex" -> Html("Sex/Sexe (5)"),
    "placeOfBirth" -> Html("Place of birth/Lieu de naissance (6)"),
    "dateOfIssue" -> Html("Date of issue/Date de délivrance (7)"),
    "authority" -> Html("Authority/Authorité (8)"),
    "dateOfExpiry" -> Html("Date of expiry/Date d'expiration (9)")
  )

  def header(english: String, headerImage: String): Html = {
    Html("%s <img src='/public/images/headers/arabic_%s.png' class='arabic'/>" format (english, headerImage))
  }

  val arabicEnglishHeaders = Map(
    "type" -> header("Type", "type"),
    "passportNumber" -> header("Passport No.", "passportNumber"),
    "issuingState" -> header("Code of Issuing State", "issuingState"),
    "familyName" -> header("Family name", "familyName"),
    "givenName" -> header("Given names", "givenName"),
    "nationality" -> header("Nationality", "nationality"),
    "dateOfBirth" -> header("Date of birth", "dateOfBirth"),
    "sex" -> header("Sex", "sex"),
    "placeOfBirth" -> header("Place of birth", "placeOfBirth"),
    "dateOfIssue" -> header("Date of issue", "dateOfIssue"),
    "authority" -> header("Authority", "authority"),
    "dateOfExpiry" -> header("Date of expiry", "dateOfExpiry")
  )

  val englishFormatter = DateTimeFormat.forPattern("dd MMM yy")
  val englishAndFrenchFormatter = DateTimeFormat.forPattern("dd MMM /%'s' yy")
  val frenchMonthFormatter = DateTimeFormat.forPattern("MMM").withLocale(Locale.FRENCH)

  val englishFrenchRenderer = new DateRenderer {
    def print(date: ReadableInstant): String = {
      englishAndFrenchFormatter.print(date).format(frenchMonthFormatter.print(date).replace(".",""))
    }
  }
  val englishRenderer = new DateRenderer {
    def print(date: ReadableInstant): String = englishFormatter.print(date)
  }

  val siheria = PassportType("siheria", Html("Siheria"), "SHR", "Siheria", englishFrenchHeaders, englishFrenchRenderer)
  val siuda = PassportType("siuda-arabia", Html("Kingdom of Siuda Arabia <img src='/public/images/headers/arabic_kingdom_of_siuda_arabia.png' class='arabic-title'/>"), "SAB", "Siuda Arabia", arabicEnglishHeaders, englishRenderer)
  val siychelle = PassportType("siychelle", Html("République de Siychelle"), "SIY", "Siychelle", englishFrenchHeaders, englishFrenchRenderer)
  val types = Seq(siheria, siuda, siychelle)

  def randomPassport(context:Map[String,String]): PassportType = {
    lazy val hash = Seq(FAMILY_NAME,GIVEN_NAMES,SEX,PLACE_OF_BIRTH,NATIONALITY).map(context).mkString
    lazy val index = {
      val hashBytes = MessageDigest.getInstance("MD5").digest(hash.getBytes)
      math.abs(hashBytes.head.toInt) % 2
    }
    types(index)
  }

  def apply(name: String, csvRecord:Map[String,String]): PassportType = {
    types.find(_.csvName == name).getOrElse(randomPassport(csvRecord))
  }
}

object Application extends Controller with Logging {

  val HIDDEN = "hidden"
  implicit def session2hidden(session: Session) = new {
    def hidden: Set[Int] = session.get(HIDDEN).map(_.split(',').filterNot(_ == "").map(_.toInt).toSet).getOrElse(Set())
    def hide(id: Int):Session = session + (HIDDEN -> (hidden + id).mkString(","))
    def show(id: Int):Session = session + (HIDDEN -> (hidden - id).mkString(","))
  }

  val testPassport = PassportDetails(
    500,
    "Al-Saidi",
    "Cristi",
    "Female",
    "Siuda Arabia",
    "01/01/1993",
    "Siuda Arabian",
    Some(new URL("http://sphotos-h.ak.fbcdn.net/hphotos-ak-ash4/392106_124361957681404_995414998_n.jpg")),
    PassportType.siuda,
    "email@email.com"
  )

  def dumpApplications = Action { request =>
    Data.withSource { source =>
      Ok(views.html.dumpApplications(request, source.getLines()))
    }
  }

  def applicationsList(showAll: Boolean) = Action { implicit request =>
    val hidden = session.hidden
    val passports = PassportDetails.allPassportApplications.filterNot(app => hidden.contains(app.applicationId) && !showAll)
    Ok(views.html.detailsList(request, passports, hidden))
  }

  def reloadApplications = Action {
    PassportDetails.reloadApplications()
    Redirect(routes.Application.applicationsList(false))
  }

  def index = Action { request =>
    Ok(views.html.index(request, "Welcome to the SPA site"))
  }

  def hide(id: Int) = Action { implicit request =>
    Redirect(routes.Application.applicationsList(false)).withSession{
      session.hide(id)
    }
  }

  def show(id: Int) = Action { implicit request =>
    Redirect(routes.Application.applicationsList(true)).withSession {
      session.show(id)
    }
  }

  def pdf = Action {
    Pdf(views.html.verybasic())
  }

  def print(id: Int) = Action {
    val pdf = Pdf.toBytes(views.html.passport(PassportDetails(id).get))
    val file = File.createTempFile("passport", ".pdf")
    val fos = new FileOutputStream(file)
    fos.write(pdf)
    fos.close()
    val command = "/usr/bin/lp -d Canon_iP4200 -o media=A5 %s" format file.getAbsolutePath
    log.info("Executing print command %s" format command)
    command.!!
    Redirect(routes.Application.hide(id))
  }

  def passport(id: Int) = Action {
    Ok(views.html.passport(PassportDetails(id).get))
  }

  def passportPdf(id: Int) = Action {
    Pdf(views.html.passport(PassportDetails(id).get))
  }

}