package hoecoga.core

/**
 * $ sbt console
 * scala> hoecoga.core.JsonUtilCodeGenerator.code("hoecoga.core")
 */
object JsonUtilCodeGenerator {
  private[this] def upper(index: Int): String = ('A' to 'Z')(index).toString

  private[this] def lower(index: Int): String = ('a' to 'z')(index).toString

  private[this] def types(size: Int): String = (0 to size).map(upper).mkString(", ")

  private[this] def path(index: Int): String = s"""${lower(index)}p"""

  private[this] def paths(size: Int): String = (0 to size).map(index => path(index) + ": String").mkString(", ")

  private[this] def args(size: Int): String =
    s"""(${paths(size - 1)}, apply: (${types(size - 1)}) => ${upper(size)}, unapply: ${upper(size)} => Option[(${types(size - 1)})])"""

  private[this] def fImplicit(index: Int) = s"""fm${lower(index)}: Format[${upper(index)}]"""

  private[this] def implicits(size: Int): String =
    s"""(implicit ${(0 to size).map(i => fImplicit(i)).mkString(", ")})"""

  private[this] def jsPath(index: Int): String = s"""(JsPath \\ ${path(index)}).format[${upper(index)}]"""

  private[this] def format(size: Int): String =
    s"""  def format[${types(size)}]${args(size)}${implicits(size - 1)}: Format[${upper(size)}] = {
       |    (${(0 to size - 1).map(i => jsPath(i)).mkString(" and ")})(apply, Function.unlift(unapply))
       |  }
     """.stripMargin

  private[this] val argc = 21

  def code(packageName: String): String =
    s"""package $packageName
       |
       |import play.api.data.validation.ValidationError
       |import play.api.libs.json._
       |import play.api.libs.functional.syntax._
       |
       |import scala.util.control.NonFatal
       |
       |trait JsonUtil {
       |  def format[A, B](f: A => B, g: B => A)(implicit fa: Format[A]): Format[B] = {
       |    def toJson(a: A): JsResult[B] = try {
       |      JsSuccess(f(a))
       |    } catch {
       |      case NonFatal(e) => JsError.apply(ValidationError.apply(e.getMessage))
       |    }
       |    Format[B](Reads[B](js => fa.reads(js).fold(JsError.apply, toJson)), Writes[B](b => fa.writes(g(b))))
       |  }
       |
       |  def format[A, B](ap: String, apply: A => B, unapply: B => Option[A])(implicit fa: Format[A]): Format[B] =
       |    (JsPath \\ ap).format[A].inmap(apply, Function.unlift(unapply))
       |
       |  ${(2 to argc).map(format).mkString("\n")}
       |}
       |
       |object JsonUtil extends JsonUtil
     """.stripMargin
}
