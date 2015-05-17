package hoecoga.core

import org.scalacheck.{Arbitrary, Gen}
import play.api.libs.json.Format

trait EnumCompanion[A, B <: EnumValue[A]] {
  val values: Set[B]

  def either(a: A): Either[IllegalArgumentException, B] = {
    values.find(_.value == a) match {
      case None => Left(new IllegalArgumentException(s"${this.getClass.getSimpleName} invalid argument: $a"))
      case Some(b) => Right(b)
    }
  }

  /**
   * @throws IllegalArgumentException if `a` is not in the enumerator values.
   */
  def apply(a: A): B = either(a).fold[B](e => throw e, b => b)

  implicit val arbitrary: Arbitrary[B] = Arbitrary(Gen.oneOf(values.toSeq))

  implicit def format(implicit f: Format[A]): Format[B] = JsonUtil.format(apply, _.value)
}
