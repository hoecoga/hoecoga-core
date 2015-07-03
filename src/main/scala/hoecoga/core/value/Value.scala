package hoecoga.core.value

import hoecoga.core.util.JsonUtil
import org.scalacheck.Arbitrary
import play.api.libs.json.Format

/**
 * @example {{{
 *           case class Sub(a: Int) extends Value[Int]
 * }}}
 */
trait Value[A] {
  val value: A
}

/**
 * @example {{{
 *           case class Sub(a: Int) extends Value[Int]
 *
 *           object Sub extends ValueCompanion[Int, Sub] {
 *             override protected val pf = {
 *               case a => Sub(a)
 *             }
 *           }
 * }}}
 */
abstract class ValueCompanion[A, B <: Value[A]](implicit f: Format[A]) extends (A => B) {
  protected val pf: PartialFunction[A, B]

  def either(a: A): Either[IllegalArgumentException, B] = pf.lift.apply(a).map(Right(_)).
    getOrElse(Left(new IllegalArgumentException(s"$a is not ${getClass.getSimpleName}")))

  protected[this] def unsafe(a: A): B = either(a).fold(a => throw a, b => b)

  implicit val format: Format[B] = JsonUtil.format(unsafe, _.value)

  implicit val arbitrary: Arbitrary[B]
}

trait IntValue extends Value[Int]

trait IntValueCompanion[A <: Value[Int]] extends ValueCompanion[Int, A]

trait LongValue extends Value[Long]

trait LongValueCompanion[A <: Value[Long]] extends ValueCompanion[Long, A]

trait StringValue extends Value[String]

trait StringValueCompanion[A <: Value[String]] extends ValueCompanion[String, A]
