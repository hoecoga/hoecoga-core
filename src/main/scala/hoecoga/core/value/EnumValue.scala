package hoecoga.core.value

import org.scalacheck.{Arbitrary, Gen}

/**
 * @example {{{
 *           sealed abstract class Sub(val value: Int) extends EnumValue[Int]
 * }}}
 */
trait EnumValue[A] extends Value[A]

/**
 * @example {{{
 *           object Sub extends EnumValueCompanion[Int, Sub] {
 *             case object Enum1 extends Sub(1)
 *             case object Enum2 extends Sub(2)
 *
 *             override val values: Set[Sub] = Set(Enum1, Enum2)
 *           }
 * }}}
 */
trait EnumValueCompanion[A, B <: EnumValue[A]] extends ValueCompanion[A, B] {
  val values: Set[B]

  override protected val pf: PartialFunction[A, B] = {
    case a if values.exists(_.value == a) => values.find(_.value == a).get
  }

  /**
   * @see [[ValueCompanion.unsafe]]
   */
  override def apply(a: A): B = unsafe(a)

  override implicit val arbitrary: Arbitrary[B] = Arbitrary(Gen.oneOf(values.toSeq))
}
