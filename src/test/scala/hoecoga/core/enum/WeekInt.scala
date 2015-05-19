package hoecoga.core.enum

import hoecoga.core._

sealed abstract class WeekInt(value: Int) extends EnumValue[Int](value)

object WeekInt extends EnumCompanion[Int, WeekInt] {
  case object Sunday extends WeekInt(0)
  case object Monday extends WeekInt(1)
  case object Tuesday extends WeekInt(2)
  case object Wednesday extends WeekInt(3)
  case object Thursday extends WeekInt(4)
  case object Friday extends WeekInt(5)
  case object Saturday extends WeekInt(6)

  override val values: Set[WeekInt] = Set(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
}

/*
val week = Seq("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
EnumCodeGenerator.intCode("hoecoga.core.enum", "WeekInt", week: _*)
 */
