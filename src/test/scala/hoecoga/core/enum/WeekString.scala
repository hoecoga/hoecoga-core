package hoecoga.core.enum

import hoecoga.core._

sealed abstract class WeekString(value: String) extends EnumValue[String](value)

object WeekString extends EnumCompanion[String, WeekString] {
  case object Sunday extends WeekString("sunday")
  case object Monday extends WeekString("monday")
  case object Tuesday extends WeekString("tuesday")
  case object Wednesday extends WeekString("wednesday")
  case object Thursday extends WeekString("thursday")
  case object Friday extends WeekString("friday")
  case object Saturday extends WeekString("saturday")

  override val values: Set[WeekString] = Set(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
}

/*
val week = Seq("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
EnumCodeGenerator.stringCode("hoecoga.core.enum", "WeekString", week: _*)
 */
