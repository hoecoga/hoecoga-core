package hoecoga.core

object EnumCodeGenerator {
  case class Enumerator[A](name: String, value: A) {
    private[EnumCodeGenerator] def code(enumClass: String): String = {
      val p = if (value.isInstanceOf[String]) s""""$value"""" else value
      s"case object $name extends $enumClass($p)"
    }
  }

  def code[A](pack: String, enumClass: String, aClass: String, enumerators: Enumerator[A]*): String = {
    s"""package $pack
        |
        |import hoecoga.core._
        |
        |sealed abstract class $enumClass(value: $aClass) extends EnumValue[$aClass](value)
        |
        |object $enumClass extends EnumCompanion[$aClass, $enumClass] {
        |  ${enumerators.map(_.code(enumClass)).mkString("\n  ")}
        |
        |  override val values: Set[$enumClass] = Set(${enumerators.map(_.name).mkString(", ")})
        |}
        |""".stripMargin
  }

  def intCode(pack: String, enumClass: String, names: String*): String = {
    code[Int](pack, enumClass, "Int", names.zipWithIndex.map {
      case (name, value) => Enumerator(name, value)
    }: _*)
  }

  def stringCode(pack: String, enumClass: String, names: String*): String = {
    code[String](pack, enumClass, "String", names.map(name => Enumerator(name, name.toLowerCase)): _*)
  }
}
