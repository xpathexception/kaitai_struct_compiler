package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.languages.KotlinCompiler
import io.kaitai.struct.languages.KotlinCompiler.enumIdKotlinType

class KotlinTranslator(
  provider: TypeProvider,
  importList: ImportList
) extends BaseTranslator(provider)
  with MinSignedIntegers {

  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String = {
    s"(if (${translate(condition)}) ${translate(ifTrue)} else ${translate(ifFalse)})"
  }

  override def doName(s: String): String = s match {
    case Identifier.ITERATOR => "_it"
    case Identifier.ITERATOR2 => "_buf"
    case Identifier.SWITCH_ON => "on"
    case Identifier.INDEX => "i"
    case _ => KotlinCompiler.escapeHardKeyword(Utils.lowerCamelCase(s))
  }

  override def doInternalName(id: Identifier): String = {
    s"${KotlinCompiler.idToStr(id, escapeHard = true)}"
  }

  def enumClassNameFrom(name: List[String]): String = {
    val relativeName = Utils.relClass(name, provider.nowClass.name)
    KotlinCompiler.classNameFrom(relativeName)
  }

  override def doEnumByLabel(enumSpec: EnumSpec, label: String): String = {
    s"${enumClassNameFrom(enumSpec.name)}.${KotlinCompiler.valueName2Const(label)}"
  }

  override def doEnumById(enumSpec: EnumSpec, id: String): String = {
    s"${enumClassNameFrom(enumSpec.name)}.byId($id.to${enumIdKotlinType()}())"
  }

  override def floatToInt(value: Ast.expr): String = {
    s"${translate(value, METHOD_PRECEDENCE)}.plus(0).toInt()"
  }

  override def enumToInt(value: Ast.expr, et: DataType.EnumType): String = {
    s"${translate(value)}.id"
  }

  override def binOp(op: Ast.operator): String = op match {
    case Ast.operator.Add => "+"
    case Ast.operator.Sub => "-"
    case Ast.operator.Mult => "*"
    case Ast.operator.Div => "/"
    case Ast.operator.Mod => "%"
    case Ast.operator.BitAnd => {
      importList.add("io.kaitai.struct.typing.*")
      "and"
    }
    case Ast.operator.BitOr => "or"
    case Ast.operator.BitXor => "xor"
    case Ast.operator.LShift => {
      importList.add("io.kaitai.struct.typing.*")
      "shl"
    }
    case Ast.operator.RShift => {
      importList.add("io.kaitai.struct.typing.*")
      "shr"
    }
    case _ => super.binOp(op)
  }

  //region Literal

  override def doFloatLiteral(n: Any): String = {
    s"${super.doFloatLiteral(n)}f"
  }

  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): String = {
    val kotlinType = KotlinCompiler.kotlinTypeOf(t)
    val commaStr = value.map(v => translate(v)).mkString(", ")

    s"arrayListOf<$kotlinType>($commaStr)"
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String = {
    val args = arr.map {
      case b if b > 127 => s"$b.toByte()"
      case b if b < -128 => s"($b).toByte()"
      case b => s"$b"
    }.mkString(", ")

    s"byteArrayOf($args)"
  }

  //endregion Literal

  //region String

  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    s"${translate(s, METHOD_PRECEDENCE)}.toLong(${translate(base)})"
  }

  override def intToStr(value: Ast.expr): String = {
    s"${translate(value, METHOD_PRECEDENCE)}.toString()"
  }

  override def bytesToStr(bytesExpr: String, encoding: String): String = encoding match {
    case "UTF-8" =>
      s"$bytesExpr.decodeToString(throwOnInvalidSequence = true)"
    case _ =>
      // Kotlin Native supports only UTF-8 at the moment
      // Render error as astring template to allow implicit type inferring in some contexts
      s"\"$${error(\"Unimplemented encoding for bytesToStr: $encoding\")}\""
  }

  override def strLength(s: Ast.expr): String = {
    s"${translate(s, METHOD_PRECEDENCE)}.length"
  }

  override def strReverse(s: Ast.expr): String = {
    s"${translate(s, METHOD_PRECEDENCE)}.reversed()"
  }

  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String = {
    s"${translate(s, METHOD_PRECEDENCE)}.substring(${translate(from)}, ${translate(to)})"
  }

  //endregion String
  //region Array

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    op match {
      case Ast.cmpop.Eq =>
        s"${translate(left)}.contentEquals(${translate(right)})"
      case Ast.cmpop.NotEq =>
        s"!${translate(left)}.contentEquals(${translate(right)})"
      case _ =>
        s"(${KotlinCompiler.kstreamName}.byteArrayCompare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

  override def arraySize(a: Ast.expr): String = {
    s"${translate(a, METHOD_PRECEDENCE)}.size"
  }

  override def arrayFirst(a: Ast.expr): String = {
    s"${translate(a, METHOD_PRECEDENCE)}.first()"
  }

  override def arrayLast(a: Ast.expr): String = {
    s"${translate(a, METHOD_PRECEDENCE)}.last()"
  }

  override def arrayMin(a: Ast.expr): String = {
    s"${translate(a, METHOD_PRECEDENCE)}.min()"
  }

  override def arrayMax(a: Ast.expr): String = {
    s"${translate(a, METHOD_PRECEDENCE)}.max()"
  }

  override def arraySubscript(container: Ast.expr, idx: Ast.expr): String = {
    s"${translate(container, METHOD_PRECEDENCE)}[${translate(idx)}]"
  }

  //endregion Array
}
