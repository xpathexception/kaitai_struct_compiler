package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{EnumType, IntType}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.KotlinCompiler
import io.kaitai.struct.{ImportList, Utils}

class KotlinTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  override def doIntLiteral(n: BigInt): String = {
    // TODO: Convert real big numbers to BigInteger
    val literal = if (n > Long.MaxValue && n <= Utils.MAX_UINT64) {
      "0x" + n.toString(16)
    } else {
      n.toString
    }
    val suffix = if (n < Int.MinValue || n > Int.MaxValue) "L" else ""

    s"$literal$suffix"
  }

  override def doArrayLiteral(t: DataType, value: Seq[expr]): String = {
    val kotlinType = KotlinCompiler.kaitaiType2KotlinType(t)
    val commaStr = value.map(v => translate(v)).mkString(", ")

    s"arrayListOf<$kotlinType>($commaStr)"
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String = s"byteArrayOf(${arr.mkString(", ")})"

  override def doByteArrayNonLiteral(elts: Seq[expr]): String =
    s"byteArrayOf(${elts.map(translate).mkString(", ")})"

  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String = {
    s"if (${translate(condition)}) ${translate(ifTrue)} else ${translate(ifFalse)}"
  }

  override def doName(s: String) = s match {
    case Identifier.ITERATOR => "_it"
    case Identifier.ITERATOR2 => "_buf"
    case Identifier.SWITCH_ON => "on"
    case Identifier.INDEX => "i"
    case _ => s"${Utils.lowerCamelCase(s)}()"
  }

  override def doInternalName(id: Identifier): String = s"${KotlinCompiler.publicMemberName(id)}()"

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s"${enumClass(enumTypeAbs)}.${Utils.upperUnderscoreCase(label)}"

  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    s"${enumClass(enumTypeAbs)}.byId($id)"

  def enumClass(enumTypeAbs: List[String]): String = {
    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)
    enumTypeRel.map((x) => Utils.upperCamelCase(x)).mkString(".")
  }

  override def genericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr, extPrec: Int) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${KotlinCompiler.kstreamName}.mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.genericBinOp(left, op, right, extPrec)
    }
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = op match {
    case Ast.cmpop.Eq =>
      s"${translate(left)} == ${translate(right)}"
    case Ast.cmpop.NotEq =>
      s"${translate(left)} != ${translate(right)}"
    case _ =>
      s"${translate(left)} ${cmpOp(op)} ${translate(right)}"
  }

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

  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}.get(${translate(idx, METHOD_PRECEDENCE)}.toInt())"

  override def doCast(value: Ast.expr, typeName: DataType): String = typeName match {
    case _: IntType => s"(${translate(value)}).to${KotlinCompiler.kaitaiType2KotlinType(typeName)}()"
    case _ => s"(${translate(value)} as ${KotlinCompiler.kaitaiType2KotlinType(typeName)})"
  }

  override def strToInt(s: expr, base: expr): String =
    s"${translate(s)}.toLong(${translate(base)})"

  override def enumToInt(v: expr, et: EnumType): String = s"${translate(v)}.id"

  override def floatToInt(v: expr): String = s"${translate(v)}.plus(0).toInt()"

  override def intToStr(i: expr): String = s"(${translate(i)}).toString()"

  override def bytesToStr(bytesExpr: String, encoding: String): String = encoding match {
    case "UTF-8" =>
      s"$bytesExpr.decodeToString(throwOnInvalidSequence = true)"
    case _ =>
      // Kotlin Native supports only UTF-8 at the moment
      s"throw UnsupportedOperationException(\"Unimplemented encoding for bytesToStr: $encoding)"
  }

  override def bytesLength(b: Ast.expr): String = s"${translate(b, METHOD_PRECEDENCE)}.size"

  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container, METHOD_PRECEDENCE)}[${translate(idx)}]"

  override def bytesFirst(b: Ast.expr): String = s"${translate(b, METHOD_PRECEDENCE)}.first()"

  override def bytesLast(b: Ast.expr): String = s"${translate(b, METHOD_PRECEDENCE)}.last()"

  override def bytesMin(b: Ast.expr): String = s"${translate(b)}.min()"

  override def bytesMax(b: Ast.expr): String = s"${translate(b)}.max()"

  override def strLength(s: expr): String = s"${translate(s, METHOD_PRECEDENCE)}.length"

  override def strReverse(s: expr): String = s"${translate(s, METHOD_PRECEDENCE)}.reversed()"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String = s"${translate(a, METHOD_PRECEDENCE)}.first()"

  override def arrayLast(a: expr): String = s"${translate(a, METHOD_PRECEDENCE)}.last()"

  override def arraySize(a: expr): String = s"${translate(a, METHOD_PRECEDENCE)}.size"

  override def arrayMin(a: Ast.expr): String = s"${translate(a)}.min()"

  override def arrayMax(a: Ast.expr): String = s"${translate(a)}.max()"

  override def binOp(op: Ast.operator): String = op match {
    case Ast.operator.Add => "+"
    case Ast.operator.Sub => "-"
    case Ast.operator.Mult => "*"
    case Ast.operator.Div => "/"
    case Ast.operator.Mod => "%"
    case Ast.operator.BitAnd => "and"
    case Ast.operator.BitOr => "or"
    case Ast.operator.BitXor => "xor"
    case Ast.operator.LShift => "shl"
    case Ast.operator.RShift => "shr"
    case _ => super.binOp(op)
  }
}
