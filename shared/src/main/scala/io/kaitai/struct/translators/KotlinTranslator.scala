package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr.IntNum
import io.kaitai.struct.exprlang.Ast.{expr, operator}
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.languages.KotlinCompiler
import io.kaitai.struct.languages.KotlinCompiler.{enumIdKotlinType, kotlinTypeOf}
import io.kaitai.struct.precompile.TypeMismatchError
import io.kaitai.struct.{ImportList, Utils}

class KotlinTranslator(
  provider: TypeProvider,
  importList: ImportList
) extends BaseTranslator(provider)
  with MinSignedIntegers {

  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String = {
    s"(if (${translate(condition)}) ${translate(ifTrue)} else ${translate(ifFalse)})"
  }

  override def doCast(value: expr, typeName: DataType): String = {
    (detectType(value), typeName) match {
      case (l: NumericType, r: NumericType) => {
        if (value.isInstanceOf[IntNum]) {
          s"${translate(value, METHOD_PRECEDENCE)}${castExpression(r)}"
        } else if (l == r) {
          translate(value, METHOD_PRECEDENCE)
        } else {
          s"${translate(value, METHOD_PRECEDENCE)}${castExpression(r)}"
        }
      }
      case (l: EnumType, r: EnumType) => translate(value, METHOD_PRECEDENCE)
      case _ => s"(${translate(value, METHOD_PRECEDENCE)} as ${kotlinTypeOf(typeName)}) /* generic cast */"
    }
  }

  def castExpression(t: NumericType): String = {
    t match {
      case int1: Int1Type => if (int1.signed) ".toIntS1()" else ".toIntU1()"
      case intMulti: IntMultiType => {
        val sign = if (intMulti.signed) "S" else "U"
        val width = s"${intMulti.width.width}"

        s".toInt$sign$width()"
      }
      case CalcIntType => ".toIntC()"
      case CalcFloatType => ".toFloatC()"
      case BitsType(_, _) => ".toIntS8()"
      case FloatMultiType(Width4, _) => ".toFloat()"
      case FloatMultiType(Width8, _) => ".toDouble()"
      case _ => s"/* todo: cast numeric to ${t} */"
    }
  }

  override def detectTypeRaw(v: expr): DataType = {
    v match {
      case Ast.expr.IntNum(n) => {
        if (n >= -128 && n <= 127) {
          Int1Type(true)
        } else if (n > 127 && n <= 255) {
          Int1Type(false)
        } else if (n > Long.MaxValue && n <= Utils.MAX_UINT64) {
          IntMultiType(signed = false, Width8, None)
        } else {
          IntMultiType(signed = true, Width4, None)
        }
      }
      case Ast.expr.UnaryOp(op: Ast.unaryop, v: Ast.expr) => {
        val t = detectType(v)
        (t, op) match {
          case (IntMultiType(_, _, _), Ast.unaryop.Minus | Ast.unaryop.Invert) => t
          case (t: IntType, Ast.unaryop.Minus | Ast.unaryop.Invert) => {
            val size = t match {
              case Int1Type(signed) => 1
              case IntMultiType(signed, width, endian) => width.width
              case BitsType(width, bitEndian) => width
            }

            if (size <= 4) {
              IntMultiType(signed = true, Width4, None)
            } else {
              IntMultiType(signed = true, Width8, None)
            }
          }
          case (_: FloatType, Ast.unaryop.Minus) => t
          case (_: BooleanType, Ast.unaryop.Not) => t
          case _ => throw new TypeMismatchError(s"unable to apply unary operator $op to $t")
        }
      }
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) => {
        (detectType(left), detectType(right), op) match {
          case (ltype: IntType, rtype: IntType, _) => calculateBinaryOpType(ltype, rtype, op)
          case (ltype: NumericType, rtype: NumericType, _) => ltype
          case (_: StrType, _: StrType, Ast.operator.Add) => CalcStrType
          case (ltype, rtype, _) =>
            throw new TypeMismatchError(s"can't apply operator $op to $ltype and $rtype")
        }
      }

      case _ => super.detectTypeRaw(v)
    }
  }

  def calculateBinaryOpType(ltype: IntType, rtype: IntType, op: Ast.operator): IntType = {
    if (ltype == CalcIntType || rtype == CalcIntType) return CalcIntType

    val lsize = ltype match {
      case Int1Type(signed) => 1
      case IntMultiType(signed, width, endian) => width.width
      case BitsType(width, bitEndian) => width
    }

    val rsize = ltype match {
      case Int1Type(signed) => 1
      case IntMultiType(signed, width, endian) => width.width
      case BitsType(width, bitEndian) => width
    }

    val smallTypes = lsize < 4 && rsize < 4
    val has8Type = lsize == 8 || rsize == 8

    op match {
      case operator.Add | operator.Sub | operator.Mult | operator.Div | operator.Mod => {
        if (smallTypes) {
          IntMultiType(signed = false, Width4, None)
        } else if (has8Type) {
          IntMultiType(signed = false, Width8, None)
        } else {
          IntMultiType(signed = false, Width4, None)
        }
      }
      case operator.LShift | operator.RShift | operator.BitOr | operator.BitXor | operator.BitAnd => {
        ltype
      }
    }
  }

  override def doName(s: String): String = s match {
    case Identifier.ITERATOR => "_it"
    case Identifier.ITERATOR2 => "_buf"
    case Identifier.SWITCH_ON => "on"
    case Identifier.INDEX => "i"
    case _ => s"${KotlinCompiler.escapeHardKeyword(Utils.lowerCamelCase(s))}()"
  }

  override def doNumericCompareOp(left: expr, op: Ast.cmpop, right: expr): String = {
    (detectType(left), detectType(right)) match {
      case (ltype: NumericType, rtype: NumericType) => {
        val ctype = TypeDetector.combineTypes(ltype, rtype)
        s"${doCast(left, ctype)} ${cmpOp(op)} ${doCast(right, ctype)} /* nmcmp $ltype ~ $rtype */"
      }
      case _ => super.doNumericCompareOp(left, op, right) + "/* nmcmp-2 */"
    }
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

  override def strLiteralGenericCC(code: Char): String = {
    strLiteralUnicode(code)
  }

  override def strLiteralUnicode(code: Char): String = {
    "\\u%04x".format(code.toInt)
  }

  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    '\b' -> "\\b",
    '\'' -> "\\\'",
    '$' -> "\\$",
  )

  override def doIntLiteral(n: BigInt): String = {
    if (n > Long.MaxValue && n <= Utils.MAX_UINT64) {
      n.toString + "U"
    }
//    else if (n < 0) {
//      s"(${super.doIntLiteral(n)})"
//    }
    else {
      super.doIntLiteral(n)
    }
  }

  override def doFloatLiteral(n: Any): String = {
    s"${super.doFloatLiteral(n)}"
  }

  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): String = {
    val kotlinType = KotlinCompiler.kotlinTypeOf(t)
    val commaStr = value.map(v => translate(v)).mkString(", ")

    s"arrayListOf($commaStr)"
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
