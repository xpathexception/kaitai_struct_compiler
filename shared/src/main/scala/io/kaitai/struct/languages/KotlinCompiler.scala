package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, ConversionError, DataType, EndOfStreamError, InheritedEndian, KSError}
import io.kaitai.struct.format._
import io.kaitai.struct.languages.KotlinCompiler.{kotlinTypeOf, kstructName}
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.KotlinTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

import scala.util.matching.Regex

class KotlinCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with FixedContentsUsingArrayByteLiteral
    with NoNeedForFullClassPath
    with ObjectOrientedLanguage
    with SingleOutputFile
    with UniversalDoc
    with UniversalFooter
    with UpperCamelCaseClasses {
  override val translator = new KotlinTranslator(typeProvider, importList)

  //region Debug

  def printDebugLogs: Boolean = true

  private def getDebugMethodName(): String = {
    val stackTrace = Thread.currentThread().getStackTrace
    if (printDebugLogs) s"/* ${stackTrace(2).getMethodName} */" else ""
  }

  private def printDebugMethodName(): Unit = {
    if (!printDebugLogs) return

    val pattern: Regex = """\$[^$]*\$([^$]*)\$.*""".r
    val stackTrace = Thread.currentThread().getStackTrace
    val name = stackTrace(2).getMethodName

    def result = name match {
      case pattern(extracted) => extracted
      case _ => name
    }

    out.puts(s"/* $result */")
  }

  //endregion Debug

  //region Naming

  override def idToStr(id: Identifier): String = KotlinCompiler.idToStr(id, escapeHard = false)

  override def paramName(id: Identifier): String = KotlinCompiler.idToStr(id, escapeHard = true)

  override def publicMemberName(id: Identifier): String = paramName(id)

  override def privateMemberName(id: Identifier): String = s"this.${paramName(id)}"

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError =>
      importList.add(config.kotlin.eosExceptionImport)
      config.kotlin.eosException
    case ConversionError =>
      "NumberFormatException"
    case _ =>
      s"KaitaiStream.${err.name}"
  }

  //endregion Naming

  //region Structure

  override def indent: String = "    "

  def isNested: Boolean = out.indentLevel > 0

  def treeClass(isNested: Boolean): String = {
    if (isNested) {
      importList.add(s"io.kaitai.struct.${KotlinCompiler.kstructTreeChildName}")
      KotlinCompiler.kstructTreeChildName
    } else {
      importList.add(s"io.kaitai.struct.${KotlinCompiler.kstructTreeRootName}")
      KotlinCompiler.kstructTreeRootName
    }
  }

  override def outFileName(topClassName: String): String = {
    s"${config.kotlin.kotlinPackage.replace('.', '/')}/${type2class(topClassName)}_2.kt"
  }

  override def outImports(topClass: ClassSpec): String = {
    "\n" + importList.toList.sorted.map((x) => s"import $x").mkString("\n") + "\n"
  }

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")

    importList.add("io.kaitai.struct.typing.*")
    importList.add(s"io.kaitai.struct.${KotlinCompiler.kstructName}")
    importList.add(s"io.kaitai.struct.${KotlinCompiler.kstreamName}")
    importList.add(s"kotlin.jvm.JvmOverloads")

    if (config.kotlin.kotlinPackage.nonEmpty) {
      outHeader.puts
      outHeader.puts(s"package ${config.kotlin.kotlinPackage}")
    }

    out.puts
  }

  override def classHeader(name: String): Unit = {
    printDebugMethodName()
  }

  override def classConstructorHeader(
    name: String,
    parentType: DataType,
    rootClassName: String,
    isHybrid: Boolean,
    params: List[ParamDefSpec]
  ): Unit = {
    printDebugMethodName()

    out.puts(s"class ${type2class(name)} @JvmOverloads constructor(")
    out.inc

    out.puts(s"${Identifier.IO}: ${kotlinTypeOf(typeProvider.determineType(Identifier.IO))},")
    out.puts(s"override val ${Identifier.PARENT}: ${kotlinTypeOf(parentType)}? = null,")

    val overrideRoot = if (typeProvider.nowClass.isTopLevel) "" else "override val "
    out.puts(s"${overrideRoot}${Identifier.ROOT}: ${type2class(rootClassName)}? = null,")

    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) => {
        out.puts(s"private var ${Identifier.IS_LE}: Boolean? = null,")
      }
      case _ =>
    }

    for (p <- params) {
      if (printDebugLogs) out.puts(s"/* $p */")
      out.puts(s"${paramName(p.id)}: ${kotlinTypeOf(p.dataType)},")
    }

    out.dec
    out.puts(s") : $kstructName(${Identifier.IO}), ${treeClass(isNested)}<${type2class(rootClassName)}> {")
    out.inc

    if (!isHybrid && typeProvider.nowClass.isTopLevel) {
      out.puts(s"override val _root: ${type2class(rootClassName)} = _root ?: this")
    }
  }

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    out.puts("/**")

    doc.summary.foreach(summary => out.putsLines(" * ", summary))

    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(" * ", s"@see $text")
      case ref: UrlRef =>
        out.putsLines(" * ", s"[${ref.text}](${ref.url})")
    }

    out.puts(" */")
  }

  override def universalFooter: Unit = {
    printDebugMethodName()
    out.dec
    out.puts("}")
    out.puts
  }

  def companionObject(): Unit = {
    val isInheritedEndian = typeProvider.nowClass.meta.endian match {
      case Some(InheritedEndian) => true
      case _ => false
    }

    val fromFileClass: String = {
      val pos = config.kotlin.fromFileClass.lastIndexOf('.')
      if (pos < 0) {
        // If relative "fromFileClass", then just use it as is
        config.kotlin.fromFileClass
      } else {
        // If absolute "fromFileClass", add relevant import + use relative
        importList.add(config.kotlin.fromFileClass)
        config.kotlin.fromFileClass.substring(pos + 1)
      }
    }

    // fromFile helper makes no sense for inherited endianness structures:
    // they require endianness to be parsed anyway
    if (
      !isInheritedEndian &&
        config.kotlin.fromFileClass.nonEmpty &&
        typeProvider.nowClass.params.isEmpty
    ) {
      val className = type2class(typeProvider.nowClass.name.last)

      out.puts(s"companion object {")
      out.inc

      out.puts(s"fun fromFile(fileName: String): $className {")
      out.inc
      out.puts(s"return $className($fromFileClass(fileName))")

      out.dec
      out.puts("}")

      out.dec
      out.puts("}")
    }
  }

  //endregion Structure
}

object KotlinCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {

  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new KotlinCompiler(tp, config)

  def idToStr(id: Identifier, escapeHard: Boolean): String = {
    val name = id match {
      case InstanceIdentifier(name) => Utils.lowerCamelCase(resolveIdName(id))
      case NamedIdentifier(name) => Utils.lowerCamelCase(resolveIdName(id))
      case _ => resolveIdName(id)
    }

    if (escapeHard) escapeHardKeyword(name) else name
  }

  def resolveIdName(id: Identifier): String = id match {
    case SpecialIdentifier(name) => name
    case NamedIdentifier(name) => name
    case InstanceIdentifier(name) => name
    case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
    case RawIdentifier(innerId) => s"_raw_${resolveIdName(innerId)}"
    case IoStorageIdentifier(innerId) => s"_io_${resolveIdName(innerId)}"
  }

  def idName(id: Identifier): Option[String] = id match {
    case SpecialIdentifier(name) => Some(name)
    case NamedIdentifier(name) => Some(name)
    case InstanceIdentifier(name) => Some(name)
    case NumberedIdentifier(_) => None
    case RawIdentifier(innerId) => idName(innerId)
    case IoStorageIdentifier(innerId) => idName(innerId)
  }

  def enumIdKotlinType(): String = "IntS8"

  def valueName2Const(value: String): String = Utils.upperUnderscoreCase(value)

  def classNameFrom(names: List[String]): String = names.map(x => type2class(x)).mkString(".")

  def kotlinTypeOf(attrType: DataType): String = attrType match {
    case Int1Type(false) => s"IntU1"
    case IntMultiType(false, Width2, _) => s"IntU2"
    case IntMultiType(false, Width4, _) => s"IntU4"
    case IntMultiType(false, Width8, _) => s"IntU8"

    case Int1Type(true) => s"IntS1"
    case IntMultiType(true, Width2, _) => s"IntS2"
    case IntMultiType(true, Width4, _) => s"IntS4"
    case IntMultiType(true, Width8, _) => s"IntS8"

    case FloatMultiType(Width4, _) => "Float"
    case FloatMultiType(Width8, _) => "Double"

    case BitsType(_, _) => "Bits"

    case _: BooleanType => "Boolean"
    case CalcIntType => "IntC"
    case CalcFloatType => "FloatC"

    case _: StrType => "String"
    case _: BytesType => "ByteArray"

    case AnyType => "Any"
    case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName
    case KaitaiStructType | CalcKaitaiStructType(_) => kstructName

    case t: UserType => classNameFrom(t.name)
    case EnumType(name, _) => classNameFrom(name)

    case ArrayTypeInStream(inType) => s"ArrayList<${kotlinTypeOf(inType)}>"
    case CalcArrayType(inType, _) => s"ArrayList<${kotlinTypeOf(inType)}>"

    case st: SwitchType => kotlinTypeOf(st.combinedType)
  }

  def isPrimitiveType(attrType: DataType): Boolean = {
    attrType match {
      case _: NumericType => true
      case _: BooleanType => true
      case s: SwitchType => isPrimitiveType(s.combinedType)
      case _ => false
    }
  }

  override def kstreamName: String = "KaitaiStream"

  override def kstructName: String = "KaitaiStruct"

  def kstructTreeRootName: String = "StructRoot"

  def kstructTreeChildName: String = "StructChild"

  /**
   * Escape name which can't be used as identifier in Kotlin
   *
   * @param name name to check and escape
   * @return escaped or original name
   */
  def escapeHardKeyword(name: String): String = {
    if (hardKeywords.contains(name)) s"`$name`" else name
  }

  val hardKeywords: Seq[String] = Seq(
    "as",
    "as",
    "break",
    "class",
    "continue",
    "do",
    "else",
    "false",
    "for",
    "fun",
    "if",
    "in",
    "interface",
    "is",
    "null",
    "object",
    "package",
    "return",
    "super",
    "this",
    "throw",
    "true",
    "try",
    "typealias",
    "typeof",
    "val",
    "var",
    "when",
    "while",
  )
}

