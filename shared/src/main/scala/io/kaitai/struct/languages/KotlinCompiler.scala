package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, ConversionError, DataType, EndOfStreamError, FixedEndian, InheritedEndian, KSError}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.KotlinCompiler.{classNameFrom, enumIdKotlinType, isPrimitiveType, kotlinTypeOf, kstreamName, kstructName, valueName2Const}
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

  override def blockScopeHeader: Unit = {
    out.puts("run {")
    out.inc
  }
  override def blockScopeFooter: Unit = universalFooter

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
    s"${config.kotlin.kotlinPackage.replace('.', '/')}/${type2class(topClassName)}.kt"
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

    out.puts("override fun _parent() = requireNotNull(_parent)")
  }

  override def classConstructorFooter: Unit = {}

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

  //region Read

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val readAccess = if (config.autoRead) "private " else ""

    val suffix = endian match {
      case Some(e) => Utils.upperUnderscoreCase(e.toSuffix)
      case None => ""
    }

    printDebugMethodName()
    out.puts(s"${readAccess}fun _read$suffix() {")
    out.inc
  }

  override def runRead(name: List[String]): Unit = out.puts("init { _read() }")

  override def runReadCalc(): Unit = {
    out.puts
    out.puts("when (_is_le) {")
    out.inc
    out.puts("true -> _readLE()")
    out.puts("false -> _readBE()")
    out.puts(s"else -> throw $kstreamName.UndecidedEndiannessError()")
    out.dec
    out.puts("}")
  }

  /** @inheritdoc */
  override def readFooter(): Unit = universalFooter

  //endregion Read

  //region Attribute

  override def attrValidateExpr(
    attrId: Identifier,
    attrType: DataType,
    checkExpr: Ast.expr,
    err: KSError,
    errArgs: List[Ast.expr]
  ): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    out.puts(s"if (!(${translator.translate(checkExpr)})) {")
    out.inc
    out.puts(s"throw ${ksErrorName(err)}($errArgsStr)")
    out.dec
    out.puts("}")
  }

  override def attributeDeclaration(
    attrName: Identifier,
    attrType: DataType,
    isNullable: Boolean
  ): Unit = {
    if (printDebugLogs) {
      out.puts
      out.puts(getDebugMethodName())
      out.puts(s"/* attrName: $attrName */")
      out.puts(s"/* attrType: $attrType */")
      out.puts(s"/* isNullable: $isNullable */")
    }

    val name = idToStr(attrName)
    val (prefix, suffix) = (isPrimitiveType(attrType), isNullable) match {
      case (false, false) => ("lateinit ", "")
      case (false, true) => ("", "? = null")
      case (true, false) => {
        importList.add("kotlin.properties.Delegates")
        ("", " by Delegates.notNull()")
      }
      case (true, true) => ("", "? = null")
    }

    out.puts(s"private ${prefix}var $name: ${kotlinTypeOf(attrType)}$suffix")
  }

  /**
   * Intended to generate getters for attributes.
   * Omitted because of using properties with private setters.
   */
  override def attributeReader(
    attrName: Identifier,
    attrType: DataType,
    isNullable: Boolean
  ): Unit = {
    printDebugMethodName()
    val reader = if (!isNullable) s"${idToStr(attrName)}" else s"requireNotNull(${idToStr(attrName)})"
    out.puts(s"fun ${idToStr(attrName)}(): ${kotlinTypeOf(attrType)} = $reader")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if (_is_le == true) {")
    out.inc
    leProc()
    out.dec
    out.puts("} else {")
    out.inc
    beProc()
    out.dec
    out.puts("}")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit = {
    out.puts(s"${privateMemberName(attrName)} = $normalIO.ensureFixedContents($contents)")
  }

  override def attrProcess(
    proc: ProcessExpr,
    varSrc: Identifier,
    varDest: Identifier,
    rep: RepeatSpec
  ): Unit = {
    printDebugMethodName()
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessZlib => s"$kstreamName.processZlib($srcExpr)"
      case ProcessXor(xorValue) => {
        val xorValueStr = translator.detectType(xorValue) match {
          case _: IntType => translator.doCast(xorValue, Int1Type(true))
          case _ => expression(xorValue)
        }
        s"$kstreamName.processXor($srcExpr, $xorValueStr)"
      }
      case ProcessRotate(isLeft, rotValue) => {
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName.processRotateLeft($srcExpr, $expr, 1)"
      }
      case ProcessCustom(name, args) => {
        val namespace = name.init.mkString(".")
        val procClass = namespace + (if (namespace.nonEmpty) "." else "") + type2class(name.last)
        val procName = s"_process_${idToStr(varSrc)}"

        out.puts(s"val $procName: $procClass = $procClass(${args.map(expression).mkString(", ")})")

        s"$procName.decode($srcExpr)"
      }
    }

    handleAssignment(varDest, expr, rep, false)
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = s"${idToStr(varName)}()"
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName[$memberName.size - 1]"
    }
  }

  //endregion Attribute

  //region IO

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val ioName = idToStr(IoStorageIdentifier(varName))

    val args = rep match {
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(varName, rep)
    }

    importList.add("io.kaitai.struct.OkioKaitaiStream")
    out.puts(s"val $ioName = OkioKaitaiStream($args)")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"val io = ${expression(ioEx)}")
    "io"
  }

  override def pushPos(io: String): Unit = {
    printDebugMethodName()
  }

  override def seek(io: String, pos: Ast.expr): Unit = {
    printDebugMethodName()

    importList.add("io.kaitai.struct.lookupAt")
    out.puts(s"$io.lookupAt(offset = ${expression(pos)}) {")
    out.inc
  }

  override def popPos(io: String): Unit = {
    printDebugMethodName()
    universalFooter
  }

  override def alignToByte(io: String): Unit = {
    out.puts(s"$io.alignToByte()")
  }

  //endregion IO

  //region Condition

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    out.puts(s"${privateMemberName(id)} = ${kotlinTypeOf(ArrayTypeInStream(dataType))}()")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    importList.add("io.kaitai.struct.doWhileWithIndex")
    out.puts("doWhileWithIndex(")

    out.inc
    out.puts("initialIndex = 0,")
    out.puts(s"condition = { i -> !$io.isEof() },")
    out.dec

    out.puts(") { i ->")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}!!.add($expr)")
  }

  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(
    id: Identifier,
    io: String,
    dataType: DataType,
    repeatExpr: expr
  ): Unit = {
    out.puts(s"for (i in 0 until ${expression(repeatExpr)}) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    handleAssignmentRepeatEos(id, expr)
  }

  override def condRepeatUntilHeader(
    id: Identifier,
    io: String,
    dataType: DataType,
    untilExpr: expr
  ): Unit = {
    importList.add("io.kaitai.struct.doWhileWithIndex")
    out.puts("doWhileWithIndex(")
    typeProvider._currentIteratorType = Some(dataType)

    out.inc
    out.puts("initialIndex = 0,")
    out.puts(s"condition = { i, ${translator.doName("_")} -> !(${expression(untilExpr)}) },")
    out.dec

    out.puts(") { i ->")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val (typeDecl, tempVar) = if (isRaw) {
      ("ByteArray", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }
    out.puts(s"($expr).also(${privateMemberName(id)}::add)")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr")
  }

  //endregion Condition

  //region Instance
  def calculatedFlagForName(ksName: Identifier) = {
    s"f_${idToStr(ksName)}"
  }

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit = {
    out.puts(s"${calculatedFlagForName(instName)} = true")
  }

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    //attributeDeclaration(attrName, attrType, isNullable)
    out.puts(s"private var ${idToStr(attrName)}: ${kotlinTypeOf(attrType)}? = null")
    out.puts(s"private var ${calculatedFlagForName(attrName)} = false")
  }

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    val nullInit = if (isNullable) "?" else ""
    out.puts(s"fun ${idToStr(instName)}(): ${kotlinTypeOf(dataType)}$nullInit {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    if (!isNullable) {
      out.puts(s"if (${calculatedFlagForName(instName)}) { return requireNotNull(${idToStr(instName)}) }")
    } else {
      instanceCheckCacheAndReturn(instName, dataType)
    }
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (${calculatedFlagForName(instName)}) { return ${idToStr(instName)} }")
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit = {
    if (dataType.isInstanceOf[NumericType]) {
      out.puts(s"${privateMemberName(instName)} = (${expression(value)}).to${kotlinTypeOf(dataType)}()")
    } else {
      out.puts(s"${privateMemberName(instName)} = ${expression(value)}")
    }
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)}")
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    if (isNullable) {
      instanceReturn(instName, attrType)
    } else {
      out.puts(s"return requireNotNull(${privateMemberName(instName)})")
    }
  }

  override def instanceFooter: Unit = {
    super.instanceFooter
  }

  //endregion Instance

  //region Switch

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    printDebugMethodName()

    val onType = translator.detectType(on)
    typeProvider._currentSwitchType = Some(onType)

    out.puts(s"when (${expression(on)}) {")
    out.inc
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    printDebugMethodName()

    val onType = typeProvider._currentSwitchType match {
      case Some(t) => t
      case None => translator.detectType(condition)
    }

    if (printDebugLogs) {
      out.puts(s"/* condition = $condition, conditionType = $onType */")
    }

    out.puts(s"${translator.doCast(condition, onType)} -> {")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    printDebugMethodName()

    out.dec
    out.puts("}")
  }

  override def switchElseStart(): Unit = {
    printDebugMethodName()

    out.puts("else -> {")
    out.inc
  }

  override def switchEnd(): Unit = {
    printDebugMethodName()

    out.dec
    out.puts("}")
  }

  //endregion Switch

  override def parseExpr(
    dataType: DataType,
    assignType: DataType,
    io: String,
    defEndian: Option[FixedEndian]
  ): String = {
    printDebugMethodName()
    val expr = dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(${translator.doCast(blt.size, CalcIntType)})"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.readBytesTerm(term = $terminator, includeTerm = $include, consumeTerm = $consume, eosError = $eosError)"
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0L"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val (parent, root) = if (t.isExternal(typeProvider.nowClass)) {
          ("null", "null")
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          (parent, Identifier.ROOT)
        }
        val addEndian = t.classSpec.get.meta.endian match {
          case Some(InheritedEndian) => s", ${Identifier.IS_LE} = ${Identifier.IS_LE}"
          case _ => ""
        }
        val addArgs = s", ${Identifier.PARENT} = $parent, ${Identifier.ROOT} = $root$addEndian"

        if (printDebugLogs) {
          out.puts(s"/* args: ${t.args} */")
          out.puts(s"/* dataType: ${dataType} */")
          out.puts(s"/* assignType: ${assignType} */")
          out.puts(s"/* dataType.params: ${t.classSpec.get.params} */")
        }

        //        val names = t.classSpec.get.params.map((p) => idToStr(p.id))
        //        val params = t.args.map((a) => translator.translate(a))
        //        val addParams = Utils.join(names.lazyZip(params).map((k, v) => s"$k = $v"), ", ", ", ", "")

        val computed = if(t.isExternal(typeProvider.nowClass)) {
          t.args.map((a) => translator.translate(a))
        } else {
          t.classSpec.get.params.lazyZip(t.args).map((param, arg) =>
            idToStr(param.id) + " = " + translator.doCast(arg, param.dataType)
          )
        }

        val addParams = Utils.join(computed, ", ", ", ", "")

        s"${classNameFrom(t.name)}(${Identifier.IO} = $io$addArgs$addParams)"
    }

    if (assignType != dataType) {
      assignType match {
        case _: NumericType => s"($expr).to${kotlinTypeOf(assignType)}()"
        case _ => expr
      }
    } else {
      expr
    }
  }

  override def bytesPadTermExpr(
    expr0: String,
    padRight: Option[Int],
    terminator: Option[Int],
    include: Boolean,
  ): String = {
    printDebugMethodName()

    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, $padByte.toByte())"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytesTerminate($expr1, $term.toByte(), $include)"
      case None => expr1
    }
    expr2
  }

  override def enumDeclaration(
    curClass: String,
    enumName: String,
    enumColl: Seq[(Long, String)]
  ): Unit = {
    printDebugMethodName()

    val enumClass = type2class(enumName)
    out.puts
    out.puts(s"enum class $enumClass(val id: ${enumIdKotlinType()}) {")
    out.inc

    if (enumColl.size > 1) {
      enumColl.dropRight(1).foreach { case (id, label) =>
        out.puts(s"${valueName2Const(label)}(${translator.doIntLiteral(id)}),")
      }
    }
    enumColl.last match {
      case (id, label) =>
        out.puts(s"${valueName2Const(label)}(${translator.doIntLiteral(id)});")
    }

    out.puts

    out.puts("companion object {")
    out.inc

    out.puts(s"private val byId: Map<${enumIdKotlinType()}, $enumClass> = entries.associateBy { it.id }")
    out.puts
    out.puts(s"fun byId(id: ${enumIdKotlinType()}): $enumClass = byId.getValue(id)")

    out.dec
    out.puts("}")

    out.dec
    out.puts("}")
  }

  override def createSubstreamFixedSize(id: Identifier, blt: BytesLimitType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]): String = {
    val ioName = idToStr(IoStorageIdentifier(id))
    handleAssignmentTempVar(KaitaiStreamType, ioName, s"$io.substream(${translator.doCast(blt.size, CalcIntType)})")
    ioName
  }

  override def extraRawAttrForUserTypeFromBytes(id: Identifier, ut: UserTypeFromBytes, condSpec: ConditionalSpec): List[AttrSpec] = {
    if (config.zeroCopySubstream) {
      ut.bytes match {
        case BytesLimitType(sizeExpr, None, _, None, None) =>
          // substream will be used, no need for store raws
          List()
        case _ =>
          // buffered implementation will be used, fall back to raw storage
          super.extraRawAttrForUserTypeFromBytes(id, ut, condSpec)
      }
    } else {
      // zero-copy streams disabled, fall back to raw storage
      super.extraRawAttrForUserTypeFromBytes(id, ut, condSpec)
    }
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit = {
    out.puts(s"val $id: ${kotlinTypeOf(dataType)} = $expr")
  }
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

    case BitsType(_, _) => "IntS8"

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

