package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.KotlinCompiler.{kaitaiType2KotlinType, kstreamName, kstructName, types2class}
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators._

class KotlinCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UpperCamelCaseClasses
    with ObjectOrientedLanguage
    with EveryReadIsExpression
    with UniversalFooter
    with UniversalDoc
    with AllocateIOLocalVar
    with FixedContentsUsingArrayByteLiteral
    with NoNeedForFullClassPath {

  val translator = new KotlinTranslator(typeProvider, importList)

  // Preprocess fromFileClass and make import
  val fromFileClass: String = {
    val pos = config.java.fromFileClass.lastIndexOf('.')
    if (pos < 0) {
      // If relative "fromFileClass", then just use it as is
      config.java.fromFileClass
    } else {
      // If absolute "fromFileClass", add relevant import + use relative
      importList.add(config.java.fromFileClass)
      config.java.fromFileClass.substring(pos + 1)
    }
  }

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "    "

  override def outFileName(topClassName: String): String =
    s"${config.java.javaPackage.replace('.', '/')}/${type2class(topClassName)}.kt"

  override def outImports(topClass: ClassSpec): String =
    "\n" + importList.toList.map((x) => s"import $x").mkString("\n") + "\n"

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    if (config.java.javaPackage.nonEmpty) {
      outHeader.puts
      outHeader.puts(s"package ${config.java.javaPackage}")
    }

    // Used in every class
    importList.add(s"io.kaitai.struct.$kstructName")
    importList.add(s"io.kaitai.struct.$kstreamName")

    out.puts
  }

  override def classHeader(name: String): Unit = {
    val typeDescStr = if (out.indentLevel > 0) {
      "class "
    } else {
      "class "
    }

    out.puts(s"${typeDescStr}${type2class(name)} : $kstructName {")
    out.inc

    if (config.readStoresPos) {
      out.puts("val _attrStart = mutableMapOf<String, Int>()")
      out.puts("val _attrEnd = mutableMapOf<String, Int>()")
      out.puts("val _arrStart = mutableMapOf<String, ArrayList<Int>>()")
      out.puts("val _arrEnd = mutableMapOf<String, ArrayList<Int>>()")
      out.puts
    }
  }

  override def classFooter(name: String): Unit = {
    val isInheritedEndian = typeProvider.nowClass.meta.endian match {
      case Some(InheritedEndian) => true
      case _ => false
    }

    // fromFile helper makes no sense for inherited endianness structures:
    // they require endianness to be parsed anyway
    if (
      !isInheritedEndian &&
        config.java.fromFileClass.nonEmpty &&
        typeProvider.nowClass.params.isEmpty
    ) {
      out.puts(s"companion object /* 1 */ {")
      out.inc

      out.puts(s"fun fromFile(fileName: String): ${type2class(name)} {")
      out.inc
      out.puts(s"return ${type2class(name)}($fromFileClass(fileName))")
      out.dec
      out.puts("}")

      out.dec
      out.puts("}")
    }

    super.classFooter(name)
  }

  override def classConstructorHeader(
    name: String,
    parentType: DataType,
    rootClassName: String,
    isHybrid: Boolean,
    params: List[ParamDefSpec]
  ): Unit = {
    val hasLeVar = typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        out.puts("private var _is_le: Boolean? = null")
        true
      case _ =>
        // no _is_le variable
        false
    }

    val paramsArg = Utils.join(params.map((p) =>
      s"${paramName(p.id)}: ${kaitaiType2KotlinType(p.dataType)}"
    ), "", ", ", ",")

    out.puts("constructor(")
    out.inc
    out.puts(s"_io: $kstreamName,")
    out.puts(s"_parent: ${kaitaiType2KotlinType(parentType)},")
    out.puts(s"_root: ${type2class(rootClassName)}? = null,")
    if (hasLeVar) out.puts(s"_is_le: Boolean? = null,")
    if (params.nonEmpty) out.puts(paramsArg)
    out.dec
    out.puts(") : super(_io) {")

    out.inc

    out.puts("this._parent = _parent")
    if (hasLeVar) out.puts("this._is_le = _is_le")

    if (!isHybrid) {
      if (name == rootClassName) {
        out.puts("this._root = _root ?: this")
      } else {
        out.puts("this._root = _root")
      }
    }

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val readAccessAndType = if (!config.autoRead) "" else "private "

    val suffix = endian match {
      case Some(e) => Utils.upperUnderscoreCase(e.toSuffix)
      case None => ""
    }

    out.puts(s"${readAccessAndType}fun _read$suffix() {")
    out.inc
  }

  override def readFooter(): Unit = universalFooter

  override def runRead(name: List[String]): Unit =
    out.puts("_read()")

  override def runReadCalc(): Unit = {
    out.puts("when (_is_le) {")
    out.inc

    out.puts(s"null -> throw $kstreamName.UndecidedEndiannessError()")
    out.puts(s"true -> _readLE()")
    out.puts(s"false -> _readBE()")

    out.dec
    out.puts("}")
  }

  override def attributeDeclaration(
    attrName: Identifier,
    attrType: DataType,
    isNullable: Boolean
  ): Unit = {
    out.puts(s"private var ${idToStr(attrName)}: ${kaitaiType2KotlinType(attrType, isNullable)}")
  }

  override def attributeReader(
    attrName: Identifier,
    attrType: DataType,
    isNullable: Boolean
  ): Unit = {
    out.puts(s"fun ${idToStr(attrName)}(): ${kaitaiType2KotlinType(attrType, isNullable)} = ${idToStr(attrName)}")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    out.puts("/**")

    doc.summary.foreach(summary => out.putsLines(" * ", summary))

    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(" * ", "@see \"" + text + "\"")
      case ref: UrlRef =>
        out.putsLines(" * ", s"@see ${ref.toAhref}")
    }

    out.puts(" */")
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

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = idToStr(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName.get($memberName.size() - 1)"
    }
  }

  override def attrProcess(
    proc: ProcessExpr,
    varSrc: Identifier,
    varDest: Identifier,
    rep: RepeatSpec
  ): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val xorValueStr = translator.detectType(xorValue) match {
          case _: IntType => translator.doCast(xorValue, Int1Type(true))
          case _ => expression(xorValue)
        }
        s"$kstreamName.processXor($srcExpr, $xorValueStr)"
      case ProcessZlib =>
        s"$kstreamName.processZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName.processRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val namespace = name.init.mkString(".")
        val procClass = namespace +
          (if (namespace.nonEmpty) "." else "") +
          type2class(name.last)
        val procName = s"_process_${idToStr(varSrc)}"
        out.puts(s"val $procName: $procClass = $procClass(${args.map(expression).mkString(", ")})")
        s"$procName.decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  //region io

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val ioName = idToStr(IoStorageIdentifier(varName))

    val args = rep match {
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(varName, rep)
    }

    importList.add("io.kaitai.struct.ByteBufferKaitaiStream")
    out.puts(s"$kstreamName $ioName = new ByteBufferKaitaiStream($args)")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"var io: $kstreamName = ${expression(ioEx)}")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"val _pos: Long = $io.pos()")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)})")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos)")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.alignToByte()")

  //endregion io

  def getOrCreatePosList(listName: String, varName: String, io: String): Unit = {
    out.puts("run {")
    out.inc
    out.puts(s"val _posList = $listName.getOrPut(\"$varName\") { arrayListOf() }")
    out.puts(s"_posList.add($io.pos())")
    out.dec
    out.puts("}")
  }

  override def attrDebugStart(
    attrId: Identifier,
    attrType: DataType,
    ios: Option[String],
    rep: RepeatSpec
  ): Unit = {
    ios.foreach { (io) =>
      val name = idToStr(attrId)
      rep match {
        case NoRepeat =>
          out.puts(s"_attrStart[\"$name\"] = $io.pos()")
        case _: RepeatExpr | RepeatEos | _: RepeatUntil =>
          getOrCreatePosList("_arrStart", name, io)
      }
    }
  }

  override def attrDebugArrInit(attrName: Identifier, attrType: DataType): Unit = {
    // no _debug[$name]['arr'] initialization needed in Java
  }

  override def attrDebugEnd(
    attrId: Identifier,
    attrType: DataType,
    io: String,
    rep: RepeatSpec
  ): Unit = {
    val name = idToStr(attrId)
    rep match {
      case NoRepeat =>
        out.puts(s"_attrEnd[\"$name\"] = $io.pos()")
      case _: RepeatExpr | RepeatEos | _: RepeatUntil =>
        getOrCreatePosList("_arrEnd", name, io)
    }
  }

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    out.puts(s"${privateMemberName(id)} = ${kaitaiType2KotlinType(ArrayTypeInStream(dataType))}()")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts("var i: Int = 0")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr)")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("i++")
    out.dec
    out.puts("}")
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

    //importList.add("java.util.ArrayList")
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatUntilHeader(
    id: Identifier,
    io: String,
    dataType: DataType,
    untilExpr: expr
  ): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"var ${translator.doName("_")}: ${kaitaiType2KotlinType(dataType)}")
    out.puts("var i: Int = 0")
    out.puts("do {")
    out.inc

    //importList.add("java.util.ArrayList")
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val (typeDecl, tempVar) = if (isRaw) {
      ("ByteArray", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }
    out.puts(s"var $tempVar: $typeDecl = $expr")
    out.puts(s"${privateMemberName(id)}.add($tempVar)")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("i++")
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}))")
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"var $id: ${kaitaiType2KotlinType(dataType)} = $expr")

  override def blockScopeHeader: Unit = {
    out.puts("run {")
    out.inc
  }

  override def blockScopeFooter: Unit = universalFooter

  override def parseExpr(
    dataType: DataType,
    assignType: DataType,
    io: String,
    defEndian: Option[FixedEndian]
  ): String = {
    val expr = dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.readBytesTerm((byte) $terminator, $include, $consume, $eosError)"
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val addArgs = if (t.isExternal(typeProvider.nowClass)) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", _is_le"
            case _ => ""
          }
          s", $parent, _root$addEndian"
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        s"${types2class(t.name)}($io$addArgs$addParams)"
    }

    if (assignType != dataType) {
      s"(${kaitaiType2KotlinType(assignType)}) ($expr)"
    } else {
      expr
    }
  }

  override def createSubstreamFixedSize(
    id: Identifier,
    blt: BytesLimitType,
    io: String,
    rep: RepeatSpec,
    defEndian: Option[FixedEndian]
  ): String = {
    val ioName = idToStr(IoStorageIdentifier(id))
    handleAssignmentTempVar(KaitaiStreamType, ioName, s"$io.substream(${translator.translate(blt.size)})")
    ioName
  }

  override def extraRawAttrForUserTypeFromBytes(
    id: Identifier,
    ut: UserTypeFromBytes,
    condSpec: ConditionalSpec
  ): List[AttrSpec] = {
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

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
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

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    val expr = if (assignType != dataType) {
      s"((${kaitaiType2KotlinType(dataType)}) ($id))"
    } else {
      id
    }
    out.puts(s"$expr._read()")
  }

  override def switchCasesRender[T](
    id: Identifier,
    on: Ast.expr,
    cases: Map[Ast.expr, T],
    normalCaseProc: T => Unit,
    elseCaseProc: T => Unit
  ): Unit = {
    // Java has a stupid limitation of being unable to match nulls in switch.
    // If our type is nullable, we'll do an extra check. For now, we're only
    // doing this workaround for enums.

    // todo: may be fine in Kotlin

    super.switchCasesRender(id, on, cases, normalCaseProc, elseCaseProc)
  }

  def value2Const(s: String) = Utils.upperUnderscoreCase(s)

  //  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
  //    case _: IntType | _: EnumType | _: StrType => false
  //    case _ => true
  //  }

  //<editor-fold desc="switching: true version">

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"when (${expression(on)}) {")

  override def switchCaseFirstStart(condition: Ast.expr): Unit = switchCaseStart(condition)

  override def switchCaseStart(condition: Ast.expr): Unit = {
    // Java is very specific about what can be used as "condition" in "case
    // condition:".
    val condStr = condition match {
      case enumByLabel: Ast.expr.EnumByLabel =>
        // If switch is over a enum, only literal enum values are supported,
        // and they must be written as "MEMBER", not "SomeEnum.MEMBER".
        value2Const(enumByLabel.label.name)
      case _ =>
        expression(condition)
    }

    out.puts(s"$condStr -> {")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def switchElseStart(): Unit = {
    out.puts("else -> {")
    out.inc
  }

  override def switchEnd(): Unit = {
    out.puts("}")
  }

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  //  override def switchIfStart(id: Identifier, on: expr, onType: DataType): Unit = {
  //    out.puts("{")
  //    out.inc
  //    out.puts(s"val ${expression(NAME_SWITCH_ON)}: ${kaitaiType2KotlinType(onType)} = ${expression(on)}")
  //  }

  def switchCmpExpr(condition: Ast.expr): String =
    expression(
      Ast.expr.Compare(
        NAME_SWITCH_ON,
        Ast.cmpop.Eq,
        condition
      )
    )

  //  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
  //    out.puts(s"if (${switchCmpExpr(condition)}) {")
  //    out.inc
  //  }
  //
  //  override def switchIfCaseStart(condition: Ast.expr): Unit = {
  //    out.puts(s"else if (${switchCmpExpr(condition)}) {")
  //    out.inc
  //  }
  //
  //  override def switchIfCaseEnd(): Unit = {
  //    out.dec
  //    out.puts("}")
  //  }
  //
  //  override def switchIfElseStart(): Unit = {
  //    out.puts("else {")
  //    out.inc
  //  }

  //  override def switchIfEnd(): Unit = {
  //    out.dec
  //    out.puts("}")
  //  }

  //</editor-fold>

  override def instanceDeclaration(
    attrName: InstanceIdentifier,
    attrType: DataType,
    isNullable: Boolean
  ): Unit = {
    val suffix = if (isNullable) "?" else ""
    out.puts(s"var ${idToStr(attrName)}: ${kaitaiType2KotlinType(attrType)}$suffix")
  }

  override def instanceHeader(
    className: String,
    instName: InstanceIdentifier,
    dataType: DataType,
    isNullable: Boolean
  ): Unit = {
    val suffix = if (isNullable) "?" else ""
    out.puts(s"fun ${idToStr(instName)}(): ${kaitaiType2KotlinType(dataType)}$suffix {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(
    instName: InstanceIdentifier,
    dataType: DataType
  ): Unit = {
    out.puts(s"if (${privateMemberName(instName)} != null)")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
  }

  override def instanceReturn(
    instName: InstanceIdentifier,
    attrType: DataType
  ): Unit = {
    out.puts(s"return ${privateMemberName(instName)}")
  }

  //  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit = {
  //    val primType = kaitaiType2KotlinType(dataType)
  //    val boxedType = kaitaiType2KotlinType(dataType)
  //
  //    if (primType != boxedType) {
  //      // Special trick to achieve both implicit type conversion + boxing.
  //      // Unfortunately, Java can't do both in one assignment, i.e. this would fail:
  //      //
  //      // Double c = 1.0f + 1;
  //
  //      out.puts(s"var _tmp: $primType = ${expression(value)} aserty $primType")
  //      out.puts(s"${privateMemberName(instName)} = _tmp")
  //    } else {
  //      out.puts(s"${privateMemberName(instName)} = ${expression(value)}")
  //    }
  //  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    val enumClass = type2class(enumName)

    out.puts
    out.puts(s"enum class $enumClass(val id: Long) {")
    out.inc

    if (enumColl.size > 1) {
      enumColl.dropRight(1).foreach { case (id, label) =>
        out.puts(s"${value2Const(label)}(${translator.doIntLiteral(id)}),")
      }
    }
    enumColl.last match {
      case (id, label) =>
        out.puts(s"${value2Const(label)}(${translator.doIntLiteral(id)});")
    }

    out.puts

    out.puts("companion object {")
    out.inc

    out.puts(s"private val byId: Map<Long, $enumClass> = entries.associateBy { it.id }")
    out.puts

    out.puts(s"fun byId(id: Long): $enumClass = byId.getValue(id)")
    out.dec
    out.puts("}")

    //importList.add("java.util.Map")
    //importList.add("java.util.HashMap")
  }

  override def debugClassSequence(seq: List[AttrSpec]) = {
    val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    out.puts(s"val _seqFields: Array<String> = arrayOf($seqStr)")
    out.puts
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts
    out.puts("override fun toString(): String {")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)}")
    out.dec
    out.puts("}")
  }

  override def idToStr(id: Identifier): String = KotlinCompiler.idToStr(id)

  override def publicMemberName(id: Identifier) = KotlinCompiler.publicMemberName(id)

  override def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => config.java.endOfStreamErrorClass
    case ConversionError => "NumberFormatException"
    case _ => s"KaitaiStream.${err.name}"
  }

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
}

object KotlinCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new KotlinCompiler(tp, config)

  def idToStr(id: Identifier): String = id match {
    case SpecialIdentifier(name) => name
    case NamedIdentifier(name) => Utils.lowerCamelCase(name)
    case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
    case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
    case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
    case IoStorageIdentifier(innerId) => s"_io_${idToStr(innerId)}"
  }

  def publicMemberName(id: Identifier) = idToStr(id)

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString(".")

  def kaitaiType2KotlinType(attrType: DataType, isNullable: Boolean): String = {
    if (isNullable) {
      s"${kaitaiType2KotlinType(attrType)}?"
    } else {
      kaitaiType2KotlinType(attrType)
    }
  }

  def kaitaiType2KotlinType(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "Int"
      case IntMultiType(false, Width2, _) => "Int"
      case IntMultiType(false, Width4, _) => "Long"
      case IntMultiType(false, Width8, _) => "Long"

      case Int1Type(true) => "Byte"
      case IntMultiType(true, Width2, _) => "Short"
      case IntMultiType(true, Width4, _) => "Int"
      case IntMultiType(true, Width8, _) => "Long"

      case FloatMultiType(Width4, _) => "Float"
      case FloatMultiType(Width8, _) => "Double"

      case BitsType(_, _) => "Long"

      case _: BooleanType => "Boolean"
      case CalcIntType => "Int"
      case CalcFloatType => "Double"

      case _: StrType => "String"
      case _: BytesType => "ByteArray"

      case AnyType => "Any"
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName
      case KaitaiStructType | CalcKaitaiStructType(_) => kstructName

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => types2class(name)

      case ArrayTypeInStream(inType) => s"ArrayList<${kaitaiType2KotlinType(inType)}> /* 1 */"
      case CalcArrayType(inType, _) => s"ArrayList<${kaitaiType2KotlinType(inType)}> /* 2 */"

      case st: SwitchType => kaitaiType2KotlinType(st.combinedType)
    }
  }

  override def kstreamName: String = "KaitaiStream"

  override def kstructName: String = "KaitaiStruct"
}
