package io.kaitai.struct

import io.kaitai.struct.datatype.DataType.{CalcUserType, UserType}
import io.kaitai.struct.datatype.Endianness
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.ExtraAttrs
import io.kaitai.struct.languages.{KotlinCompiler, KtConfig}

class KotlinClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, KotlinCompiler) {
  val kotlinlang = lang.asInstanceOf[KotlinCompiler]

  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    compileClassDoc(curClass)
    lang.classHeader(curClass.name)

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(List(typeName)) }

    // Forward declarations for params which reference types external to this type
    curClass.params.foreach((paramDefSpec) =>
      paramDefSpec.dataType match {
        case ut: UserType =>
          if (ut.isExternal(curClass)) {
            val externalTypeName = ut.classSpec.get.name
            lang.classForwardDeclaration(externalTypeName)
          }
        case _ => // no forward declarations needed
      }
    )

    compileConstructor(curClass)

    if (lang.config.readStoresPos) lang.debugClassSequence(curClass.seq)

    // Read method(s)
    compileEagerRead(curClass.seq, curClass.meta.endian)

    compileInstances(curClass)

    // Attributes declarations and readers
    val allAttrs: List[MemberSpec] =
      curClass.seq ++
        curClass.params ++
        List(
          AttrSpec(List(), RootIdentifier, CalcUserType(topClassName, None)),
          AttrSpec(List(), ParentIdentifier, curClass.parentType)
        ) ++
        ExtraAttrs.forClassSpec(curClass, lang)
    compileAttrDeclarations(allAttrs)
    compileAttrReaders(allAttrs)

    curClass.toStringExpr.foreach(expr => lang.classToString(expr))

    compileSubclasses(curClass)
    provider.nowClass = curClass

    compileEnums(curClass)

    lang.classFooter(curClass.name)
  }

  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    compileInstanceDeclaration(instName, instSpec)

    if (!lang.innerDocstrings)
      compileInstanceDoc(instName, instSpec)
    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
    if (lang.innerDocstrings)
      compileInstanceDoc(instName, instSpec)
    lang.instanceCheckCacheAndReturn(instName, dataType)

    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
        lang.instanceSetCalculated(instName)
      case pi: ParseInstanceSpec =>
        lang.attrParse(pi, instName, endian)
    }

    if (KtConfig.returnNullByDefault) kotlinlang.instanceReturn(instName, dataType, instSpec.isNullable)
    else lang.instanceReturn(instName, dataType)

    lang.instanceFooter
  }

  override def compileInstanceDeclaration(
    instName: InstanceIdentifier,
    instSpec: InstanceSpec
  ): Unit = {
    super.compileInstanceDeclaration(instName, instSpec)
  }
}
