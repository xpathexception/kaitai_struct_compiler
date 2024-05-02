package io.kaitai.struct

import io.kaitai.struct.datatype.DataType.CalcUserType
import io.kaitai.struct.format.{AttrSpec, ClassSpec, ClassSpecs, MemberSpec, ParentIdentifier, RootIdentifier}
import io.kaitai.struct.languages.KotlinCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

class KotlinClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, KotlinCompiler) {
  val kotlinlang = lang.asInstanceOf[KotlinCompiler]

  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    compileClassDoc(curClass)

    // Constructor
    compileConstructor(curClass)

    if (lang.config.readStoresPos) lang.debugClassSequence(curClass.seq)

    val specialAttrs = List(
      AttrSpec(List(), RootIdentifier, CalcUserType(topClassName, None)),
      AttrSpec(List(), ParentIdentifier, curClass.parentType)
    )

    // Attributes declarations and readers
    val allAttrs: List[MemberSpec] = curClass.seq ++ curClass.params ++ ExtraAttrs.forClassSpec(curClass, lang)
    compileAttrDeclarations(allAttrs)
    compileAttrReaders(allAttrs)

    compileEagerRead(curClass.seq, curClass.meta.endian)

    compileInstances(curClass)

    curClass.toStringExpr.foreach(expr => lang.classToString(expr))

    compileSubclasses(curClass)
    provider.nowClass = curClass

    compileEnums(curClass)

    kotlinlang.companionObject()
    lang.classFooter(curClass.name)
  }
}
