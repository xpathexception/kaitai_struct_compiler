package io.kaitai.struct

import io.kaitai.struct.format.{ClassSpec, ClassSpecs}
import io.kaitai.struct.languages.KotlinCompiler

class KotlinClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, KotlinCompiler) {
  val kotlinlang = lang.asInstanceOf[KotlinCompiler]
}
