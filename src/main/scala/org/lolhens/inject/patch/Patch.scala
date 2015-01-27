package org.lolhens.inject.patch

import org.lolhens.inject.asm.ComponentType
import org.objectweb.asm.tree.ClassNode

/**
 * Created by LolHens on 27.01.2015.
 */
class Patch(condition: ClassNode => Boolean, patchType: ComponentType) {
  def patch(classNode: ClassNode) = {

  }
}
