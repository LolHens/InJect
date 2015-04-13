package org.lolhens.inject.patch

import org.objectweb.asm.tree.ClassNode

/**
 * Created by LolHens on 27.01.2015.
 */
class Patch(condition: ClassNode => Boolean) {
  def patch(classNode: ClassNode) = {

  }
}
