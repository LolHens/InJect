package org.lolhens.asmpatcher.patch

import java.util

import org.objectweb.asm.tree.MethodNode

/**
 * Created by LolHens on 29.12.2014.
 */
class MethodPatch(val name: String, val signature: String) {
  var add = false
  var remove = false
  var rename: String = null
  val insnPatches = new util.ArrayList[InsnPatch]()

  def isMethodNode(methodNode: MethodNode) = methodNode.name == name
}
