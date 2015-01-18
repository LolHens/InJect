package org.lolhens.asmpatcher.patch

import java.util

import org.objectweb.asm.tree.ClassNode
import scala.collection.JavaConversions._

/**
 * Created by LolHens on 29.12.2014.
 */
class PatchList {
  val classPatches = new util.ArrayList[ClassPatch]()
  val methodPatches = new util.ArrayList[MethodPatch]()
  val insnPatches = new util.ArrayList[InsnPatch]()
  
  def patch(classNode: ClassNode) = {
    for (method <- classNode.methods) {

    }
  }
}
