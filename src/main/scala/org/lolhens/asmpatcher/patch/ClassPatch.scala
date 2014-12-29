package org.lolhens.asmpatcher.patch

import java.util

import org.objectweb.asm.tree.ClassNode
import scala.collection.JavaConversions._

/**
 * Created by LolHens on 29.12.2014.
 */
class ClassPatch(val name: String) {
  var add = false
  var delete = false
  var setAccess = 0
  val fieldPatches = new util.ArrayList[FieldPatch]()
  val methodPatches = new util.ArrayList[MethodPatch]()

  def patch(classNode: ClassNode) = {
    classNode.access

    for (methodPatch <- methodPatches) {
      var methodNode = for (methodNode <- classNode.methods) if (methodPatch)
      if (methodNode.name == methodPatch.name) {

      }
    }
  }
}
