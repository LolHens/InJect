package org.lolhens.asmpatcher.patch

import java.util

import org.objectweb.asm.tree.MethodNode

/**
 * Created by LolHens on 29.12.2014.
 */
class MethodPatch(val name: String, val signature: String) {
  val changeAccess = false;
  val access = 0;
  
}
