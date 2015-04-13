package org.lolhens.inject.patch

import java.util

import org.objectweb.asm.tree.ClassNode

/**
 * Created by LolHens on 29.12.2014.
 */
class PatchList() {
  private val patchList = new util.ArrayList[Patch]()

  def +=(patch: Patch) = patchList.add(patch)

  def patch(classNode: ClassNode): Unit = patchList.forEach((element: Patch) => element.patch(classNode): Unit)
}
