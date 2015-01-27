package org.lolhens.asmpatcher.patch

import java.io.{InputStreamReader, BufferedReader, BufferedInputStream, InputStream}
import java.util
import java.util.function.Consumer

import org.objectweb.asm.tree.ClassNode
import scala.collection.JavaConversions._
import org.lolhens.scala.function.JavaConversions._

/**
 * Created by LolHens on 29.12.2014.
 */
class PatchList() {
  private val patchList = new util.ArrayList[Patch]()

  def +=(patch: Patch) = patchList.add(patch)

  def patch(classNode: ClassNode): Unit = patchList.forEach((element: Patch) => {
    element.patch(classNode)
  }: Unit);
}
