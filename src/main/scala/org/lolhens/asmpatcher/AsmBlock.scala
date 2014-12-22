package org.lolhens.asmpatcher

import java.util

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.util._

import scala.collection.JavaConversions._

/**
 * Created by LolHens on 18.12.2014.
 */
class AsmBlock {
  val insnList = new InsnList()
  val labelMap = new util.HashMap[Int, LabelNode]()

  def insert(index: Int, insnNode: AbstractInsnNode): Unit = {
    if (index == 0)
      insnList.insert(insnNode)
    else
      insnList.insert(insnList.get(index - 1), insnNode)
  }

  def label(num: Int): LabelNode = {
    if (labelMap.containsKey(num)) return labelMap.get(num)
    val newLabel = new LabelNode(new Label())
    labelMap.put(num, newLabel)
    newLabel
  }

  override def toString = {
    val printer = new Textifier()

    val traceMethodVisitor = new TraceMethodVisitor(printer)

    val methodNode = new MethodNode()
    methodNode.instructions = insnList
    methodNode.accept(traceMethodVisitor)

    var ret = ""
    for (text <- printer.text) ret += text
    ret
  }
}

