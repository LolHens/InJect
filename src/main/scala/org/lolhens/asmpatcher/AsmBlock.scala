package org.lolhens.asmpatcher

import java.util

import org.lolhens.asmpatcher.AsmBlock._
import org.objectweb.asm._
import org.objectweb.asm.tree._

import scala.collection.JavaConversions._

/**
 * Created by LolHens on 18.12.2014.
 */
class AsmBlock {
  val insnList = new util.LinkedList[AbstractInsnNode]()
  val labelMap = new util.HashMap[Int, LabelNode]()

  private def insert(index: Int, insnNode: AbstractInsnNode): Unit = {
    insnList.add(index, insnNode)
  }

  def insert(index: Int, insn: String): Unit = {
    val opcode = Opcode(insn)
    if (opcode != null) insert(index, toInsn(opcode, insn))
  }

  def label(num: Int): LabelNode = {
    if (labelMap.containsKey(num)) return labelMap.get(num)
    val newLabel = new LabelNode(new Label())
    labelMap.put(num, newLabel)
    newLabel
  }

  private def label(arg: String): LabelNode = {
    if (arg.toLowerCase.startsWith("l"))
      label(arg.drop(1).toInt)
    else
      null
  }

  def toInsn(opcode: Opcode, insn: String): AbstractInsnNode = {
    val insnParts = insn.split(" ")
    opcode.optype match {
      case AbstractInsnNode.INSN => new InsnNode(opcode.opcode)
      case AbstractInsnNode.INT_INSN => new IntInsnNode(opcode.opcode, insnParts(1).toInt)
      case AbstractInsnNode.VAR_INSN => new VarInsnNode(opcode.opcode, insnParts(1).toInt)
      case AbstractInsnNode.TYPE_INSN => new TypeInsnNode(opcode.opcode, insnParts(1))
      case AbstractInsnNode.FIELD_INSN => new FieldInsnNode(opcode.opcode, insnParts(1), insnParts(2), insnParts(3))
      case AbstractInsnNode.METHOD_INSN => {
        val split = insnParts(1).split("\\.")
        new MethodInsnNode(opcode.opcode, split(0), split(1), insnParts(2), false)
      }
      case AbstractInsnNode.INVOKE_DYNAMIC_INSN => ???
      case AbstractInsnNode.JUMP_INSN => new JumpInsnNode(opcode.opcode, label(insnParts(1)))
      case AbstractInsnNode.LDC_INSN => new LdcInsnNode(fromLdcArg(insnParts(1)))
      case AbstractInsnNode.IINC_INSN => new IincInsnNode(opcode.opcode, insnParts(1).toInt)
      case AbstractInsnNode.LABEL => label(insnParts(0))
      case AbstractInsnNode.TABLESWITCH_INSN => new TableSwitchInsnNode(insnParts(1).toInt, insnParts(2).toInt, label(insnParts(3)), {
        val labels = new Array[LabelNode](insnParts.length - 4)
        for (i <- 0 until labels.length) labels(i) = label(insnParts(i + 4))
        labels
      }: _*)
      case AbstractInsnNode.LOOKUPSWITCH_INSN => {
        val default = label(insnParts(1))
        val keys = new Array[Int](insnParts.length - 2)
        val labels = new Array[LabelNode](keys.length)
        for (i <- 0 until keys.length) {
          val split = insnParts(i + 2).split(":")
          keys(i) = split(0).toInt
          labels(i) = label(split(1))
        }
        new LookupSwitchInsnNode(default, keys, labels)
      }
      case AbstractInsnNode.MULTIANEWARRAY_INSN => new MultiANewArrayInsnNode(insnParts(1), insnParts(2).toInt)
      case AbstractInsnNode.FRAME => ???
      case _ => null
    }
  }

  override def toString = {
    var string = ""
    for (insn: AbstractInsnNode <- insnList) string += insn.toString + "\n"
    string.dropRight(1)
  }
}

object AsmBlock {
  private def fromLdcArg(arg: String): Any = arg.toLowerCase match {
    case arg if (arg == "*") => null
    case arg if (arg.endsWith("\"")) => arg.drop(1).dropRight(1)
    case arg if (arg.endsWith("l")) => arg.dropRight(1).toLong
    case arg if (arg.endsWith("f")) => arg.dropRight(1).toFloat
    case arg if (arg.endsWith("d")) => arg.dropRight(1).toDouble
    case arg if (arg.contains(".")) => arg.toDouble
    case arg => arg.toInt
  }
}