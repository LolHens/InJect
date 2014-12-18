package org.lolhens.asmpatcher

import org.objectweb.asm.tree._
import org.objectweb.asm._
import java.util
import AsmBlock._

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
    val part = insn.toLowerCase.split(" ", 2)
    val opcode = Opcode(part(0).toLowerCase)
    if (opcode != null) insert(index, toInsn(opcode, part(1)))
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

  def toInsn(opcode: Opcode, arg: String): AbstractInsnNode = {
    val args = arg.split(" ")
    opcode.optype match {
      case AbstractInsnNode.INSN => new InsnNode(opcode.opcode)
      case AbstractInsnNode.INT_INSN => new IntInsnNode(opcode.opcode, args(0).toInt)
      case AbstractInsnNode.VAR_INSN => new VarInsnNode(opcode.opcode, args(0).toInt)
      case AbstractInsnNode.TYPE_INSN => new TypeInsnNode(opcode.opcode, args(0))
      case AbstractInsnNode.FIELD_INSN => new FieldInsnNode(opcode.opcode, args(0), args(1), args(2))
      case AbstractInsnNode.METHOD_INSN => new MethodInsnNode(opcode.opcode, args(0), args(1), args(2), args(3).toBoolean)
      case AbstractInsnNode.INVOKE_DYNAMIC_INSN => ???
      case AbstractInsnNode.JUMP_INSN => new JumpInsnNode(opcode.opcode, label(args(0)))
      case AbstractInsnNode.LDC_INSN => new LdcInsnNode(fromLdcArg(args(0)))
      case AbstractInsnNode.IINC_INSN => new IincInsnNode(opcode.opcode, args(0).toInt)
      case AbstractInsnNode.LABEL => label(args(0))
      case AbstractInsnNode.TABLESWITCH_INSN => new TableSwitchInsnNode(args(0).toInt, args(1).toInt, label(args(2)), {
        val labels = new Array[LabelNode](args.length - 3)
        for (i <- 0 until labels.length) labels(i) = label(args(i + 3))
        labels
      }: _*)
      case AbstractInsnNode.LOOKUPSWITCH_INSN => {
        val default = label(args(0))
        val keys = new Array[Int](args.length - 1)
        val labels = new Array[LabelNode](keys.length)
        for (i <- 0 until keys.length) {
          val split = args(i + 1).split(":")
          keys(i) = split(0).toInt
          labels(i) = label(split(1))
        }
        new LookupSwitchInsnNode(default, keys, labels)
      }
      case AbstractInsnNode.MULTIANEWARRAY_INSN => new MultiANewArrayInsnNode(args(0), args(1).toInt)
      case AbstractInsnNode.FRAME => ???
      case _ => null
    }
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