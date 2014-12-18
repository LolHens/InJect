package org.lolhens.asmpatcher

import org.objectweb.asm.tree._
import java.util

/**
 * Created by LolHens on 18.12.2014.
 */
class AsmBlock {
  val insnList = new util.LinkedList[AbstractInsnNode]()
  val labelList = new util.LinkedList[Object]()

  private def insert(index: Int, insnNode: AbstractInsnNode) = {
    insnList.add(index, insnNode)
  }

  private def insert(index: Int, insn: String) = {
    val part = insn.toLowerCase.split(" ", 2)
    val opcode = Opcode(part(0).toLowerCase)
    if (opcode != null) insert(index, toInsn(opcode, part(1)))
  }

  def toInsn(opcode: Opcode, arg: String): AbstractInsnNode = {
    val args = arg.split(" ")
    opcode.optype match {
      case AbstractInsnNode.INSN => new InsnNode(opcode.opcode)
      case AbstractInsnNode.INT_INSN => new IntInsnNode(opcode.opcode, args(0).toInt)
      case AbstractInsnNode.VAR_INSN => new VarInsnNode(opcode.opcode, args(0).toInt)
      case AbstractInsnNode.TYPE_INSN => ??? //TODO
      case AbstractInsnNode.FIELD_INSN => ??? //TODO
      case AbstractInsnNode.METHOD_INSN => ??? //TODO
      case AbstractInsnNode.INVOKE_DYNAMIC_INSN => ???
      case AbstractInsnNode.JUMP_INSN => new JumpInsnNode(opcode.opcode, null) //TODO LABEL!
      case AbstractInsnNode.LDC_INSN => args(0) match {
        case arg if (arg == "*") => new LdcInsnNode(null)
        case arg if (arg.endsWith("\"")) => new LdcInsnNode(arg.drop(1).dropRight(1))
        case arg if (arg.endsWith("l")) => new LdcInsnNode(arg.dropRight(1).toLong)
        case arg if (arg.endsWith("f")) => new LdcInsnNode(arg.dropRight(1).toFloat)
        case arg if (arg.endsWith("d")) => new LdcInsnNode(arg.dropRight(1).toDouble)
        case arg if (arg.contains(".")) => new LdcInsnNode(arg.toDouble)
        case arg => new LdcInsnNode(arg.toInt)
      }
      case AbstractInsnNode.IINC_INSN => new IincInsnNode(opcode.opcode, args(0).toInt)
      case AbstractInsnNode.LABEL => ???
      case AbstractInsnNode.TABLESWITCH_INSN => ???
      case AbstractInsnNode.LOOKUPSWITCH_INSN => ???
      case AbstractInsnNode.MULTIANEWARRAY_INSN => new MultiANewArrayInsnNode(args(0), args(1).toInt)
      case AbstractInsnNode.FRAME => ???
      case _ => null
    }
  }
}
