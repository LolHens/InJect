package org.lolhens.asmpatcher.asmblock

import org.lolhens.asmpatcher.Opcode
import org.lolhens.asmpatcher.asmblock.AsmBlockParser._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import java.util
import scala.collection.JavaConversions._

/**
 * Created by LolHens on 23.12.2014.
 */
class AsmBlockParser(val asmBlock: AsmBlock = new AsmBlock()) {
  def parseInsn(insn: String): AbstractInsnNode = {
    val opcode_args = insn.split(" ", 2)
    val opcode = Opcode(opcode_args(0))
    if (opcode != null) return parseInsnArgs(opcode, opcode_args(1))

    val labelNode = parseLabel(insn)
    if (labelNode != null) return labelNode

    null
  }

  def parseLabel(arg: String): LabelNode = {
    if (arg.toLowerCase.startsWith("l") && arg.drop(1).matches("-?\\d+"))
      asmBlock.label(arg.drop(1).toInt)
    else
      null
  }

  def parseInsnArgs(opcode: Opcode, _args: String): AbstractInsnNode = {
    val args = _args
      .replaceAll("\r\n|\r|\n", " ")
      .replaceAll(":", "")
      .trim
      .replaceAll(" +", " ")
      .toLowerCase

    val split = args.split(" ")

    opcode.optype match {
      case AbstractInsnNode.FIELD_INSN | AbstractInsnNode.METHOD_INSN => {
        val owner_name = split(0).split("\\.")
        val desc = split(1)
        parseSeparatedInsnArgs(opcode, Array(owner_name(0), owner_name(1), desc))
      }
      case _ => parseSeparatedInsnArgs(opcode, split)
    }
  }

  def parseSeparatedInsnArgs(opcode: Opcode, args: Array[String]): AbstractInsnNode = {
    opcode.optype match {
      case AbstractInsnNode.INSN => new InsnNode(opcode.opcode)
      case AbstractInsnNode.INT_INSN => new IntInsnNode(opcode.opcode, args(0).toInt)
      case AbstractInsnNode.VAR_INSN => new VarInsnNode(opcode.opcode, args(0).toInt)
      case AbstractInsnNode.TYPE_INSN => new TypeInsnNode(opcode.opcode, args(0))
      case AbstractInsnNode.FIELD_INSN => new FieldInsnNode(opcode.opcode, args(0), args(1), args(2))
      case AbstractInsnNode.METHOD_INSN => new MethodInsnNode(opcode.opcode, args(0), args(1), args(2), opcode.opcode == Opcodes.INVOKEINTERFACE)
      case AbstractInsnNode.INVOKE_DYNAMIC_INSN => ???
      case AbstractInsnNode.JUMP_INSN => new JumpInsnNode(opcode.opcode, parseLabel(args(0)))
      case AbstractInsnNode.LDC_INSN => new LdcInsnNode(castLdcArg(args(0)))
      case AbstractInsnNode.IINC_INSN => new IincInsnNode(opcode.opcode, args(0).toInt)
      case AbstractInsnNode.LABEL => parseLabel(args(0)) //unused
      case AbstractInsnNode.TABLESWITCH_INSN => {
        val (labelMap, default) = parseSwitchInsn(args)

        var min = -1
        for (num <- labelMap.keySet()) if (min == -1 || num < min) min = num

        val labels = new Array[LabelNode](labelMap.size())
        for (i <- 0 until labels.length) {
          val label = labelMap.get(i + min)
          if (label == null) throw new IllegalArgumentException(s"Missing label #${i + min}")
          labels(i) = label
        }

        new TableSwitchInsnNode(min, min + labels.length - 1, default, labels: _*)
      }
      case AbstractInsnNode.LOOKUPSWITCH_INSN => {
        val (labelMap, default) = parseSwitchInsn(args)

        val keys = new Array[Int](labelMap.size())
        val labels = new Array[LabelNode](labelMap.size())

        var i = 0
        for (entry <- labelMap.entrySet()) {
          keys(i) = entry.getKey
          labels(i) = entry.getValue
          i += 1
        }

        new LookupSwitchInsnNode(default, keys, labels)
      }
      case AbstractInsnNode.MULTIANEWARRAY_INSN => new MultiANewArrayInsnNode(args(0), args(1).toInt)
    }
  }

  private def parseSwitchInsn(args: Array[String]): (util.Map[Int, LabelNode], LabelNode) = {
    val labels = new util.HashMap[Int, LabelNode]()
    var default: LabelNode = null

    var i = 0
    while (default == null) {
      if (i + 1 >= args.length) throw new IllegalArgumentException("Missing default label!")
      args(i) match {
        case "default" => default = parseLabel(args(i + 1))
        case value if (value.matches("-?\\d+")) => labels.put(value.toInt, parseLabel(args(i + 1)))
      }
      i += 2
    }
    (labels, default)
  }
}

object AsmBlockParser {
  private def castLdcArg(arg: String): Any = arg.toLowerCase match {
    case arg @ "*" => null
    case arg if (arg.startsWith("\"") && arg.endsWith("\"")) => arg.drop(1).dropRight(1)
    case arg if (arg.endsWith("l")) => arg.dropRight(1).toLong
    case arg if (arg.endsWith("f")) => arg.dropRight(1).toFloat
    case arg if (arg.endsWith("d")) => arg.dropRight(1).toDouble
    case arg if (arg.contains(".")) => arg.toDouble
    case arg => arg.toInt
  }

  def numArgs(insnNode: AbstractInsnNode, args: String) = insnNode.getOpcode match {
    case AbstractInsnNode.INSN => 0
    case AbstractInsnNode.INT_INSN | AbstractInsnNode.VAR_INSN | AbstractInsnNode.TYPE_INSN => 1
    //TODO!
  }
}