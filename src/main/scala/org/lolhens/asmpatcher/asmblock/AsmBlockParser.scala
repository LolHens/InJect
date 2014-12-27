package org.lolhens.asmpatcher.asmblock

import java.util
import java.util.Comparator
import java.util.Map.Entry

import org.lolhens.asmpatcher.Opcode
import org.lolhens.asmpatcher.asmblock.AsmBlockParser._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._

import scala.collection.JavaConversions._

/**
 * Created by LolHens on 23.12.2014.
 */
class AsmBlockParser(val asmBlock: AsmBlock = new AsmBlock()) {
  private val strings = new util.ArrayList[String]()

  def parseInsns(_asm: String): util.List[AbstractInsnNode] = {
    var asm = reformatAsm(_asm)
    val ret = new util.ArrayList[AbstractInsnNode]()

    while (asm != "") {
      val insnNode = parseInsn(asm)
      if (insnNode == null) throw new IllegalArgumentException(s"Unable to parse intruction: $asm")
      ret.add(insnNode)

      val opcode_other = forceSplit(asm, " ", 2)
      val args = numArgs(insnNode.getOpcode, opcode_other(1))
      asm = forceSplit(opcode_other(1), " ", 1 + args).last
    }
    ret
  }

  def parseInsn(_insn: String): AbstractInsnNode = {
    val insn = reformatAsm(_insn)

    val opcode_args = forceSplit(insn, " ", 2)
    val opcode = Opcode(opcode_args(0))
    if (opcode != null) return parseInsnArgs(opcode, opcode_args(1))

    val labelNode = parseLabel(opcode_args(0))
    if (labelNode != null) return labelNode

    null
  }

  private def parseLabel(arg: String): LabelNode = {
    if (arg.toLowerCase.startsWith("l") && arg.drop(1).matches("-?\\d+"))
      asmBlock.label(arg.drop(1).toInt)
    else
      null
  }

  private def parseInsnArgs(opcode: Opcode, args: String): AbstractInsnNode = {
    val split = args.split(" ")

    opcode.optype match {
      case AbstractInsnNode.FIELD_INSN
           | AbstractInsnNode.METHOD_INSN => {
        val owner_name = split(0).split("\\.")
        val desc = split(1)
        parseSeparatedInsnArgs(opcode, Array(owner_name(0), owner_name(1), desc))
      }
      case _ => parseSeparatedInsnArgs(opcode, split)
    }
  }

  private def parseSeparatedInsnArgs(opcode: Opcode, args: Array[String]): AbstractInsnNode = {
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
      case AbstractInsnNode.IINC_INSN => new IincInsnNode(args(0).toInt, args(1).toInt)
      case AbstractInsnNode.LABEL => ???
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

        val labelList = new util.LinkedList[util.Map.Entry[Int, LabelNode]]
        labelList.addAll(labelMap.entrySet())
        labelList.sort(new Comparator[Entry[Int, LabelNode]] {
          override def compare(o1: Entry[Int, LabelNode], o2: Entry[Int, LabelNode]): Int = o1.getKey - o2.getKey
        })

        var i = 0
        for (entry <- labelList) {
          keys(i) = entry.getKey
          labels(i) = entry.getValue
          i += 1
        }

        new LookupSwitchInsnNode(default, keys, labels)
      }
      case AbstractInsnNode.MULTIANEWARRAY_INSN => new MultiANewArrayInsnNode(args(0), args(1).toInt)
    }
  }

  private def castLdcArg(arg: String): Any = arg.toLowerCase match {
    case arg @ "*" => null
    case arg if (arg.startsWith("\"") && arg.endsWith("\"")) => arg.drop(1).dropRight(1) //unused (use string map)
    case arg if (arg.endsWith("s")) => strings.get(arg.dropRight(1).toInt)
    case arg if (arg.endsWith("l")) => arg.dropRight(1).toLong
    case arg if (arg.endsWith("f")) => arg.dropRight(1).toFloat
    case arg if (arg.endsWith("d")) => arg.dropRight(1).toDouble
    case arg if (arg.contains(".")) => arg.toDouble
    case arg => println(arg) //arg.toInt
  }

  private def parseSwitchInsn(args: Array[String]): (util.Map[Int, LabelNode], LabelNode) = {
    val labels = new util.HashMap[Int, LabelNode]()
    var default: LabelNode = null

    var i = 0
    while (default == null) {
      if (i + 1 >= args.length) throw new IllegalArgumentException("Missing default label!")
      args(i).toLowerCase match {
        case "default" => default = parseLabel(args(i + 1))
        case value if (value.matches("-?\\d+")) => labels.put(value.toInt, parseLabel(args(i + 1)))
      }
      i += 2
    }
    (labels, default)
  }

  private def reformatAsm(_args: String): String = {
    var args = _args
    while (args.contains("\"")) {
      val start = args.indexOf("\"")
      val end = args.indexOf("\"", start + 1)
      if (end == -1) throw new IllegalArgumentException(s"Incomplete string: $args")
      val index = strings.size()
      strings.add(args.substring(start + 1, end))
      args = s"${args.substring(0, start)}${index}s${args.substring(end + 1)}"
    }
    args
      .replaceAll("\r\n|\r|\n", " ")
      .replaceAll(":", " ")

      .trim
      .replaceAll(" +", " ")
  }

  def numArgs(insn: Int, _args: String): Int = {
    val args = reformatAsm(_args)
    val split = args.split(" ")

    Opcode(insn).optype match {
      case AbstractInsnNode.INSN
           | AbstractInsnNode.LABEL => 0
      case AbstractInsnNode.INT_INSN
           | AbstractInsnNode.VAR_INSN
           | AbstractInsnNode.TYPE_INSN
           | AbstractInsnNode.JUMP_INSN
           | AbstractInsnNode.LDC_INSN => 1
      case AbstractInsnNode.FIELD_INSN
           | AbstractInsnNode.METHOD_INSN
           | AbstractInsnNode.MULTIANEWARRAY_INSN
           | AbstractInsnNode.IINC_INSN => 2
      case AbstractInsnNode.INVOKE_DYNAMIC_INSN => ???
      case AbstractInsnNode.TABLESWITCH_INSN | AbstractInsnNode.LOOKUPSWITCH_INSN => {
        var i = 0
        while (i + 1 < args.length) {
          if (split(i).toLowerCase == "default") {
            if (i % 2 == 1) throw new IllegalArgumentException(s"Illegal switch parameters: $args")
            return i + 2
          }
          i += 1
        }
        throw new IllegalArgumentException(s"Missing default label: $args")
      }
    }
  }
}

object AsmBlockParser {
  private def forceSplit(string: String, regex: String, limit: Int): Array[String] = {
    val split = string.split(regex, limit)
    if (split.length < limit) {
      val newSplit = new Array[String](limit)
      for (i <- 0 until newSplit.length) newSplit(i) = if (i < split.length) split(i) else ""
      newSplit
    } else {
      split
    }
  }

  def parseAsmBlock(asm: String) = {
    val parser = new AsmBlockParser()
    val insns = parser.parseInsns(asm)
    for (insn <- insns) parser.asmBlock.add(insn)
    parser.asmBlock
  }
}