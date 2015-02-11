package org.lolhens.inject

import java.util

import org.lolhens.inject.Opcode._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._

import scala.collection.JavaConversions._

/**
 * Created by LolHens on 14.12.2014.
 */
abstract class Opcode(val name: String,
                      val opcode: Int = -1,
                      val optype: Int) {
  opcodes.add(this)

  def numArgs(args: String): Int

  def parse(args: Array[String]): AbstractInsnNode

  override def toString = name
}

object Opcode {
  private val opcodes = new util.ArrayList[Opcode]()

  class INSN(name: String, opcode: Int) extends Opcode(name, opcode, AbstractInsnNode.INSN) {
    override def numArgs(args: String) = 0

    override def parse(args: Array[String]) = new InsnNode(opcode)
  }
  class INT_INSN(name: String, opcode: Int) extends Opcode(name, opcode, AbstractInsnNode.INT_INSN) {
    override def numArgs(args: String) = 1

    override def parse(args: Array[String]) = new IntInsnNode(opcode, args(0).toInt)
  }
  class VAR_INSN(name: String, opcode: Int) extends Opcode(name, opcode, AbstractInsnNode.VAR_INSN) {
    override def numArgs(args: String) = 1

    override def parse(args: Array[String]) = new VarInsnNode(opcode, args(0).toInt)
  }
  class FIELD_INSN(name: String, opcode: Int) extends Opcode(name, opcode, AbstractInsnNode.FIELD_INSN) {
    override def numArgs(args: String) = 2

    override def parse(args: Array[String]) = new FieldInsnNode(opcode, args(0), args(1), args(2))
  }
  class METHOD_INSN(name: String, opcode: Int) extends Opcode(name, opcode, AbstractInsnNode.METHOD_INSN) {
    override def numArgs(args: String) = 2

    override def parse(args: Array[String]) = new MethodInsnNode(opcode, args(0), args(1), args(2), opcode == Opcodes.INVOKEINTERFACE)
  }
  class TYPE_INSN(name: String, opcode: Int) extends Opcode(name, opcode, AbstractInsnNode.TYPE_INSN) {
    override def numArgs(args: String) = 1

    override def parse(args: Array[String]) = new TypeInsnNode(opcode, args(0))
  }
  class JUMP_INSN(name: String, opcode: Int) extends Opcode(name, opcode, AbstractInsnNode.JUMP_INSN) {
    override def numArgs(args: String) = 1

    override def parse(args: Array[String]) = new JumpInsnNode(opcode, LABEL.parseLabel(args(0)))
  }
  class SWITCH_INSN(name: String, opcode: Int, optype: Int) extends Opcode(name, opcode, optype) {
    override def numArgs(args: String) = {
      val args = reformatAsm(_args)
      val split = args.split(" ")

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

  object LABEL extends Opcode("label", -1, AbstractInsnNode.LABEL) {
    override def numArgs(args: String) = 0

    override def parse(args: Array[String]) = ???

    private def parseLabel(arg: String): LabelNode = {
      if (arg.toLowerCase.startsWith("l") && arg.drop(1).matches("-?\\d+"))
        ??? //label (arg.drop(1).toInt)
      else
        null
    }
  }
  object NOP extends INSN("nop", Opcodes.NOP)
  object ACONST_NULL extends INSN("aconst_null", Opcodes.ACONST_NULL)
  object ICONST_M1 extends INSN("iconst_m1", Opcodes.ICONST_M1)
  object ICONST_0 extends INSN("iconst_0", Opcodes.ICONST_0)
  object ICONST_1 extends INSN("iconst_1", Opcodes.ICONST_1)
  object ICONST_2 extends INSN("iconst_2", Opcodes.ICONST_2)
  object ICONST_3 extends INSN("iconst_3", Opcodes.ICONST_3)
  object ICONST_4 extends INSN("iconst_4", Opcodes.ICONST_4)
  object ICONST_5 extends INSN("iconst_5", Opcodes.ICONST_5)
  object LCONST_0 extends INSN("lconst_0", Opcodes.LCONST_0)
  object LCONST_1 extends INSN("lconst_1", Opcodes.LCONST_1)
  object FCONST_0 extends INSN("fconst_0", Opcodes.FCONST_0)
  object FCONST_1 extends INSN("fconst_1", Opcodes.FCONST_1)
  object FCONST_2 extends INSN("fconst_2", Opcodes.FCONST_2)
  object DCONST_0 extends INSN("dconst_0", Opcodes.DCONST_0)
  object DCONST_1 extends INSN("dconst_1", Opcodes.DCONST_1)
  object BIPUSH extends INT_INSN("bipush", Opcodes.BIPUSH)
  object SIPUSH extends INT_INSN("sipush", Opcodes.SIPUSH)
  object LDC extends Opcode("ldc", Opcodes.LDC, AbstractInsnNode.LDC_INSN) {
    override def numArgs(args: String) = 1

    override def parse(args: Array[String]) = new LdcInsnNode(castLdcArg(args(0), null)) //TODO

    private def castLdcArg(arg: String, strings: util.List[String] = null): Any = arg.toLowerCase match {
      case arg@"*" => null
      case arg if arg.startsWith("\"") && arg.endsWith("\"") => arg.drop(1).dropRight(1) //unused (use string map)
      case arg if arg.endsWith("s") => if (strings == null) null else strings.get(arg.dropRight(1).toInt)
      case arg if arg.endsWith("l") => arg.dropRight(1).toLong
      case arg if arg.endsWith("f") => arg.dropRight(1).toFloat
      case arg if arg.endsWith("d") => arg.dropRight(1).toDouble
      case arg if arg.contains(".") => arg.toDouble
      case arg => arg.toInt
    }
  }
  object ILOAD extends VAR_INSN("iload", Opcodes.ILOAD)
  object LLOAD extends VAR_INSN("lload", Opcodes.LLOAD)
  object FLOAD extends VAR_INSN("fload", Opcodes.FLOAD)
  object DLOAD extends VAR_INSN("dload", Opcodes.DLOAD)
  object ALOAD extends VAR_INSN("aload", Opcodes.ALOAD)
  object IALOAD extends INSN("iaload", Opcodes.IALOAD)
  object LALOAD extends INSN("laload", Opcodes.LALOAD)
  object FALOAD extends INSN("faload", Opcodes.FALOAD)
  object DALOAD extends INSN("daload", Opcodes.DALOAD)
  object AALOAD extends INSN("aaload", Opcodes.AALOAD)
  object BALOAD extends INSN("baload", Opcodes.BALOAD)
  object CALOAD extends INSN("caload", Opcodes.CALOAD)
  object SALOAD extends INSN("saload", Opcodes.SALOAD)
  object ISTORE extends VAR_INSN("istore", Opcodes.ISTORE)
  object LSTORE extends VAR_INSN("lstore", Opcodes.LSTORE)
  object FSTORE extends VAR_INSN("fstore", Opcodes.FSTORE)
  object DSTORE extends VAR_INSN("dstore", Opcodes.DSTORE)
  object ASTORE extends VAR_INSN("astore", Opcodes.ASTORE)
  object IASTORE extends INSN("iastore", Opcodes.IASTORE)
  object LASTORE extends INSN("lastore", Opcodes.LASTORE)
  object FASTORE extends INSN("fastore", Opcodes.FASTORE)
  object DASTORE extends INSN("dastore", Opcodes.DASTORE)
  object AASTORE extends INSN("aastore", Opcodes.AASTORE)
  object BASTORE extends INSN("bastore", Opcodes.BASTORE)
  object CASTORE extends INSN("castore", Opcodes.CASTORE)
  object SASTORE extends INSN("sastore", Opcodes.SASTORE)
  object POP extends INSN("pop", Opcodes.POP)
  object POP2 extends INSN("pop2", Opcodes.POP2)
  object DUP extends INSN("dup", Opcodes.DUP)
  object DUP_X1 extends INSN("dup_x1", Opcodes.DUP_X1)
  object DUP_X2 extends INSN("dup_x2", Opcodes.DUP_X2)
  object DUP2 extends INSN("dup2", Opcodes.DUP2)
  object DUP2_X1 extends INSN("dup2_x1", Opcodes.DUP2_X1)
  object DUP2_X2 extends INSN("dup2_x2", Opcodes.DUP2_X2)
  object SWAP extends INSN("swap", Opcodes.SWAP)
  object IADD extends INSN("iadd", Opcodes.IADD)
  object LADD extends INSN("ladd", Opcodes.LADD)
  object FADD extends INSN("fadd", Opcodes.FADD)
  object DADD extends INSN("dadd", Opcodes.DADD)
  object ISUB extends INSN("isub", Opcodes.ISUB)
  object LSUB extends INSN("lsub", Opcodes.LSUB)
  object FSUB extends INSN("fsub", Opcodes.FSUB)
  object DSUB extends INSN("dsub", Opcodes.DSUB)
  object IMUL extends INSN("imul", Opcodes.IMUL)
  object LMUL extends INSN("lmul", Opcodes.LMUL)
  object FMUL extends INSN("fmul", Opcodes.FMUL)
  object DMUL extends INSN("dmul", Opcodes.DMUL)
  object IDIV extends INSN("idiv", Opcodes.IDIV)
  object LDIV extends INSN("ldiv", Opcodes.LDIV)
  object FDIV extends INSN("fdiv", Opcodes.FDIV)
  object DDIV extends INSN("ddiv", Opcodes.DDIV)
  object IREM extends INSN("irem", Opcodes.IREM)
  object LREM extends INSN("lrem", Opcodes.LREM)
  object FREM extends INSN("frem", Opcodes.FREM)
  object DREM extends INSN("drem", Opcodes.DREM)
  object INEG extends INSN("ineg", Opcodes.INEG)
  object LNEG extends INSN("lneg", Opcodes.LNEG)
  object FNEG extends INSN("fneg", Opcodes.FNEG)
  object DNEG extends INSN("dneg", Opcodes.DNEG)
  object ISHL extends INSN("ishl", Opcodes.ISHL)
  object LSHL extends INSN("lshl", Opcodes.LSHL)
  object ISHR extends INSN("ishr", Opcodes.ISHR)
  object LSHR extends INSN("lshr", Opcodes.LSHR)
  object IUSHR extends INSN("iushr", Opcodes.IUSHR)
  object LUSHR extends INSN("lushr", Opcodes.LUSHR)
  object IAND extends INSN("iand", Opcodes.IAND)
  object LAND extends INSN("land", Opcodes.LAND)
  object IOR extends INSN("ior", Opcodes.IOR)
  object LOR extends INSN("lor", Opcodes.LOR)
  object IXOR extends INSN("ixor", Opcodes.IXOR)
  object LXOR extends INSN("lxor", Opcodes.LXOR)
  object IINC extends Opcode("iinc", Opcodes.IINC, AbstractInsnNode.IINC_INSN) {
    override def numArgs(args: String) = 2

    override def parse(args: Array[String]) = new IincInsnNode(args(0).toInt, args(1).toInt)
  }
  object I2L extends INSN("i2l", Opcodes.I2L)
  object I2F extends INSN("i2f", Opcodes.I2F)
  object I2D extends INSN("i2d", Opcodes.I2D)
  object L2I extends INSN("l2i", Opcodes.L2I)
  object L2F extends INSN("l2f", Opcodes.L2F)
  object L2D extends INSN("l2d", Opcodes.L2D)
  object F2I extends INSN("f2i", Opcodes.F2I)
  object F2L extends INSN("f2l", Opcodes.F2L)
  object F2D extends INSN("f2d", Opcodes.F2D)
  object D2I extends INSN("d2i", Opcodes.D2I)
  object D2L extends INSN("d2l", Opcodes.D2L)
  object D2F extends INSN("d2f", Opcodes.D2F)
  object I2B extends INSN("i2b", Opcodes.I2B)
  object I2C extends INSN("i2c", Opcodes.I2C)
  object I2S extends INSN("i2s", Opcodes.I2S)
  object LCMP extends INSN("lcmp", Opcodes.LCMP)
  object FCMPL extends INSN("fcmpl", Opcodes.FCMPL)
  object FCMPG extends INSN("fcmpg", Opcodes.FCMPG)
  object DCMPL extends INSN("dcmpl", Opcodes.DCMPL)
  object DCMPG extends INSN("dcmpg", Opcodes.DCMPG)
  object IFEQ extends JUMP_INSN("ifeq", Opcodes.IFEQ)
  object IFNE extends JUMP_INSN("ifne", Opcodes.IFNE)
  object IFLT extends JUMP_INSN("iflt", Opcodes.IFLT)
  object IFGE extends JUMP_INSN("ifge", Opcodes.IFGE)
  object IFGT extends JUMP_INSN("ifgt", Opcodes.IFGT)
  object IFLE extends JUMP_INSN("ifle", Opcodes.IFLE)
  object IF_ICMPEQ extends JUMP_INSN("if_icmpeq", Opcodes.IF_ICMPEQ)
  object IF_ICMPNE extends JUMP_INSN("if_icmpne", Opcodes.IF_ICMPNE)
  object IF_ICMPLT extends JUMP_INSN("if_icmplt", Opcodes.IF_ICMPLT)
  object IF_ICMPGE extends JUMP_INSN("if_icmpge", Opcodes.IF_ICMPGE)
  object IF_ICMPGT extends JUMP_INSN("if_icmpgt", Opcodes.IF_ICMPGT)
  object IF_ICMPLE extends JUMP_INSN("if_icmple", Opcodes.IF_ICMPLE)
  object IF_ACMPEQ extends JUMP_INSN("if_acmpeq", Opcodes.IF_ACMPEQ)
  object IF_ACMPNE extends JUMP_INSN("if_acmpne", Opcodes.IF_ACMPNE)
  object GOTO extends JUMP_INSN("goto", Opcodes.GOTO)
  object JSR extends JUMP_INSN("jsr", Opcodes.JSR)
  object RET extends VAR_INSN("ret", Opcodes.RET)
  object TABLESWITCH extends SWITCH_INSN("tableswitch", Opcodes.TABLESWITCH, AbstractInsnNode.TABLESWITCH_INSN)
  object LOOKUPSWITCH extends SWITCH_INSN("lookupswitch", Opcodes.LOOKUPSWITCH, AbstractInsnNode.LOOKUPSWITCH_INSN)
  object IRETURN extends INSN("ireturn", Opcodes.IRETURN)
  object LRETURN extends INSN("lreturn", Opcodes.LRETURN)
  object FRETURN extends INSN("freturn", Opcodes.FRETURN)
  object DRETURN extends INSN("dreturn", Opcodes.DRETURN)
  object ARETURN extends INSN("areturn", Opcodes.ARETURN)
  object RETURN extends INSN("return", Opcodes.RETURN)
  object GETSTATIC extends FIELD_INSN("getstatic", Opcodes.GETSTATIC)
  object PUTSTATIC extends FIELD_INSN("putstatic", Opcodes.PUTSTATIC)
  object GETFIELD extends FIELD_INSN("getfield", Opcodes.GETFIELD)
  object PUTFIELD extends FIELD_INSN("putfield", Opcodes.PUTFIELD)
  object INVOKEVIRTUAL extends METHOD_INSN("invokevirtual", Opcodes.INVOKEVIRTUAL)
  object INVOKESPECIAL extends METHOD_INSN("invokespecial", Opcodes.INVOKESPECIAL)
  object INVOKESTATIC extends METHOD_INSN("invokestatic", Opcodes.INVOKESTATIC)
  object INVOKEINTERFACE extends METHOD_INSN("invokeinterface", Opcodes.INVOKEINTERFACE)
  object INVOKEDYNAMIC extends Opcode("invokedynamic", Opcodes.INVOKEDYNAMIC, AbstractInsnNode.INVOKE_DYNAMIC_INSN) {
    override def numArgs(args: String) = ???

    override def parse(args: Array[String]) = ???
  }
  object NEW extends TYPE_INSN("new", Opcodes.NEW)
  object NEWARRAY extends INT_INSN("newarray", Opcodes.NEWARRAY)
  object ANEWARRAY extends TYPE_INSN("anewarray", Opcodes.ANEWARRAY)
  object ARRAYLENGTH extends INSN("arraylength", Opcodes.ARRAYLENGTH)
  object ATHROW extends INSN("athrow", Opcodes.ATHROW)
  object CHECKCAST extends TYPE_INSN("checkcast", Opcodes.CHECKCAST)
  object INSTANCEOF extends TYPE_INSN("instanceof", Opcodes.INSTANCEOF)
  object MONITORENTER extends INSN("monitorenter", Opcodes.MONITORENTER)
  object MONITOREXIT extends INSN("monitorexit", Opcodes.MONITOREXIT)
  object MULTIANEWARRAY extends Opcode("multianewarray", Opcodes.MULTIANEWARRAY, AbstractInsnNode.MULTIANEWARRAY_INSN) {
    override def numArgs(args: String) = 2

    override def parse(args: Array[String]) = new MultiANewArrayInsnNode(args(0), args(1).toInt)
  }
  object IFNULL extends JUMP_INSN("ifnull", Opcodes.IFNULL)
  object IFNONNULL extends JUMP_INSN("ifnonnull", Opcodes.IFNONNULL)

  def apply(opcode: Int): Opcode = {
    if (opcode < 0) return null
    for (op <- opcodes) if (op.opcode == opcode) return op
    null
  }

  def apply(name: String): Opcode = {
    val opName = name.toLowerCase
    for (opcode <- opcodes) if (opName == opcode.name) return opcode
    null
  }
}