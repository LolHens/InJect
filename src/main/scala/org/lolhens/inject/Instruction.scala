package org.lolhens.inject

import java.util

import org.lolhens.inject.Instruction.Opcode
import org.lolhens.inject.Instruction.Opcode._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._

import scala.collection.mutable

/**
 * Created by LolHens on 14.12.2014.
 */
abstract class Instruction(val opcode: Opcode) {
  def toAbstractInsnNode: AbstractInsnNode
}

object Instruction {
  def parse(string: String): AbstractInsnNode = {
    val split = string.split(" ", 2)
    Opcode(split(0)) match {
      case Some(opcode) => ??? //opcode.parse(split(2))
      case None => throw new NoSuchElementException()
    }
  }

  class Opcode(val name: String, val id: Int = -1)(val `type`: Int) {
    opcodes += this
  }

  object Opcode {
    private val opcodes = mutable.MutableList[Opcode]()

    def apply(opcode: Int): Option[Opcode] = if (opcode < 0) None else opcodes.find(_.id == opcode)

    def apply(_name: String): Option[Opcode] = {
      val name = _name.toLowerCase
      opcodes.find(_.name == name)
    }
  }

  private type _Opcode = (Int) => Opcode

  abstract class INSN(_opcode: _Opcode) extends Instruction(_opcode(AbstractInsnNode.INSN)) {
    override def toAbstractInsnNode = new InsnNode(opcode.id)
  }
  abstract class INT_INSN(_opcode: _Opcode, val int: Int) extends Instruction(_opcode(AbstractInsnNode.INT_INSN)) {
    override def toAbstractInsnNode = new IntInsnNode(opcode.id, int)
  }
  abstract class VAR_INSN(_opcode: _Opcode, val `var`: Int) extends Instruction(_opcode(AbstractInsnNode.VAR_INSN)) {
    override def toAbstractInsnNode = new VarInsnNode(opcode.id, `var`)
  }
  abstract class FIELD_INSN(_opcode: _Opcode, val owner: String, val name: String, val desc: String) extends Instruction(_opcode(AbstractInsnNode.FIELD_INSN)) {
    override def toAbstractInsnNode = new FieldInsnNode(opcode.id, owner, name, desc)
  }
  abstract class METHOD_INSN(_opcode: _Opcode, val owner: String, val name: String, val desc: String, val interface: Boolean) extends Instruction(_opcode(AbstractInsnNode.METHOD_INSN)) {
    override def toAbstractInsnNode = new MethodInsnNode(opcode.id, owner, name, desc, interface)
  }
  abstract class TYPE_INSN(_opcode: _Opcode, val desc: String) extends Instruction(_opcode(AbstractInsnNode.TYPE_INSN)) {
    override def toAbstractInsnNode = new TypeInsnNode(opcode.id, desc)
  }
  abstract class JUMP_INSN(_opcode: _Opcode, val label: LabelNode) extends Instruction(_opcode(AbstractInsnNode.JUMP_INSN)) {
    override def toAbstractInsnNode = new JumpInsnNode(opcode.id, label)
  }
  abstract class SWITCH_INSN(opcode: Opcode) extends Instruction(opcode) {}

  class LABEL extends Instruction(new Opcode("label")(AbstractInsnNode.LABEL)) {
    private def parseLabel(arg: String): LabelNode = {
      if (arg.toLowerCase.startsWith("l") && arg.drop(1).matches("-?\\d+"))
        ??? //label (arg.drop(1).toInt)
      else
        null
    }

    override def toAbstractInsnNode: AbstractInsnNode = new LabelNode()
  }
  class NOP extends INSN(new Opcode("nop", Opcodes.NOP)(_))
  class ACONST_NULL extends INSN(new Opcode("aconst_null", Opcodes.ACONST_NULL)(_))
  class ICONST_M1 extends INSN(new Opcode("iconst_m1", Opcodes.ICONST_M1)(_))
  class ICONST_0 extends INSN(new Opcode("iconst_0", Opcodes.ICONST_0)(_))
  class ICONST_1 extends INSN(new Opcode("iconst_1", Opcodes.ICONST_1)(_))
  class ICONST_2 extends INSN(new Opcode("iconst_2", Opcodes.ICONST_2)(_))
  class ICONST_3 extends INSN(new Opcode("iconst_3", Opcodes.ICONST_3)(_))
  class ICONST_4 extends INSN(new Opcode("iconst_4", Opcodes.ICONST_4)(_))
  class ICONST_5 extends INSN(new Opcode("iconst_5", Opcodes.ICONST_5)(_))
  class LCONST_0 extends INSN(new Opcode("lconst_0", Opcodes.LCONST_0)(_))
  class LCONST_1 extends INSN(new Opcode("lconst_1", Opcodes.LCONST_1)(_))
  class FCONST_0 extends INSN(new Opcode("fconst_0", Opcodes.FCONST_0)(_))
  class FCONST_1 extends INSN(new Opcode("fconst_1", Opcodes.FCONST_1)(_))
  class FCONST_2 extends INSN(new Opcode("fconst_2", Opcodes.FCONST_2)(_))
  class DCONST_0 extends INSN(new Opcode("dconst_0", Opcodes.DCONST_0)(_))
  class DCONST_1 extends INSN(new Opcode("dconst_1", Opcodes.DCONST_1)(_))
  class BIPUSH(int: Int) extends INT_INSN(new Opcode("bipush", Opcodes.BIPUSH)(_), int)
  class SIPUSH(int: Int) extends INT_INSN(new Opcode("sipush", Opcodes.SIPUSH)(_), int)
  class LDC(obj: AnyRef) extends Instruction(new Opcode("ldc", Opcodes.LDC)(AbstractInsnNode.LDC_INSN)) {
    override def toAbstractInsnNode = new LdcInsnNode(obj)

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
  class ILOAD(`var`: Int) extends VAR_INSN(new Opcode("iload", Opcodes.ILOAD)(_), `var`)
  class LLOAD(`var`: Int) extends VAR_INSN(new Opcode("lload", Opcodes.LLOAD)(_), `var`)
  class FLOAD(`var`: Int) extends VAR_INSN(new Opcode("fload", Opcodes.FLOAD)(_), `var`)
  class DLOAD(`var`: Int) extends VAR_INSN(new Opcode("dload", Opcodes.DLOAD)(_), `var`)
  class ALOAD(`var`: Int) extends VAR_INSN(new Opcode("aload", Opcodes.ALOAD)(_), `var`)
  class IALOAD extends INSN(new Opcode("iaload", Opcodes.IALOAD)(_))
  class LALOAD extends INSN(new Opcode("laload", Opcodes.LALOAD)(_))
  class FALOAD extends INSN(new Opcode("faload", Opcodes.FALOAD)(_))
  class DALOAD extends INSN(new Opcode("daload", Opcodes.DALOAD)(_))
  class AALOAD extends INSN(new Opcode("aaload", Opcodes.AALOAD)(_))
  class BALOAD extends INSN(new Opcode("baload", Opcodes.BALOAD)(_))
  class CALOAD extends INSN(new Opcode("caload", Opcodes.CALOAD)(_))
  class SALOAD extends INSN(new Opcode("saload", Opcodes.SALOAD)(_))
  class ISTORE(`var`: Int) extends VAR_INSN(new Opcode("istore", Opcodes.ISTORE)(_), `var`)
  class LSTORE(`var`: Int) extends VAR_INSN(new Opcode("lstore", Opcodes.LSTORE)(_), `var`)
  class FSTORE(`var`: Int) extends VAR_INSN(new Opcode("fstore", Opcodes.FSTORE)(_), `var`)
  class DSTORE(`var`: Int) extends VAR_INSN(new Opcode("dstore", Opcodes.DSTORE)(_), `var`)
  class ASTORE(`var`: Int) extends VAR_INSN(new Opcode("astore", Opcodes.ASTORE)(_), `var`)
  class IASTORE extends INSN(new Opcode("iastore", Opcodes.IASTORE)(_))
  class LASTORE extends INSN(new Opcode("lastore", Opcodes.LASTORE)(_))
  class FASTORE extends INSN(new Opcode("fastore", Opcodes.FASTORE)(_))
  class DASTORE extends INSN(new Opcode("dastore", Opcodes.DASTORE)(_))
  class AASTORE extends INSN(new Opcode("aastore", Opcodes.AASTORE)(_))
  class BASTORE extends INSN(new Opcode("bastore", Opcodes.BASTORE)(_))
  class CASTORE extends INSN(new Opcode("castore", Opcodes.CASTORE)(_))
  class SASTORE extends INSN(new Opcode("sastore", Opcodes.SASTORE)(_))
  class POP extends INSN(new Opcode("pop", Opcodes.POP)(_))
  class POP2 extends INSN(new Opcode("pop2", Opcodes.POP2)(_))
  class DUP extends INSN(new Opcode("dup", Opcodes.DUP)(_))
  class DUP_X1 extends INSN(new Opcode("dup_x1", Opcodes.DUP_X1)(_))
  class DUP_X2 extends INSN(new Opcode("dup_x2", Opcodes.DUP_X2)(_))
  class DUP2 extends INSN(new Opcode("dup2", Opcodes.DUP2)(_))
  class DUP2_X1 extends INSN(new Opcode("dup2_x1", Opcodes.DUP2_X1)(_))
  class DUP2_X2 extends INSN(new Opcode("dup2_x2", Opcodes.DUP2_X2)(_))
  class SWAP extends INSN(new Opcode("swap", Opcodes.SWAP)(_))
  class IADD extends INSN(new Opcode("iadd", Opcodes.IADD)(_))
  class LADD extends INSN(new Opcode("ladd", Opcodes.LADD)(_))
  class FADD extends INSN(new Opcode("fadd", Opcodes.FADD)(_))
  class DADD extends INSN(new Opcode("dadd", Opcodes.DADD)(_))
  class ISUB extends INSN(new Opcode("isub", Opcodes.ISUB)(_))
  class LSUB extends INSN(new Opcode("lsub", Opcodes.LSUB)(_))
  class FSUB extends INSN(new Opcode("fsub", Opcodes.FSUB)(_))
  class DSUB extends INSN(new Opcode("dsub", Opcodes.DSUB)(_))
  class IMUL extends INSN(new Opcode("imul", Opcodes.IMUL)(_))
  class LMUL extends INSN(new Opcode("lmul", Opcodes.LMUL)(_))
  class FMUL extends INSN(new Opcode("fmul", Opcodes.FMUL)(_))
  class DMUL extends INSN(new Opcode("dmul", Opcodes.DMUL)(_))
  class IDIV extends INSN(new Opcode("idiv", Opcodes.IDIV)(_))
  class LDIV extends INSN(new Opcode("ldiv", Opcodes.LDIV)(_))
  class FDIV extends INSN(new Opcode("fdiv", Opcodes.FDIV)(_))
  class DDIV extends INSN(new Opcode("ddiv", Opcodes.DDIV)(_))
  class IREM extends INSN(new Opcode("irem", Opcodes.IREM)(_))
  class LREM extends INSN(new Opcode("lrem", Opcodes.LREM)(_))
  class FREM extends INSN(new Opcode("frem", Opcodes.FREM)(_))
  class DREM extends INSN(new Opcode("drem", Opcodes.DREM)(_))
  class INEG extends INSN(new Opcode("ineg", Opcodes.INEG)(_))
  class LNEG extends INSN(new Opcode("lneg", Opcodes.LNEG)(_))
  class FNEG extends INSN(new Opcode("fneg", Opcodes.FNEG)(_))
  class DNEG extends INSN(new Opcode("dneg", Opcodes.DNEG)(_))
  class ISHL extends INSN(new Opcode("ishl", Opcodes.ISHL)(_))
  class LSHL extends INSN(new Opcode("lshl", Opcodes.LSHL)(_))
  class ISHR extends INSN(new Opcode("ishr", Opcodes.ISHR)(_))
  class LSHR extends INSN(new Opcode("lshr", Opcodes.LSHR)(_))
  class IUSHR extends INSN(new Opcode("iushr", Opcodes.IUSHR)(_))
  class LUSHR extends INSN(new Opcode("lushr", Opcodes.LUSHR)(_))
  class IAND extends INSN(new Opcode("iand", Opcodes.IAND)(_))
  class LAND extends INSN(new Opcode("land", Opcodes.LAND)(_))
  class IOR extends INSN(new Opcode("ior", Opcodes.IOR)(_))
  class LOR extends INSN(new Opcode("lor", Opcodes.LOR)(_))
  class IXOR extends INSN(new Opcode("ixor", Opcodes.IXOR)(_))
  class LXOR extends INSN(new Opcode("lxor", Opcodes.LXOR)(_))
  class IINC(`var`: Int, incr: Int) extends Instruction(new Opcode("iinc", Opcodes.IINC)(AbstractInsnNode.IINC_INSN)) {
    override def toAbstractInsnNode = new IincInsnNode(`var`, incr)
  }
  class I2L extends INSN(new Opcode("i2l", Opcodes.I2L)(_))
  class I2F extends INSN(new Opcode("i2f", Opcodes.I2F)(_))
  class I2D extends INSN(new Opcode("i2d", Opcodes.I2D)(_))
  class L2I extends INSN(new Opcode("l2i", Opcodes.L2I)(_))
  class L2F extends INSN(new Opcode("l2f", Opcodes.L2F)(_))
  class L2D extends INSN(new Opcode("l2d", Opcodes.L2D)(_))
  class F2I extends INSN(new Opcode("f2i", Opcodes.F2I)(_))
  class F2L extends INSN(new Opcode("f2l", Opcodes.F2L)(_))
  class F2D extends INSN(new Opcode("f2d", Opcodes.F2D)(_))
  class D2I extends INSN(new Opcode("d2i", Opcodes.D2I)(_))
  class D2L extends INSN(new Opcode("d2l", Opcodes.D2L)(_))
  class D2F extends INSN(new Opcode("d2f", Opcodes.D2F)(_))
  class I2B extends INSN(new Opcode("i2b", Opcodes.I2B)(_))
  class I2C extends INSN(new Opcode("i2c", Opcodes.I2C)(_))
  class I2S extends INSN(new Opcode("i2s", Opcodes.I2S)(_))
  class LCMP extends INSN(new Opcode("lcmp", Opcodes.LCMP)(_))
  class FCMPL extends INSN(new Opcode("fcmpl", Opcodes.FCMPL)(_))
  class FCMPG extends INSN(new Opcode("fcmpg", Opcodes.FCMPG)(_))
  class DCMPL extends INSN(new Opcode("dcmpl", Opcodes.DCMPL)(_))
  class DCMPG extends INSN(new Opcode("dcmpg", Opcodes.DCMPG)(_))
  class IFEQ(label: LabelNode) extends JUMP_INSN(new Opcode("ifeq", Opcodes.IFEQ)(_), label)
  class IFNE(label: LabelNode) extends JUMP_INSN(new Opcode("ifne", Opcodes.IFNE)(_), label)
  class IFLT(label: LabelNode) extends JUMP_INSN(new Opcode("iflt", Opcodes.IFLT)(_), label)
  class IFGE(label: LabelNode) extends JUMP_INSN(new Opcode("ifge", Opcodes.IFGE)(_), label)
  class IFGT(label: LabelNode) extends JUMP_INSN(new Opcode("ifgt", Opcodes.IFGT)(_), label)
  class IFLE(label: LabelNode) extends JUMP_INSN(new Opcode("ifle", Opcodes.IFLE)(_), label)
  class IF_ICMPEQ(label: LabelNode) extends JUMP_INSN(new Opcode("if_icmpeq", Opcodes.IF_ICMPEQ)(_), label)
  class IF_ICMPNE(label: LabelNode) extends JUMP_INSN(new Opcode("if_icmpne", Opcodes.IF_ICMPNE)(_), label)
  class IF_ICMPLT(label: LabelNode) extends JUMP_INSN(new Opcode("if_icmplt", Opcodes.IF_ICMPLT)(_), label)
  class IF_ICMPGE(label: LabelNode) extends JUMP_INSN(new Opcode("if_icmpge", Opcodes.IF_ICMPGE)(_), label)
  class IF_ICMPGT(label: LabelNode) extends JUMP_INSN(new Opcode("if_icmpgt", Opcodes.IF_ICMPGT)(_), label)
  class IF_ICMPLE(label: LabelNode) extends JUMP_INSN(new Opcode("if_icmple", Opcodes.IF_ICMPLE)(_), label)
  class IF_ACMPEQ(label: LabelNode) extends JUMP_INSN(new Opcode("if_acmpeq", Opcodes.IF_ACMPEQ)(_), label)
  class IF_ACMPNE(label: LabelNode) extends JUMP_INSN(new Opcode("if_acmpne", Opcodes.IF_ACMPNE)(_), label)
  class GOTO(label: LabelNode) extends JUMP_INSN(new Opcode("goto", Opcodes.GOTO)(_), label)
  class JSR(label: LabelNode) extends JUMP_INSN(new Opcode("jsr", Opcodes.JSR)(_), label)
  class RET(`var`: Int) extends VAR_INSN(new Opcode("ret", Opcodes.RET)(_), `var`)
  class TABLESWITCH extends SWITCH_INSN(new Opcode("tableswitch", Opcodes.TABLESWITCH)(AbstractInsnNode.TABLESWITCH_INSN)) {
    override def toAbstractInsnNode: AbstractInsnNode = ??? //new TableSwitchInsnNode()
  }
  class LOOKUPSWITCH extends SWITCH_INSN(new Opcode("lookupswitch", Opcodes.LOOKUPSWITCH)(AbstractInsnNode.LOOKUPSWITCH_INSN)) {
    override def toAbstractInsnNode: AbstractInsnNode = ???
  }
  class IRETURN extends INSN(new Opcode("ireturn", Opcodes.IRETURN)(_))
  class LRETURN extends INSN(new Opcode("lreturn", Opcodes.LRETURN)(_))
  class FRETURN extends INSN(new Opcode("freturn", Opcodes.FRETURN)(_))
  class DRETURN extends INSN(new Opcode("dreturn", Opcodes.DRETURN)(_))
  class ARETURN extends INSN(new Opcode("areturn", Opcodes.ARETURN)(_))
  class RETURN extends INSN(new Opcode("return", Opcodes.RETURN)(_))
  class GETSTATIC(owner: String, name: String, desc: String) extends FIELD_INSN(new Opcode("getstatic", Opcodes.GETSTATIC)(_), owner, name, desc)
  class PUTSTATIC(owner: String, name: String, desc: String) extends FIELD_INSN(new Opcode("putstatic", Opcodes.PUTSTATIC)(_), owner, name, desc)
  class GETFIELD(owner: String, name: String, desc: String) extends FIELD_INSN(new Opcode("getfield", Opcodes.GETFIELD)(_), owner, name, desc)
  class PUTFIELD(owner: String, name: String, desc: String) extends FIELD_INSN(new Opcode("putfield", Opcodes.PUTFIELD)(_), owner, name, desc)
  class INVOKEVIRTUAL(owner: String, name: String, desc: String) extends METHOD_INSN(new Opcode("invokevirtual", Opcodes.INVOKEVIRTUAL)(_), owner, name, desc, false)
  class INVOKESPECIAL(owner: String, name: String, desc: String) extends METHOD_INSN(new Opcode("invokespecial", Opcodes.INVOKESPECIAL)(_), owner, name, desc, false)
  class INVOKESTATIC(owner: String, name: String, desc: String) extends METHOD_INSN(new Opcode("invokestatic", Opcodes.INVOKESTATIC)(_), owner, name, desc, false)
  class INVOKEINTERFACE(owner: String, name: String, desc: String) extends METHOD_INSN(new Opcode("invokeinterface", Opcodes.INVOKEINTERFACE)(_), owner, name, desc, true)
  class INVOKEDYNAMIC extends Instruction(new Opcode("invokedynamic", Opcodes.INVOKEDYNAMIC)(AbstractInsnNode.INVOKE_DYNAMIC_INSN)) {
    override def toAbstractInsnNode = ??? //new InvokeDynamicInsnNode()
  }
  class NEW(desc: String) extends TYPE_INSN(new Opcode("new", Opcodes.NEW)(_), desc)
  class NEWARRAY(int: Int) extends INT_INSN(new Opcode("newarray", Opcodes.NEWARRAY)(_), int)
  class ANEWARRAY(desc: String) extends TYPE_INSN(new Opcode("anewarray", Opcodes.ANEWARRAY)(_), desc)
  class ARRAYLENGTH extends INSN(new Opcode("arraylength", Opcodes.ARRAYLENGTH)(_))
  class ATHROW extends INSN(new Opcode("athrow", Opcodes.ATHROW)(_))
  class CHECKCAST(desc: String) extends TYPE_INSN(new Opcode("checkcast", Opcodes.CHECKCAST)(_), desc)
  class INSTANCEOF(desc: String) extends TYPE_INSN(new Opcode("instanceof", Opcodes.INSTANCEOF)(_), desc)
  class MONITORENTER extends INSN(new Opcode("monitorenter", Opcodes.MONITORENTER)(_))
  class MONITOREXIT extends INSN(new Opcode("monitorexit", Opcodes.MONITOREXIT)(_))
  class MULTIANEWARRAY(val desc: String, val dims: Int) extends Instruction(new Opcode("multianewarray", Opcodes.MULTIANEWARRAY)(AbstractInsnNode.MULTIANEWARRAY_INSN)) {
    override def toAbstractInsnNode = new MultiANewArrayInsnNode(desc, dims)
  }
  class IFNULL(label: LabelNode) extends JUMP_INSN(new Opcode("ifnull", Opcodes.IFNULL)(_), label)
  class IFNONNULL(label: LabelNode) extends JUMP_INSN(new Opcode("ifnonnull", Opcodes.IFNONNULL)(_), label)
}