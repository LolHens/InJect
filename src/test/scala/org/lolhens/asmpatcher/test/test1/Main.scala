package org.lolhens.asmpatcher.test.test1

import org.lolhens.asmpatcher.AsmBlockReader

/**
 * Created by LolHens on 17.12.2014.
 */
object Main {
  def main(args: Array[String]): Unit = {
    val asmBlock = new AsmBlockReader()

    asmBlock.add("L0")
    asmBlock.add("LDC \"abc\"")
    asmBlock.add("ASTORE 2")
    asmBlock.add("L1")
    asmBlock.add("NEW java/lang/Float")
    asmBlock.add("DUP")
    asmBlock.add("LDC 1.3")
    asmBlock.add("INVOKESPECIAL java/lang/Float.<init> (F)V")
    asmBlock.add("ASTORE 3")
    asmBlock.add("L2")
    asmBlock.add("LDC 24.3")
    asmBlock.add("DSTORE 4")
    asmBlock.add("L3")
    asmBlock.add("LDC 3")
    asmBlock.add("LSTORE 6")
    asmBlock.add("L4")
    asmBlock.add("ILOAD 1")
    asmBlock.add("LDC 2000000")
    asmBlock.add("IF_ICMPNE L5")
    asmBlock.add("SIPUSH 30000")
    asmBlock.add("IRETURN")
    asmBlock.add("L5")
    asmBlock.add("ICONST_0")
    asmBlock.add("IRETURN")
    asmBlock.add("L6")
    asmBlock.add("LOCALVARIABLE this Lorg/lolhens/asmpatcher/test/testclasses/Testclass1; L0 L6 0")
    asmBlock.add("LOCALVARIABLE i1 I L0 L6 1")
    asmBlock.add("LOCALVARIABLE a Ljava/lang/String; L1 L6 2")
    asmBlock.add("LOCALVARIABLE b Ljava/lang/Float; L2 L6 3")
    asmBlock.add("LOCALVARIABLE c D L3 L6 4")
    asmBlock.add("LOCALVARIABLE d J L4 L6 6")

    System.out.println(asmBlock.toString)


  }
}
