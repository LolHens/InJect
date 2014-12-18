package org.lolhens.asmpatcher.test.test1

import org.lolhens.asmpatcher.AsmBlock

/**
 * Created by LolHens on 17.12.2014.
 */
object Main {
  def main(args: Array[String]): Unit = {
    val asmBlock = new AsmBlock()

    asmBlock.insert(0, "L0")
    asmBlock.insert(1, "LDC \"abc\"")
    asmBlock.insert(2, "ASTORE 2")
    asmBlock.insert(3, "L1")
    asmBlock.insert(4, "NEW java/lang/Float")
    asmBlock.insert(5, "DUP")
    asmBlock.insert(6, "LDC 1.3")
    asmBlock.insert(7, "INVOKESPECIAL java/lang/Float.<init> (F)V")
    asmBlock.insert(8, "ASTORE 3")
    asmBlock.insert(9, "L2")
    asmBlock.insert(10, "LDC 24.3")
    asmBlock.insert(11, "DSTORE 4")
    asmBlock.insert(12, "L3")
    asmBlock.insert(13, "LDC 3")
    asmBlock.insert(14, "LSTORE 6")
    asmBlock.insert(15, "L4")
    asmBlock.insert(16, "ILOAD 1")
    asmBlock.insert(17, "LDC 2000000")
    asmBlock.insert(18, "IF_ICMPNE L5")
    asmBlock.insert(19, "SIPUSH 30000")
    asmBlock.insert(20, "IRETURN")
    asmBlock.insert(21, "L5")
    asmBlock.insert(22, "ICONST_0")
    asmBlock.insert(23, "IRETURN")
    asmBlock.insert(24, "L6")

    println(asmBlock)
  }
}
