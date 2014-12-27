package org.lolhens.asmpatcher.test.test1

import org.lolhens.asmpatcher.asmblock.AsmBlockParser

/**
 * Created by LolHens on 17.12.2014.
 */
object Main {
  def main(args: Array[String]): Unit = {
    println(AsmBlockParser.parseAsmBlock("L0\n    ICONST_2\n    ISTORE 2\n   L1\n    ALOAD 0\n    ICONST_1\n    PUTFIELD org/lolhens/asmpatcher/test/testclasses/Testclass1.test : I\n   L2\n    ICONST_1\n    PUTSTATIC org/lolhens/asmpatcher/test/testclasses/Testclass1.test2 : I\n   L3\n    ICONST_0\n    ISTORE 3\n   L4\n       ILOAD 3\n    ICONST_3\n    IF_ICMPGE L5\n   L6\n    ALOAD 0\n    ILOAD 3\n    INVOKESPECIAL org/lolhens/asmpatcher/test/testclasses/Testclass1.test2 (I)V\n   L7\n    IINC 3 1\n    GOTO L4\n   L5\n    GETSTATIC java/lang/System.out : Ljava/io/PrintStream;\n    LDC \"test\"\n    INVOKEVIRTUAL java/io/PrintStream.println (Ljava/lang/String;)V\n   L8\n    ICONST_3\n    LOOKUPSWITCH\n      1: L9\n      10000: L10\n      default: L10\n   L9\n    GOTO L10\n   L10\n    ICONST_3\n    TABLESWITCH\n      0: L11\n      1: L12\n      2: L13\n      3: L14\n      default: L14\n   L11\n    GOTO L14\n   L12\n    GOTO L14\n   L13\n    GOTO L14\n   L14\n    ICONST_0\n    IRETURN\n   L15"))
  }
}
