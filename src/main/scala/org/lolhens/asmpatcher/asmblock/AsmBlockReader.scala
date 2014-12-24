package org.lolhens.asmpatcher.asmblock

import java.io.InputStream

/**
 * Created by LolHens on 24.12.2014.
 */
class AsmBlockReader(val asmBlock: AsmBlock, val inputStream: InputStream) {
  private val parser = new AsmBlockParser(asmBlock)

  def read = {

  }
}
