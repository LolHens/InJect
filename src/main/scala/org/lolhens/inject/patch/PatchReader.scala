package org.lolhens.inject.patch

import java.io.{BufferedReader, Reader}

/**
 * Created by LolHens on 27.01.2015.
 */
class PatchReader(reader: Reader) {
  private val bufferedReader = new BufferedReader(reader)
  private var eof = false;

  def read(): Patch = {
    val line = bufferedReader.readLine()
    if (line == null) {
      eof = true
      return null
    }
    val patch = new Patch(null)

    patch
  }

  def readAll(): PatchList = {
    val patchList = new PatchList()
    while (!eof) {

    }
    null
  }
}
