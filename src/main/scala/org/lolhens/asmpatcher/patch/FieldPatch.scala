package org.lolhens.asmpatcher.patch

/**
 * Created by LolHens on 29.12.2014.
 */
class FieldPatch(val name: String, val signature: String) {
  var add = false
  var remove = false
  var rename: String = null
}
