package org.lolhens.asmpatcher.asm

/**
 * Created by LolHens on 18.01.2015.
 */
sealed trait Access {
}

object Access {

  final object Public extends Access

  final object Private extends Access

  final object Package extends Access

  final object Protected extends Access

}
