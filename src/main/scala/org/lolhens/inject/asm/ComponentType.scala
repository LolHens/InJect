package org.lolhens.inject.asm

/**
 * Created by LolHens on 27.01.2015.
 */
sealed trait ComponentType {
}

object Access {

  final object Class extends ComponentType

  final object Field extends ComponentType

  final object Method extends ComponentType

  final object Insn extends ComponentType

}
