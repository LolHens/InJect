package org.lolhens.scala.function

import java.util._

/**
 * Created by LolHens on 27.01.2015.
 */
object JavaConversions {
  implicit def toJavaFunction[A, B](func: Function1[A, B]) = new function.Function[A, B] {
    override def apply(a: A): B = func(a)
  }

  implicit def toJavaPredicate[A](func: Function1[A, Boolean]) = new function.Predicate[A] {
    override def test(a: A): Boolean = func(a)
  }

  implicit def toJavaConsumer[A](func: Function1[A, Unit]) = new function.Consumer[A] {
    override def accept(a: A): Unit = func(a)
  }
}
