package orc.scala

package object impl {
  type PublicationCont[T] = (T) => Unit
  class Counter
  class Terminator
}