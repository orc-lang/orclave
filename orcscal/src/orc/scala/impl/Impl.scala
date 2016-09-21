package orc.scala.impl

import orc.scala._

class Parallel[T](l: Orc[T], r: Orc[T]) extends Orc[T] {
  def execute(p: PublicationCont[T], c: Counter, t: Terminator): Unit = {
    // TODO: Check
    schedule { l.execute(p, c, t) }
    r.execute(p, c, t)
  }
}

class Filter[T](e: Orc[T], pred: T ⇒ Boolean) extends Orc[T] {
  def execute(p: PublicationCont[T], c: Counter, t: Terminator): Unit = {
    // TODO: Check
    e.execute(x => {
      if (pred(x))
        p(x)
    }, c, t)
  }
}

class Branch[A, B](l: Orc[A], r: A ⇒ Orc[B]) extends Orc[B] {
  def execute(p: PublicationCont[B], c: Counter, t: Terminator): Unit = {
    // TODO: Check
    l.execute(x => schedule { r(x).execute(p, c, t) }, c, t)
  }
}

class Otherwise[T](l: Orc[T], r: Orc[T]) extends Orc[T] {
  def execute(p: PublicationCont[T], c: Counter, t: Terminator): Unit = {
    // TODO
    l.execute(???, ??? /*new NestedCounter(c) {
      def halt() = {
        if (not published)
          r.execute(p, c, t)
      }
    }*/ , t)
  }
}

class Trim[T](e: Orc[T]) extends Orc[T] {
  def execute(p: PublicationCont[T], c: Counter, t: Terminator): Unit = {
    e.execute(??? /*on publication call kill block all later publications*/ , c, t)
  }
}

class Graft[T](e: Orc[T]) extends Future[T] {
  // TODO
  def bind(v: T): Unit = ???

  val binder: Orc[Nothing] = new Orc[Nothing] {
    def execute(p: PublicationCont[Nothing], c: Counter, t: Terminator): Unit = {
      // TODO: Check
      e.execute(bind, c, t)
    }
  }

  // TODO: Future implementation
  def map[B](f: T => B): orc.scala.Future[B] = ???
  def onHalted(f: () => Unit): Unit = ???
  def withFilter(b: T => Boolean): orc.scala.Future[T] = ???
}

class Const[T](v: T) extends Orc[T] {
  def execute(p: PublicationCont[T], c: Counter, t: Terminator): Unit = {
    // TODO: Check
    schedule { p(v) }
  }
}

class Variable[T](fut: Future[T]) extends Orc[T] {
  def execute(p: PublicationCont[T], c: Counter, t: Terminator): Unit = {
    // TODO: Check
    fut.map(p)
    // TODO: Handle halting properly
  }
}

class Stop() extends Orc[Nothing] {
  def execute(p: PublicationCont[Nothing], c: Counter, t: Terminator): Unit = {
    // TODO: Check
  }
}
