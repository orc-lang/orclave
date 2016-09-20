package orc.scala

/** An instance of Orc[T] represents an instance of an Orc expression.
  *
  * The expression may or may not be executing already. Other expressions (or
  * external code) can observe publication and halting using `map` and `onHalted`.
  */
trait Orc[T] {
  /** @group Observation */

  /** Call f and execute the result for each value that this expression
    * publishes.
    *
    * map on Orc is actually `flatMap` because this enables Orc expressions to
    * appear in the body of for-loops over Orc[T]. This should not cause problems
    * because any expression can be wrapped into an Orc[T].
    *
    * Only one call to map, flatMap, or withFilter is allowed and it must be
    * called before execute is called.
    */
  def map[B](f: T => Orc[B]): Orc[B]

  /** Call f and execute the result for each value that this expression
    * publishes.
    *
    * Only one call to map, flatMap, or withFilter is allowed and it must be
    * called before execute is called.
    */
  def flatMap[B](f: T => Orc[B]): Orc[B] = map(f)

  /** Drop publications for which `b` is not true.
    *
    * Only one call to map, flatMap, or withFilter is allowed and it must be
    * called before execute is called.
    */
  def withFilter(b: T => Boolean): Orc[T]

  /** Call f when this expression is halted
    */
  def onHalted(f: () => Unit): Unit

  /** @group Combinators */

  /** Combine this with `r` such that they will run concurrently.
    */
  def |||[A >: T](r: Orc[A]): Orc[A]

  /** Run `f` for each publication of this.
    *
    * @see map
    */
  def branch[B](f: T => Orc[B]): Orc[B]

  /** Convert this expression into a future which is bound to the first
    * publication of this.
    */
  def graft: Future[T]

  /** Create a new expression which will kill this when it publishes for the first
    * time.
    */
  def trim: Orc[T]

  /** Create an expression which will execute `f` if this never publishes.
    *
    * Run the second operand if the first calls onHalt's f without calling map's f.
    */
  def otherwise[A >: T](f: Orc[A]): Orc[A]

  /** @group Control */

  def kill(): Unit

  /** Execute this expression.
    */
  def execute(): Unit
}

object Orc extends OrcLowPriorityImplicits {
  /** The orc expression which never publishes and halts immediately.
    */
  val stop: Orc[Nothing] = ???
  // TODO: Nothing cannot be inferred as a type parameter in at least some cases so type inferrence will mess up when using this sometimes.

  /** Execute an Orc expression eager/leniently.
    *
    * The returned iterable will contain all the publications of the expressions
    * as they become available. The iterable will end when the Orc expression halts.
    */
  def orc[T](o: Orc[T]): Iterable[T] = ???

  /** Get an Orc expression directly without executing it.
    *
    * This triggers Orc macro expansion without executing it or creating an
    * iterable from the publications.
    */
  def orcRaw[T](o: Orc[T]): Orc[T] = ???

  /** Alternative syntax for graft.
    */
  def graft[T](f: Orc[T]): Future[T] = f.graft

  /** Alternative syntax for trim.
    */
  def trim[T](f: Orc[T]): Orc[T] = f.trim

  /** Create an Orc variable expression.
    *
    * The returned Orc expression publishes the value of f once it has one and halts.
    */
  implicit def variable[T](f: Future[T]): Orc[T] = ???
}

trait OrcLowPriorityImplicits {
  /** Create an Orc constant expression.
    *
    * The returned Orc expression publishes v and halts.
    */
  implicit def const[T](v: T): Orc[T] = ???
}

/** An object which may later be given a value.
  */
trait Future[T] {
  /** Call f with the value of the future.
    *
    * Unlike Orc.map this may be called at any time and will never call f
    * more than once.
    */
  def map[B](f: T => B): Future[B]

  /** Ignore the value if `b` is false for it.
    *
    * The resulting future will halt if the value is rejected.
    */
  def withFilter(b: T => Boolean): Future[T]

  /** Call f if the future is halted and will never be bound.
    */
  def onHalted(f: () => Unit): Unit
}

object Future {
  // TODO: Need to autogenerate these

  /** Combine a number of futures into a future for a tuple.
    *
    * The result will halt if any future in the tuple halts.
    */
  def join[T1, T2](f1: Future[T1], f2: Future[T2]): Future[(T1, T2)] = ???
  def join[T1, T2, T3](f1: Future[T1], f2: Future[T2], f3: Future[T3]): Future[(T1, T2, T3)] = ???
  def join[T1, T2, T3, T4](f1: Future[T1], f2: Future[T2], f3: Future[T3], f4: Future[T4]): Future[(T1, T2, T3, T4)] = ???
}
