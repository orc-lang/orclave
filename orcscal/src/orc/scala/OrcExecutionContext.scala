package orc.scala

import scala.concurrent.ExecutionContext
import orc.scala.impl.PublicationCont
import orc.scala.impl.Counter
import orc.scala.impl.Terminator
import orc.scala.impl.OrcExecutionContextWrapper
import orc.scala.impl.KilledException
import orc.scala.impl.CounterRoot

trait OrcExecutionContext extends ExecutionContext {
  val counter: Counter
  val terminator: Terminator
  
  def withCounter(newCounter: Counter): OrcExecutionContext
  def withTerminator(newTerminator: Terminator): OrcExecutionContext  

  def schedule(task: => Unit): Unit
  
  @throws[KilledException]
  def checkLive(): Unit = terminator.checkLive()
  def prepareSpawn(): Unit = counter.prepareSpawn()
  def halt(): Unit = counter.halt()
}

object OrcExecutionContext {
  def apply(ectx: ExecutionContext) = {
    new OrcExecutionContextWrapper(new CounterRoot(), new Terminator, ectx)
  }
}