package orc.scala

import scala.concurrent.ExecutionContext
import orc.scala.impl.PublicationCont
import orc.scala.impl.Counter
import orc.scala.impl.Terminator
import orc.scala.impl.OrcExecutionContextWrapper
import orc.scala.impl.KilledException
import orc.scala.impl.CounterRoot
import orc.scala.impl.TerminatableThread

trait OrcExecutionContext extends ExecutionContext {
  val counter: Counter
  val terminator: Terminator
  
  def withCounter(newCounter: Counter): OrcExecutionContext
  def withTerminator(newTerminator: Terminator): OrcExecutionContext  

  def schedule(task: => Unit): Unit
  
  @throws[KilledException]
  def checkLive(): Unit = terminator.checkLive()
  @throws[KilledException]
  def enterTerminatable(): Unit = {
    terminator.addChild(OrcExecutionContext.threadTerminatable.get())
  }
  @throws[KilledException]
  def leaveTerminatable(): Unit = {
    terminator.removeChild(OrcExecutionContext.threadTerminatable.get())
  }
  
  def prepareSpawn(): Unit = counter.prepareSpawn()
  def halt(): Unit = counter.halt()
}

object OrcExecutionContext {
  def apply(ectx: ExecutionContext) = {
    new OrcExecutionContextWrapper(new CounterRoot(), new Terminator, ectx)
  }
  
  private val threadTerminatable = new ThreadLocal[TerminatableThread]() {
    override def initialValue(): TerminatableThread = {
      new TerminatableThread()
    }
  }
}