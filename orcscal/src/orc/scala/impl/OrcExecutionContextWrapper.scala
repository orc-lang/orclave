package orc.scala.impl

import orc.scala.OrcExecutionContext
import scala.concurrent.ExecutionContext

class OrcExecutionContextWrapper(val counter: Counter, val terminator: Terminator,
                                 underlying: ExecutionContext) extends OrcExecutionContext {
  def execute(runnable: Runnable): Unit = {
    underlying.execute(runnable)
  }

  def reportFailure(cause: Throwable): Unit = {
    underlying.reportFailure(cause)
  }

  override def prepare(): ExecutionContext = {
    underlying.prepare()
  }

  def schedule(task: => Unit): Unit = {
    // Matched to: halt in runnable below
    counter.prepareSpawn()
    prepare().execute(new Runnable() {
      def run() = {
        try {
          task
        } catch {
          case _: KilledException => ()
        } finally {
          // Matched to: prepareSpawn before execute
          counter.halt()
        }
      }
    })
  }

  def withCounter(newCounter: Counter): OrcExecutionContext = {
    new OrcExecutionContextWrapper(newCounter, terminator, underlying)
  }

  def withTerminator(newTerminator: Terminator): OrcExecutionContext = {
    new OrcExecutionContextWrapper(counter, newTerminator, underlying)
  }
}