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
    // Check live before scheduling because it may be expensive on this and other threads.
    checkLive()
    // Matched to: halt in runnable below
    prepareSpawn()
    prepare().execute(new Runnable() {
      def run() = {
        try {
          try {
            task
          } finally {
            // Matched to: prepareSpawn before execute
            halt()
          }
        } catch {
          case _: KilledException => ()
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