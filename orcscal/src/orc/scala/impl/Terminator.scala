//
// Terminator.scala -- Terminator implementation
// Project OrcScal
//
// Created by amp
//
// Copyright (c) 2016 The University of Texas at Austin. All rights reserved.
//
// Use and redistribution of this file is governed by the license terms in
// the LICENSE file found in the project's top-level directory and also found at
// URL: http://orc.csres.utexas.edu/license.shtml .
//

package orc.scala.impl

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

trait Terminatable {
  def kill(): Unit
}

/** The a termination tracker.
  *
  * @author amp
  */
class Terminator extends Terminatable {
  // TODO: children can theoretically grow without bound. We need to actually remove the children when they are gone.
  private[this] val children: AtomicReference[List[Terminatable]] = new AtomicReference(Nil)

  def addChild(child: Terminatable): Unit = {
    val orig = children.get()
    if (orig == null) {
      child.kill()
      throw KilledException.SINGLETON
    }

    val n = child :: orig
    if (!children.compareAndSet(orig, n)) {
      // Retry on failure to set
      addChild(child)
    }
  }

  /** Check that this context is live and throw KilledException if it is not.
    */
  def checkLive(): Unit = {
    if (!isLive()) {
      throw KilledException.SINGLETON
    }
  }

  /** Return true if this context is still live (has not been killed or halted
    * naturally).
    */
  def isLive(): Boolean = {
    children.get() != null
  }

  /** Kill the expressions under this terminator.
    *
    * This will throw KilledException if the terminator has already been killed otherwise it will just return to allow handling.
    */
  def kill(): Unit = {
    val cs = children.get()
    if (cs != null && children.compareAndSet(cs, null)) {
      // If we were the first to kill and it succeeded
      for (c <- cs) {
        try {
          c.kill()
        } catch {
          case _: KilledException => {}
        }
      }
    } else if (cs == null) {
      // If it was already killed
      throw KilledException.SINGLETON
    } else {
      // If the kill attempt failed.
      // Retry
      kill()
    }
  }
}

/** The a termination tracker.
  *
  * @author amp
  */
final class TerminatorNested(parent: Terminator) extends Terminator {
  parent.addChild(this)
}