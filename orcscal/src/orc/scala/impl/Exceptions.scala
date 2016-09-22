//
// Exceptions.scala -- Exceptions used internally to the OrcScal implementation
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

/** Notify the enclosing code that this direct form Orc code has halted.
  */
class HaltException() extends RuntimeException() {
}

object HaltException {
  /** A singleton instance of HaltException to avoid allocation.
    */
  def SINGLETON = new HaltException()
  /* NOTE: Using a singleton is the "right thing" for performance, 
   * however it makes the stacks wrong. You can change this to a def 
   * to get the stacks right.
   */
}

/** Notify the enclosing code that this Orc code has been killed.
  *
  * This is thrown by checkLive() and caught in Trim implementations.
  */
final class KilledException extends RuntimeException

object KilledException {
  /** A singleton instance of KilledException to avoid allocation.
    */
  def SINGLETON = new KilledException
  /* NOTE: Using a singleton is the "right thing" for performance, 
   * however it makes the stacks wrong. You can change this to a def 
   * to get the stacks right.
   */
}