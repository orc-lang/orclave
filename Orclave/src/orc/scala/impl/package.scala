//
// package.scala -- Package level declarations for orc.scala.impl
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

package orc.scala

package object impl {
  type PublicationCont[T] = (T) => Unit
}