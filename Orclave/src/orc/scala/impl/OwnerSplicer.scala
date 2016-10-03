package orc.scala.impl

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

// Modified from: https://github.com/scalamacros/macrology201/blob/722854c9504e65e45663597c9b6baf93dca7d87f/macros/src/main/scala/Macros.scala

trait OwnerSplicer {
  val c: Context
  
  // inspired by https://gist.github.com/retronym/10640845#file-macro2-scala
  // check out the gist for a detailed explanation of the technique
  def ownerSplice(tree: c.Tree, owner: c.Symbol = c.internal.enclosingOwner): c.Tree = {
    import c.universe._, c.internal._, decorators._

    tree.updateAttachment(OwnerSplicer.OrigOwnerAttachment(enclosingOwner))
    q"_root_.orc.scala.impl.OwnerSplicer.changeOwner($tree)"
  }
}

object OwnerSplicer {
  case class OrigOwnerAttachment(sym: Any)
  def impl(c: Context)(tree: c.Tree): c.Tree = {
    import c.universe._, c.internal._, decorators._
    val origOwner = tree.attachments.get[OrigOwnerAttachment].map(_.sym).get.asInstanceOf[Symbol]
    c.internal.changeOwner(tree, origOwner, c.internal.enclosingOwner)
  }
  def changeOwner[A](tree: A): A = macro impl
}