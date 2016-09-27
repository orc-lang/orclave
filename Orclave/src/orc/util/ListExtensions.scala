package orc.util

object ListExtensions {
  implicit class ListListAdds[T](val l: List[List[T]]) extends AnyVal {
    def innerMap[B](f: T => B): List[List[B]] = {
      l.map(_.map(f))
    }
    def innerZip[B](r: List[List[B]]): List[List[(T, B)]] = {
      (l zip r).map {
        case (le, re) => le zip re
      }
    }
  }
}