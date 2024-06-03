package funsets
import scala.annotation.tailrec

object FunSets {
  type FunSet = Int => Boolean

  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): FunSet = x => x == elem

  def union(s: FunSet, t: FunSet): FunSet = x => contains(s, x) || contains(t, x)

  def intersect(s: FunSet, t: FunSet): FunSet = x => contains(s, x) && contains(t, x)

  def diff(s: FunSet, t: FunSet): FunSet = x => contains(s, x) && !contains(t, x)

  def filter(s: FunSet, p: Int => Boolean): FunSet = x => contains(s, x) && p(x)

  val bound = 1000
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    @tailrec
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false
      else iter(a + 1)
    }

    iter(-bound)
  }

  def exists(s: FunSet, p: Int => Boolean): Boolean = {
    @tailrec
    def iter(a: Int): Boolean = {
      if (a > bound) false
      else if (s(a) && p(a)) true
      else iter(a + 1)
    }

    iter(-bound)
  }

  def map(s: FunSet, f: Int => Int): FunSet = {

    @tailrec
    def iter(a: Int, resFun: FunSet): FunSet = {
      if (a > bound) resFun
      else {
        def newFun: FunSet = if (contains(s,a)) x => resFun(x) || x == f(a) else resFun
        iter(a + 1, newFun)
      }

    }

    iter(-bound, x => false)
  }

  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def printSet(s: FunSet): Unit =
    println(toString(s))

}
