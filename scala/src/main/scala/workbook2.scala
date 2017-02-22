package com.fpis.workbook2

object Workbook2 extends App{

  sealed trait Stream[+A] {
    def foldRight[B](z: B)(f: (A, => B) => B): B =
      this match{
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream{
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def toListOF[A](s: Stream[A]): List[A] = s match {
    case Cons(h, t) => h() :: toListOF(t())
    case _ => List()
  }

  import Stream._
  def take[A](s: Stream[A], n: Int): Stream[A] = s match {
    case Cons(h, t) if (n > 1) => cons(h(), take(t(), n-1))
    case Cons(h, _) if (n == 1) => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  def drop[A](s: Stream[A], n: Int): Stream[A] = s match {
    case Cons(_, t) if (n > 0) => drop(t(), n-1)
    case _ => s
  }

  def takeWhile[A](s: Stream[A])(f: A => Boolean): Stream[A] = s match {
    case Cons(h, t) if f(h()) => cons(h(), takeWhile(t())(f))
    case _ => empty
  }

  def forAll[A](s: Stream[A])(p: A => Boolean): Boolean =
    s.foldRight(true)((a, b) => p(a) && b)

  //def takeWhile_1[A](s: Stream[A])(p: A => Boolean): Stream[A] =
    //s.foldRight(empty)((a, b) => if (p(a)) cons(a, b) else empty)
}
