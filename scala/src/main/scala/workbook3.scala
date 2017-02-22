package com.fpis.workbook3
object Workbook extends App{
  import Stream._

  sealed trait Stream[+A] {
    def toList(): List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

      go(this, List()).reverse
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t()take(n-1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 1 => t().drop(n-1)
      case Cons(_, t) if n == 1 => t()
      case _ => empty
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) =>
          if(p(h)) cons(h, t)
          else empty)

    def headOption2(): Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons(f(h), t))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) =>
          if (p(h)) cons(h, t)
          else t)

    def append[B>:A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h, t) => f(h) append t)

    def find(p: A => Boolean): Option[A] =
      filter(p).headOption2

    def map2[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
      }

    def take2(n: Int): Stream[A] =
      unfold((this, n)) {
        case (Cons(h, t), 1) => Some((h(), (empty, 0)))
        case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
        case _ => None
      }

    def takeWhile3(p: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t)  if p(h()) => Some((h(), t()))
        case _ => None
      }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def headOption[A](a: Stream[A]): Option[A] = a match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def constant[A](a: A): Stream[A] =
    {
      lazy val tail: Stream[A] = Cons(() => a, () => tail)
      tail
    }

    def from(n: Int): Stream[Int] =
      cons(n, from(n+1))

    def fibs(): Stream[Int] = {
      def go(n0: Int, n1: Int): Stream[Int] = {
        cons(n0, go(n1, (n0 + n1)))
      }
      go(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((a, s)) => cons(a, unfold(s)(f))
        case None => empty
      }

    def filbs2(): Stream[Int] =
      unfold((0, 1)) { case(f0, f1) => Some((f1, (f1, f0+f1))) }

    def from2(n: Int): Stream[Int] =
      unfold(n) { n => Some((n, n+1))}

    def constant2[A](a: A): Stream[A] =
      unfold(a) { _ => Some((a, a))}

    def one2(): Stream[Int] =
      unfold(1) { _ => Some((1, 1))}
  }

  // List functions
  import Stream._

  def printSeparator: Unit = println("--")
  val l = Stream(1, 2, 3, 4, 5, 6)
  println(l.toList); printSeparator
  println(l.take(2).toList); printSeparator
  println(l.drop(2).toList); printSeparator
  println(l.takeWhile(x => x < 3).toList); printSeparator

  val ones: Stream[Int] = Stream.cons(1, ones)
  println(fibs.take(5).toList); printSeparator
  println(ones.map(_ + 1).exists(_ % 2 == 0)); printSeparator
  //println(ones.takeWhile(_ == 1).toList); printSeparator
  println(ones.forAll(_ != 1)); printSeparator

}
