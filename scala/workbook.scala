package fpinscala.datastructures.workbook

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def product(p: List[Double]) =
    foldRight(p, 1.0)((x, y) => x * y)

  def sum(s: List[Int]) =
    foldRight(s, 0.0)((x, y) => x + y)

  def length[A](as: List[A]): Int = 
    foldRight(as, 0)((_, acc) => (acc + 1))

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def productFL(p: List[Double]): Double =
    foldLeft(p, 1.0)((x, y) => x * y)

  def sumFL(s: List[Int]): Int =
    foldLeft(s, 0)((x, y) => x + y)

  def lengthFL(as: List[Int]): Int =
    foldLeft(as, 0)((acc, _) => (acc + 1))

  //def reverse(l: List[Int]): List[Int] =
    //foldLeft(l, List[Int]())((h, acc) => Cons(acc, h))
  
  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))

  def toString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List [A] =
    foldRight(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def pairwiseAdd(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => b
    case (_, Nil) => a
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, pairwiseAdd(t1, t2))
  }

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = (a, b) match {
    case(Nil, _) => b
    case(_, Nil) => a
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
}

sealed trait Tree[+A]
case class Leaf[+A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }
  
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}
