package com.fpis.workbook

object Workbook extends App{
  def findFirst[A](ss: Array[A], p: A => Boolean): Int = {
    //1. Start with first element of array
    //2. If key is found, return the index
    //3. Else call the function with the next element
    //4. Need to check for the bound, if bound is called with, return -1
    @annotation.tailrec
    def go(n: Int): Int = {
      if (n > (ss.length -1)) -1
      else if (p(ss(n))) n
      else go(n+1)
    }
    go(0)
  }

  val arr1 = Array("hello", "world", "hello world")
  val key1 = "hello"
  def findMatchString(s: String): Boolean = s == key1

  val arr2 = Array(1, 5, 8, 10, 2, 4, -5)
  val key2 = 4
  def findMatchInt(s: Int): Boolean = s == key2

  println(findFirst(arr2, findMatchInt))

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if (as.length == 1) true

    @annotation.tailrec
    def go(n: Int): Boolean ={
      if (n > (as.length-1)) true
      else if (ordered(as(n-1), as(n))) go(n+1)
      else false
    }
    go(1)
  }

  val arr3 = Array(1, 7, 3, 4)
  def orderFunction(a: Int, b: Int): Boolean = (a < b)
  println(isSorted(arr3, orderFunction))


  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    //g andThen f
    //f compose g
    a => f(g(a))
  }

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(h, t) => h + sum(t)
    }

    def product(ints: List[Int]): Int = ints match {
      case Nil => 1
      case Cons(0, _) => 0
      case Cons(h, t) => h*product(t)
    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def tail[A](xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    def setHead[A](xs: List[A], h: A): List[A] = xs match {
      case Nil => List(h)
      case Cons(_, t) => Cons(h, t)
    }

    def drop[A](xs: List[A], n: Int): List[A] = {
      if (n == 0) xs
      else {
        xs match {
          case Nil => Nil
          case Cons(_, t) => drop(t, n-1)
        }
      }
    }

    def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => xs
    }

    def init[A](xs: List[A]): List[A] = {
      xs match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }
      /*
      xs match {
        case Cons(h1, Cons(h2, t1)) => t1 match {
          case Nil => List(h1)
          case _ => Cons(h1, init(Cons(h2, t1)))
        }
        case _ => Nil
      }
     */
    }

    def showList[A](l: List[A]): Unit = l match {
      case Cons(h, t) => {
        println(h)
        showList(t)
      }
      case _ => println("Nil")
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }

    def sumR(ints: List[Int]): Int =
      foldRight(ints, 0)(_ + _)

    def productR(ints: List[Int]): Int =
      foldRight(ints, 1)(_ * _)

    def lengthR[A](as: List[A]): Int =
      foldRight(as, 0)((_, acc) => acc + 1)

    def sumL(ints: List[Int]): Int =
      foldLeft(ints, 0)(_ + _)

    def productL(ints: List[Int]): Int =
      foldLeft(ints, 1)(_ * _)

    def lengthL[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, List[A]())((b, a) => Cons(a, b))

    def append[A](a: List[A], b: List[A]): List[A] =
      foldRight(a, b)((x, y) => Cons(x, y))

    def concat[A](ll: List[List[A]]): List[A] =
      foldLeft(ll, Nil: List[A])(append)

    def addOne(ints: List[Int]): List[Int] =
      foldLeft(ints, Nil: List[Int])((t, h) => Cons(h+1, t))

    def doubleToString(ds: List[Double]): List[String] =
      foldLeft(ds, Nil: List[String])((t, h) => Cons(h.toString + " foo", t))

    def map[A, B](l: List[A])(f: A => B): List[B] =
      foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

    //def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
      //case Cons(h, t) => {
        //if (f(h)) Cons(h, filter(t)(f))
        //else filter(t)(f)
      //}
      //case Nil => Nil
    //}
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      concat(map(as)(f))
      // Why does this implementation not work??
      //concat(foldRight(as, Nil: List[B])((h, t) => (Cons(f(h), t))))

    def addPairWise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
    }

    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
      (as, bs) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  import List._

  def printSeparator: Unit = println("--")
  val l = List(1, 2, 3, 4, 5, 6)
  println(tail(l)); printSeparator
  println(setHead(l, -10)); printSeparator
  println(l); printSeparator
  println(drop(l, 1)); printSeparator
  println(dropWhile(l)(x => x < 4)); printSeparator
  println(init(l)); printSeparator
  println(sumR(l)); printSeparator
  println(productR(l)); printSeparator
  println(lengthR(l)); printSeparator
  println(sumL(l)); printSeparator
  println(productL(l)); printSeparator
  println(lengthL(l)); printSeparator
  showList(reverse(l)); printSeparator
  println("append"); printSeparator
  showList(append(List(1, 2, 3), List(10, 20, 30))); printSeparator
  showList(addOne(List(1, 2, 3))); printSeparator
  showList(doubleToString(List(1, 2, 3))); printSeparator
  showList(concat(List(List("a"), List("b", "C")))); printSeparator
  showList(filter(List(1, 2, 3, 4, 5))(x => x % 2 == 0)); printSeparator
  showList(addPairWise(List(1, 2, 3), List(4, 5, 6))); printSeparator
  showList(zipWith(List(1, 2, 3), List(4, 5, 6))(_+_))
}
