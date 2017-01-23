object Chapter3{
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    //EX 3.2
    def tail[A](list: List[A]): List[A] = list match {
      case Cons(head, tail) => tail
    }
    //EX 3.3
    def setHead[A](newHead: A, list: List[A]) = list match {
      case Cons(head, tail) => Cons(newHead, tail)
    }

    //EX 3.4
    def drop[A](l: List[A], n: Int): List[A] =
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(head, tail) => drop(tail, n-1)
      }

    //EX 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      def loop(filtered: List[A], notFiltered: List[A]): List[A] = notFiltered match {
        case Nil => filtered
        case Cons(head, tail) =>
          f(head) match {
            case true => loop(Cons(head,filtered),tail)
            case false => loop(filtered,tail)
          }
      }
      loop(Nil,l)
    }

    //EX 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    //EX 3.9
    def length[A](as: List[A]): Int =
      foldRight(as, 0)((xs,z) => z+1)

    //EX 3.10
    @annotation.tailrec
    def foldLeft[A,B](as: List[A],z:B)(f: (A,B) => B): B =
      as match {
        case Nil => z
        case Cons(h, t) => foldLeft(t,f(h,z))(f)
      }

    //EX 3.11
    def sumFL(l: List[Int]): Int =
      foldLeft(l,0)(_+_)
    def productFL(l: List[Int]): Int =
      foldLeft(l,1)(_*_)
    def lengthFL(l: List[Int]): Int =
      foldLeft(l,0)((ls,acc) => acc+1)

    //EX 3.12
    def reverse[A](l: List[A]): List[A] =
      foldLeft(l,List[A]())((acc,h) => Cons(acc,h))

    //EX 3.14
    def append[A](l: List[A], r: List[A]): List[A] =
      foldLeft(l,r)(Cons(_,_))

    //EX 3.1
    def concat[A](l: List[List[A]]): List[A] =
      foldRight(l,Nil:List[A])(append)

    //EX 3.16
    def addOne(l: List[Int]): List[Int] =
      foldRight(l,Nil:List[Int])((h,t) => Cons(h+1,t))

    //EX 3.17
    def doublesToString(l: List[Double]): List[String] =
      foldRight(l,Nil:List[String])((h,t) => Cons(h.toString,t))

    //EX 3.18
    def map[A,B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil:List[B])((h,t) => Cons(f(h),t))

    //EX 3.19
    // List.filter(List(1,2,3))(_%2==0)
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil:List[A])((h,t) => f(h) match {
                                  case true => Cons(h,t)
                                  case false => t
                                })

    //EX 3.20
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
      concat(map(as)(f))
    assert(List.flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))

    //EX 3.21
    def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
      List.flatMap(as){a =>
        f(a) match {
          case true => List(a)
          case false => Nil
        }
      }
    assert(List.filterFM(List(1,2,3))(_%2==0) == List(2))

    //EX 3.22
    def zipAdd(l: List[Int], r: List[Int]): List[Int] = (l,r) match {
      case (_,Nil) => Nil
      case (Nil,_) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2,zipAdd(t1,t2))
    }
    assert(List.zipAdd(List(1,2,3),List(4,5,6)) == List(5,7,9))

    //EX 3.23
    def zipWith[A](l: List[A], r: List[A])(f: (A,A) => A): List[A] =
      (l,r) match {
        case (_,Nil) => Nil
        case (Nil,_) => Nil
        case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
      }
    assert(List.zipWith(List(1,2,3),List(4,5,6))(_+_) == List(5,7,9))
  }
}
object Chapter4{
  sealed trait Option[+A]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  // object Option{
  //   //EX 4.1
  //   def map[B](f: A => B): Option[B]
  //   def getOrElse[B>:A](default: => B): B
  //   def flatMap[B](f: A => Option[B]): Option[B]
  //   def orElse[B>:A](ob: => Option[B]): Option[B]
  //   def filter(f: A => B): Option[A]
  // }
}
