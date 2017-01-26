import scala.collection.immutable.List

sealed trait Stream[+A]{
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h()) // has to pass thunk
  }

  //EX 5.1
  def toList: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), acc :+ h())
    }
    go(this, List.empty)
  }

  //EX 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h,_) if n == 1 => cons(h(), empty)
    case _ => empty
  }
  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case _ => this
  }
  //EX 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons(h,t) => t().takeWhile(p)
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  //EX 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  //EX 5.5
  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a,b) => if(p(a)) cons(a,b) else b)

  //EX 5.6
  def headOptionFR: Option[A] =
    foldRight(None:Option[A])((h,_) => Some(h))

  //EX 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty:Stream[B])((h,t) => cons(f(h),t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty:Stream[A])((h,t) => if(p(h)) cons(h,t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  //!!
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]:Stream[B])((h,t) => f(h).append(t))


  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  //EX 5.13
  def mapU[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }
  def takeU(n: Int): Stream[A] =
    unfold((n,this)){
      case (n: Int, Cons(h,t)) if n > 0 => Some((h(), (n-1,t())))
      case _ =>  None
    }

  def takeWhileU(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if p(h()) => Some((h(), t()))
      case Cons(h,t) => t().headOption.map((_,t().drop(1))) //skips a step
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this,s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()),(t1(),t2())))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  //EX 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  //EX 5.15 HARD
  def tails: Stream[Stream[A]] =
    unfold(this){
        case Cons(_,t) => Some((t(),t()))
        case _ => None
      }

  def hasSubsesquence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
}

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream{
    //smart constructors for thunk memoization
    def cons[A](hd: => A, tl: => Stream[A])= {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A] = Empty
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))

    //EX 5.8
    def constant[A](a: A): Stream[A] =
      cons(a,constant(a))

    //EX 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((h,s)) => cons(h, unfold(s)(f))
        case None => empty
      }
  }

  val s = Stream(1,2,3)
  assert(s.toList == List(1,2,3)) //EX.5.1
  assert(s.drop(1).toList == List(2,3))
  assert(s.take(1).toList == List(1))
  assert(s.takeU(1).toList == List(1))
  assert(s.takeWhile(_%2==0).toList == List(2))
  assert(s.forAll(_%2==0) == false)
  assert(Stream(2,4,6).forAll(_%2==0) == true)
  assert(Stream(1,2,2,3,4,5).takeWhileU(_%2==0).toList == List(2,2,4))
  assert(s.headOptionFR == Some(1))
  assert(s.map(_.toString).toList == List("1","2","3"))
  assert(s.mapU(_.toString).toList == List("1","2","3"))
  assert(s.filter(_%2==0).toList == List(2))
  assert(s.append(Stream(4,5)).toList == List(1,2,3,4,5))
  assert(s.flatMap(b => Stream(b.toString)).toList == List("1","2","3"))
  assert(Stream.constant(5).take(3).toList == List(5,5,5))
  assert(Stream.unfold[String,Int](4)(s => if (s-1>=1) Some(s-1).map(v => (v.toString,v)) else None).toList
          == List("3","2","1"))

  //EX 5.9
  def from(n: Int): Stream[Int] =
    Stream.cons(n,from(n+1))
  assert(from(1).take(3).toList == List(1,2,3))

  //EX 5.10
  //!!
  def fibs: Stream[Int] = {
    def go(f0: Int,f1: Int): Stream[Int] =
      Stream.cons(f0,go(f1,f0+f1))
    go(0,1)
  }
  assert(fibs.take(5).toList == List(0,1,1,2,3))

  //EX 5.12
  def fibsU: Stream[Int] =
    Stream.unfold((0,1)){case (s0,s1) => Some(s0,(s1,s0+s1))}
  assert(fibsU.take(5).toList == List(0,1,1,2,3))

  def fromU(n: Int): Stream[Int] =
    Stream.unfold(n)(s => Some(s,s+1))
  assert(fromU(1).take(3).toList == List(1,2,3))

def constantU(n: Int): Stream[Int] =
  Stream.unfold(n)(_ => Some(n,n))
assert(constantU(1).take(3).toList == List(1,1,1))

def onesU: Stream[Int] =
  Stream.unfold(1)(_ => Some(1,1))
assert(onesU.take(3).toList == List(1,1,1))

assert(Stream(1,2,3).tails.map(_.toList).toList == List(List(2,3),List(3),List()))
