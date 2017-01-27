trait RNG {
    def nextInt: (Int, RNG)
  }

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  //EX 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i: Int, newRNG: RNG) = rng.nextInt
    (if (i<0) -(i+1) else i, newRNG)
  }

  //EX 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i: Int, newRNG: RNG) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), newRNG)
  }

  //EX 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i: Int, rng1: RNG) = rng.nextInt
    val (d: Double, rng2: RNG) = double(rng1)
    ((i,d),rng2)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),nextRNG) = intDouble(rng)
    ((d,i),nextRNG)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d0: Double, rng1: RNG) = double(rng)
    val (d1: Double, rng2: RNG) = double(rng1)
    val (d2: Double, rng3: RNG) = double(rng2)
    ((d0,d1,d2),rng3)
  }

  //EX 6.4
  def ints(count: Int)(rng: RNG): (List[Int],RNG) = {
    @annotation.tailrec
    def go(count: Int, rng: RNG, ints: List[Int]): (List[Int],RNG) =
      if (count>0) {
        val (i: Int, nextRNG: RNG) = rng.nextInt
        go(count-1,nextRNG,ints:+i)
      } else {
        (ints,rng)
      }
    go(count,rng,List.empty)
  }
  assert(ints(3)(SimpleRNG(42))._1.size == 3)

  type Rand[+A] = RNG => (A,RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a,rng2) = s(rng)
      (f(a),rng2)
    }

  //EX 6.5
  def doubleM: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  //EX 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    rng => {
      val (a,rng2) = ra(rng)
      val (b,rng3) = rb(rng2)
      (f(a,b),rng3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[( A,B )] =
    map2(ra,rb)(( _,_ ))

  val randIntDouble: Rand[(Int,Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double,Int)] =
    both(double, int)

  //EX 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => unit[List[A]](Nil)
    case h :: t => map2(h,sequence(t))(_::_)
  }

  //EX 6.8
  // def flatMap[A,B](r: RNG => (A,RNG))(f: A => (RNG => B,RNG))): (B,RNG) =
  def flatMap[A,B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a,rng0) = r(rng)
      f(a)(rng0)
    }
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){i: Int =>
      val mod = i % n
      if(i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def rollDie: Rand[Int] = mapF(nonNegativeLessThan(6))(_ + 1)

  //EX 6.9
  def mapF[A,B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r){a: A => { rng =>
        (f(a),rng)
      }
    }
  def map2F[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra){a: A => map(rb)(f(a,_))}

  def bothF[A,B](ra: Rand[A], rb: Rand[B]): Rand[( A,B )] =
    map2F(ra,rb)(( _,_ ))
}

object State {
  def unit[S,A](a: A) = State{s: S => (a,s)}
  def sequence[S,A](ss: List[State[S,A]]): State[S,List[A]] =
    ss.foldRight(unit[S,List[A]](Nil))((h,t) => h.map2(t)(_::_))
}

case class State[S,+A](run: S => (A,S)){
  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State { s =>
      val (a,s1) = run(s)
      f(a).run(s1)
    }
  def map[B](f: A => B): State[S,B] =
    flatMap{a: A => State.unit(f(a))}
  def map2[B,C](s2: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap{a: A => s2.map(b => f(a,b))}
}
