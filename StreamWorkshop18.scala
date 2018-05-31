package streams

object StreamWorkshop18 {

  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Cons(h, t) => Some(h())
      case Empty => None
    }

//    def toListRecursive: List[A] = this match {
//      case Cons(h, t) => h() :: t().toListRecursive
//      case Empty => List()
//    }

    def toList: List[A] = {
      @annotation.tailrec
      def move(s : Stream[A], acc : List[A]): List[A] = s match {
        case Cons(h, t) => move(t(), h() :: acc)
        case _ => acc
      }
      move(this, List()).reverse
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hFunc, tFunc) if (n > 0) =>
        Cons(hFunc, () => tFunc().take(n - 1))
      case _ => Empty
    }

    def drop(n : Int) : Stream[A] = this match {
      case Cons(_, tFunc) if (n > 0) =>
        tFunc().drop(n - 1)
      case _ => this
    }

//    def exists(p : A => Boolean) : Boolean = this match {
//      case Cons(hFunc, tFunc) =>
//        p(hFunc()) || tFunc().exists(p)
//      case _ => false
//    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(hFunc,tFunc) => f(hFunc(), tFunc().foldRight(z)(f))
        case _ => z
      }

    def exists(p : A => Boolean) : Boolean =  {
      foldRight(false)((a, b) => p(a) || b)
    }

//    def forAll(p: A => Boolean) : Boolean = this match {
//      case Cons(hFunc, tFunc) =>
//        p(hFunc()) && tFunc().forAll(p)
//      case _ => true
//    }

    def forAll(p : A => Boolean) : Boolean = {
      foldRight(true)((a,b) => p(a) && b)
    }

    def map[B](f : A => B) : Stream[B] = this match {
      case Cons(hFunc, tFunc) => Cons[B](() => f(hFunc()), () => tFunc().map(f))
      case _ => Empty
    }

    def filter(p : A => Boolean) : Stream[A] = this match {
      case Cons(hFunc, tFunc) => {
        if (p(hFunc()))
          Cons(hFunc, () => tFunc().filter(p))
        else tFunc().filter(p)
      }
      case _ => Empty
    }

  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](args: ( () => A   )*): Stream[A] = {
      if (args.isEmpty) empty
      else cons(args.head(), apply(args.tail : _*))
    }

    def from(n: Int) : Stream[Int] = {
      cons(n, from(n + 1))
    }

  }

  def main(args: Array[String]): Unit = {
    println("before stream init")

    val firstStream = Cons(
      () => { println("2 + 3"); 2 + 3 },
      () => Cons(
        () => 1 + 7,
        () => Empty
      ))

    println("after stream init")
    println(firstStream.head())
    println(firstStream.head())
    println("after multiple stream.head")
    println("before smartStream init")

    val smartStream = Stream.cons(
      { println("expensive 1"); 1 },
      Stream.empty
    )

    println("after smartStream init")
    println(smartStream.headOption)
    println(smartStream.headOption)
    println("after multiple smartStream.headOption")
    println("before streamOfVariableArgs init")

    val streamOfVariableArgs = Stream(
      () => { println("expensive 2"); 2 },
      () => { println("expensive 3"); 3 },
      () => { println("expensive 4"); 4 },
      () => { println("expensive 5"); 5 }
    )

    println("after streamOfVariableArgs init")
//    println(streamOfVariableArgs.headOption)
//    println(streamOfVariableArgs.headOption)

//    println("after multiple streamOfVariableArgs.headOption")
//
//    println(streamOfVariableArgs.toList)
//
//    println(streamOfVariableArgs.take(3).toList)
//    println("after multiple streamOfVariableArgs.take(3).toList")
//
//    println(streamOfVariableArgs.drop(2).toList)
//    println(streamOfVariableArgs.forAll((a) => {a < 4}))
//    println(streamOfVariableArgs.exists((a) => {a == 5}))
//    println(streamOfVariableArgs.map(x => {println("mapped"); x + 1}).take(2).toList)
    println(streamOfVariableArgs.filter(x => x % 2 == 0).toList)
    println(streamOfVariableArgs.filter(x => x % 2 == 0).toList)


    val more = Stream.from(100)

    println(more.take(5).toList)

    val fibs = {
      def next(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, next(f1, f0 + f1))

      next(0, 1)
    }

    println(fibs.take(5).toList)

    val primes = {
      def isPrime(p : Int) : Boolean = p match {
        case 1 => false
        case _ => Stream.from(2).take(p - 2).forAll(x => !(p % x == 0) )
      }

      def nextPrime(p0: Int) : Stream[Int] = {
        if (isPrime(p0))
          Stream.cons(p0, nextPrime(p0 + 1))
        else nextPrime(p0 + 1)
      }

      nextPrime(2)
    }

    println(primes.take(7).toList)
  }




}
