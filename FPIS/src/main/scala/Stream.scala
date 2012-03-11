object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] {
      def uncons = None
    }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  lazy val ones: Stream[Int] =
    cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(i: Int): Stream[Int] = cons(i, from(i + 1))

  def fibs: Stream[Int] = 0 #:: 1 #:: unfold(0, 1) {
    case (a, b) => Some(a + b, (b, a + b))
  }

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] = f(s).map {
    case (a, s2) => cons(a, unfold(s2)(f))
  } getOrElse empty[A]

  def main(args: Array[String]) {
    val s = Stream("a", "b", "c", "d", "e", "f")
    println(s.toList)
    println(ones.take(5).toList)
    println(constant(10).take2(5).toList)
    println(from(2).take(5).toList)
    println(s.takeWhile(_ != "d").toList)
    println(ones.exists(_ == 1))
    println(from(1).takeWhile3(_ < 10).toList)
    println(s.map(_.toUpperCase).toList)
    println(s.filter(_ < "e").toList)
    println(ones.map(_ + 1).take(5).toList)
    println(ones.map2(_ + 1).take(5).toList)
    println((s ++ ones.map(_.toString)).take(20).toList)
    println((ones ++ ones).take(20).toList)
    println((from(1) zip from(2)).take(5).toList)
    println((from(1).take(2) zipAll from(2).take(3)).toList)
    println((from(1).take(3) zipAll from(2).take(2)).toList)
    println(s.startsWith(Stream("a", "b")))
    println(ones.startsWith(Stream(1, 1, 1)))
    println(ones.startsWith(Stream(1, 1, 2)))
    println(s.tails.map(_.toList).toList)
    println(s.hasSubsequence(Stream("c", "d")))
    println(s.hasSubsequence(Stream("c", "x")))
    println(ones.tails.take(1).toList)
    println(ones.hasSubsequence(Stream(1, 1)))
    println(fibs.take(20).toList)
  }
}

trait Stream[A] {

  import Stream._

  def uncons: Option[(A, Stream[A])]

  def #::(a: => A) = cons(a, this)

  def toList: List[A] = foldRight(Nil: List[A]) {
    case (x, xs) => x :: xs
  }

  def take(n: Int): Stream[A] = if (n <= 0) empty
  else uncons.map {
    case (x, xs) => cons(x, xs.take(n - 1))
  } getOrElse empty

  def takeWhile(f: A => Boolean): Stream[A] = foldRight(empty[A]) {
    (x, xs) =>
      if (f(x)) cons(x, xs) else empty[A]
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons.map {
    case (h, t) => f(h, t.foldRight(z)(f))
  }.getOrElse(z)

  def exists(f: A => Boolean): Boolean = foldRight(false)((h, t) => f(h) || t)

  def forall(f: A => Boolean): Boolean = foldRight(true)((h, t) => f(h) && t)

  def map[B](f: A => B): Stream[B] = foldRight(empty[B]) {
    (h, t) => cons(f(h), t)
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A]) {
    (h, t) => if (f(h)) cons(h, t) else t
  }

  def ++(s2: Stream[A]): Stream[A] = foldRight(s2) {
    (h, t) => cons(h, t)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]) {
    (h, t) => f(h) ++ t
  }

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    _.uncons.map {
      case (h, t) => (f(h), t)
    }
  }

  def take2(n: Int): Stream[A] = unfold(n, this) {
    case (n2, s) => if (n2 <= 0) None
    else s.uncons.map {
      case (h, t) => (h, (n2 - 1, t))
    }
  }

  def takeWhile3(f: A => Boolean): Stream[A] = unfold(this) {
    _.uncons flatMap {
      t => if (f(t._1)) Some(t) else None
    }
  }

  def zip[B](s2: Stream[B]): Stream[(A, B)] = unfold(this, s2) {
    case (a, b) => for (as <- a.uncons; bs <- b.uncons)
    yield ((as._1, bs._1), (as._2, bs._2))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this, s2) {
    case (a, b) => a.uncons map {
      as =>
        Some(b.uncons map {
          bs =>
            ((Some(as._1), Some(bs._1)), (as._2, bs._2))
        } getOrElse {
          ((Some(as._1), None), (as._2, empty[B]))
        })
    } getOrElse {
      b.uncons map {
        bs =>
          ((None, Some(bs._1)), (empty[A], bs._2))
      }
    }
  }

  def startsWith(s: Stream[A]): Boolean = this.zip(s) forall {
    case (a, b) => a == b
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    _.uncons map {
      case (h, t) => (cons(h, t), t)
    }
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(z)(f))

  def hasSubsequence(s: Stream[A]): Boolean = tails exists (_ startsWith s)
}