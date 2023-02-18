package chapters.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, t) => Some(h())

  /** my error was: `case Cons(h, t) => h() :: toList(t())` I was calling on a
    * function when I should have called on `t()`'s method toListRecursive. *
    */
  def toListRecursive: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toListRecursive

  def toList: List[A] =
    @annotation.tailrec
    def go(acc: List[A], ll: LazyList[A]): List[A] = ll match
      case Empty      => acc.reverse
      case Cons(h, t) => go(h() :: acc, t())

    go(Nil: List[A], this)

  /** In order to avoid the `reverse` at the end we could write it using a
    * mutable list buffer and an explicit loop instead. Note that the mutable
    * list buffer never escapes our `toList` method so this function is still
    * _pure_. *
    */
  def toListFast: List[A] =
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(ll: LazyList[A]): List[A] = ll match
      case Cons(h, t) => buf += h(); go(t())
      case Empty      => buf.toList

    go(this)

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty

  def exists(p: A => Boolean): Boolean = this match
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false

  // the arrow `=>` in front of the argument type B means that the function
  // `f` takes its second arg by name and may choose not to evaluate it
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if (p(a)) then cons(a, b) else empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((h, acc) => cons(f(h), acc))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A])((h, acc) => if f(h) then cons(h, acc) else acc)

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOptionViaFoldRight



object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = cons(1, ones)

  def constant[A](a: A): LazyList[A] =
    cons(a, constant(a))

  def constant_2[A](a: A): LazyList[A] =
    lazy val tail: LazyList[A] = Cons(() => a, () => tail)
    tail

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  val fibs =
    def go(f0: Int, f1: Int): LazyList[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] = f(z) match
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None         => empty


  def fibsViaUnfold =
    unfold((0,1)) { case (current, next) =>
      Some((current, (next, current + next)))
    }
    
  def constantViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some((a, ())))

  def onesViaUnfold: LazyList[Int] = 
    unfold(())(_ => Some((1, ())))

  @main def x: Unit =
    val xs = unfold(0)(x => Some(x + 1, x + 1))
    println(xs.take(10).toList)
