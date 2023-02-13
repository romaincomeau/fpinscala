package com.laziness

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

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1  => cons(h, t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h, empty)
    case _                    => empty

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  def main(args: Array[String]): Unit =
    val nums = Cons(() => 1, () => Cons(() => 2, () => Empty))
    val fruits = cons("banana", cons("dragon fruit", empty))
    val people = cons("Romain", cons("Claude", cons("Guillaume", empty)))
