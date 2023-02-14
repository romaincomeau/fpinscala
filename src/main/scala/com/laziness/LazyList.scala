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

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if(p(a)) then cons(a, b) else empty)

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))
