package com.errorhandling

import scala.{Option as _, Some as _, None as _} // hide std lib

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None    => None
    case Some(a) => Some(f(a))

  // B >: A   says that the B type parameter must
  // be a supertype of A
  def getOrElse[B >: A](default: => B): B = this match
    case None    => default
    case Some(a) => a

  def flatMap[B](f: A => Option[B]): Option[B] =
    this map f getOrElse None

  // pattern matched flatMap
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match
    case None    => None
    case Some(a) => f(a)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case None    => ob
    case Some(a) => Some(a)

  def filter(f: A => Boolean): Option[A] = this match
    case Some(a) if f(a) => this
    case _               => None

object Option:
  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail~")
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail~")): Int)
    catch case e: Exception => 43
