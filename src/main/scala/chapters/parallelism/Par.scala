package chapters.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par:
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) =>
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))


object Examples:
  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if(ints.size <= 1) then
      Par.unit(ints.headOption getOrElse 0)
    else
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)

  @main def main: Unit =
    val nums = IndexedSeq(1, 2, 3, 4, 5, 6, 7)
    val res = sum(nums)
    println(res)



