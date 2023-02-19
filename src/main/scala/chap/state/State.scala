package chap.state

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)



trait RNG:
  def nextInt: (Int, RNG)

object RNG:
  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(x => x / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng0 =>
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)


  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)


  def double(rng: RNG): (Double, RNG) =
    val (i, r) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), r)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r0) = rng.nextInt
    val (d, r1) = double(r0)
    ((i, d), r1)

  def doubleInt(rng: RNG): ((Int, Int), RNG) =
    val (i0, r0) = rng.nextInt
    val (i1, r1) = r0.nextInt
    ((i0, i1), r1)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d0, r0) = double(rng)
    val (d1, r1) = double(r0)
    val (d2, r2) = double(r1)
    ((d0, d1, d2), r2)

  def ints(n: Int)(rng: RNG): (List[Int], RNG) =
    @annotation.tailrec
    def go(xs: List[Int])(rng: RNG)(n: Int): (List[Int], RNG) = n match
      case x if x > 0 => {
        val (i, r) = rng.nextInt
        go(i::xs)(r)(n-1)
      }
      case _ => (xs, rng)

    go(Nil: List[Int])(rng)(n)

  def ints_2(n: Int)(rng: RNG): (List[Int], RNG) =
    @annotation.tailrec
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if count <= 0 then
        (xs, r)
      else
        val (x, r2) = r.nextInt
        go(count-1, r2, x :: xs)

    go(n, rng, Nil)


  @main def state: Unit =
    val rng = Simple(System.currentTimeMillis())
    val (i, r) = ints(5000)(rng)





  

