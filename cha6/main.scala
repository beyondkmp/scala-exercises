class Rational(n: Int, d: Int) {
  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val numer: Int = n / g
  val denom: Int = d / g

  def this(n: Int) = this(n, 1)

  override def toString = numer + "/" + denom

  def +(that: Rational): Rational =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def +(i: Int): Rational =
    new Rational(numer + i * denom, denom)

  def -(that: Rational): Rational =
    new Rational(numer * that.denom - that.numer * denom, denom * that.denom)

  def -(i: Int): Rational =
    new Rational(numer - i * denom, denom)

  def *(that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)

  def *(i: Int): Rational =
    new Rational(numer, denom * i)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}

object Main {
  implicit def intToRational(x: Int) = new Rational(x)
  def main(args: Array[String]): Unit = {

    val x = new Rational(1, 2)
    val y = new Rational(8, 9)

    println(x + y)
    println(x * y)
    println(x + x * y)

    val z = new Rational(10)
    println(z)

    var m = new Rational(66, 42)
    println(m)

    var r = new Rational(2, 3)
    println(2 * r)
  }
}
