package ninetynine

import org.scalatest.Assertions
import org.junit.{Ignore, Test}

class ArithmeticTests extends Assertions {

  import ninetynine.S99Int._

  @Test
  def P31_isPrime() {
    assert(!(-1.isPrime))
    assert(!(0.isPrime))
    assert(!(1.isPrime))
    assert(2.isPrime)
    assert(3.isPrime)
    assert(!(4.isPrime))
    assert(5.isPrime)
    assert(!(6.isPrime))
    assert(7.isPrime)
    assert(!(8.isPrime))
    assert(!(9.isPrime))
    assert(Integer.MAX_VALUE.isPrime)
  }

  @Test
  def P32_gcd() {
    assert(gcd(5, 7) === 1)
    assert(gcd(6, 16) === 2)
    assert(gcd(18, 30) === 6)
  }
  @Test
  def P33_isCoprimeTo() {
    assert(1 isCoprimeTo 1)
    assert(1 isCoprimeTo 2)
    assert(1 isCoprimeTo 3)
    assert(2 isCoprimeTo 3)
    assert(34 isCoprimeTo 35)
    assert(!(30 isCoprimeTo 35))
  }
  @Test
  def P34_totient() {
    assert(10.totient === 4)
  }

  @Test
  def P35_primeFactors() {
    intercept[ArithmeticException] { 0.primeFactors }
    intercept[ArithmeticException] { -1.primeFactors }
    assert(1.primeFactors === List())
    assert(2.primeFactors === List(2))
    assert(30.primeFactors === List(2, 3, 5))
    assert(110.primeFactors === List(2, 5, 11))
    assert(24.primeFactors === List(2, 2, 2, 3))
    assert(315.primeFactors === List(3, 3, 5, 7))
  }

  @Test
  def P36_primeFactorMultiplicity() {
    intercept[ArithmeticException] { 0.primeFactorMultiplicity }
    intercept[ArithmeticException] { -1.primeFactorMultiplicity }
    assert(1.primeFactorMultiplicity === List())
    assert(2.primeFactorMultiplicity === List((2, 1)))
    assert(30.primeFactorMultiplicity === List((2, 1), (3, 1), (5, 1)))
    assert(110.primeFactorMultiplicity === List((2, 1), (5, 1), (11, 1)))
    assert(24.primeFactorMultiplicity === List((2, 3), (3, 1)))
    assert(36.primeFactorMultiplicity === List((2, 2), (3, 2)))
    assert(315.primeFactorMultiplicity === List((3, 2), (5, 1), (7, 1)))
  }

  @Test
  def P37_totient2() {
    (1 to 1000).foreach(
      n => assert(n.totient2 === n.totient,
        s"totient2 should match totient for n = $n"))
  }

  @Test
  def P38_compareP34andP37() {
    def checkInt(n : Int) {
      assert(n.totient2 === n.totient, s"totient2 should match totient for n = $n")
    }
    (1 to 1000).foreach { checkInt(_) }
    checkInt(10090)
  }

  @Test
  def P39_listPrimesInRange() {
    assert(listPrimesInRange(1 to 10) === List(2, 3, 5, 7))
    assert(listPrimesInRange(11 to 20) === List(11, 13, 17, 19))
  }
  @Test
  def P40_goldbach() {
    assert(28.goldbach === (5, 23))
  }
}
