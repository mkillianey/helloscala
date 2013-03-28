package ninetynine

import org.scalatest._

class ArithmeticSuite extends FunSuite {

  import ninetynine.S99Int._

  test("P31: isPrime") {
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

  test("P32: gcd") {
    assert(gcd(5, 7) === 1)
    assert(gcd(6, 16) === 2)
    assert(gcd(18, 30) === 6)
  }
  test("P33: isCoprimeTo") {
    assert(34 isCoprimeTo 35)
    assert(!(30 isCoprimeTo 35))
  }
  test("P34: totient") {
    assert(10.totient === 4)
  }

  test("P35: primeFactors") {
    intercept[ArithmeticException] { 0.primeFactors }
    intercept[ArithmeticException] { -1.primeFactors }
    assert(1.primeFactors === List())
    assert(2.primeFactors === List(2))
    assert(30.primeFactors === List(2, 3, 5))
    assert(110.primeFactors === List(2, 5, 11))
    assert(24.primeFactors === List(2, 2, 2, 3))
    assert(315.primeFactors === List(3, 3, 5, 7))
  }

  test("P36: primeFactorMultiplicity") {
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

  ignore("P37:") {
  }
  ignore("P38:") {
  }
  ignore("P39:") {
  }
  ignore("P40:") {
  }
  ignore("P41:") {
  }
  ignore("P42:") {
  }
  ignore("P43:") {
  }
  ignore("P44:") {
  }
  ignore("P45:") {
  }
  ignore("P46:") {
  }
  ignore("P47:") {
  }
  ignore("P48:") {
  }
  ignore("P49:") {
  }
  ignore("P50:") {
  }
  ignore("P51:") {
  }
  ignore("P52:") {
  }
  ignore("P53:") {
  }
  ignore("P54:") {
  }
  ignore("P55:") {
  }
  ignore("P56:") {
  }
  ignore("P57:") {
  }
  ignore("P58:") {
  }
  ignore("P59:") {
  }
  ignore("P60:") {
  }
  ignore("P61:") {
  }
  ignore("P62:") {
  }
  ignore("P63:") {
  }
  ignore("P64:") {
  }
  ignore("P65:") {
  }
  ignore("P66:") {
  }
  ignore("P67:") {
  }
  ignore("P68:") {
  }
  ignore("P69:") {
  }
  ignore("P70:") {
  }
  ignore("P71:") {
  }
  ignore("P72:") {
  }
  ignore("P73:") {
  }
  ignore("P74:") {
  }
  ignore("P75:") {
  }
  ignore("P76:") {
  }
  ignore("P77:") {
  }
  ignore("P78:") {
  }
  ignore("P79:") {
  }
  ignore("P80:") {
  }
  ignore("P81:") {
  }
  ignore("P82:") {
  }
  ignore("P83:") {
  }
  ignore("P84:") {
  }
  ignore("P85:") {
  }
  ignore("P86:") {
  }
  ignore("P87:") {
  }
  ignore("P88:") {
  }
  ignore("P89:") {
  }
  ignore("P90:") {
  }
  ignore("P91:") {
  }
  ignore("P92:") {
  }
  ignore("P93:") {
  }
  ignore("P94:") {
  }
  ignore("P95:") {
  }
  ignore("P96:") {
  }
  ignore("P97:") {
  }
  ignore("P98:") {
  }
  ignore("P99:") {
  }
}