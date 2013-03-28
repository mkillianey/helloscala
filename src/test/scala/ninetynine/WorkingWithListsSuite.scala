package ninetynine

import org.scalatest._

class WorkingWithListsSuite extends FunSuite {
  
  import ninetynine.WorkingWithLists._
  
  val MANY = 10000
  
  test("P01: last") {
    assert(last(List(1, 2, 3, 4)) === 4)
    assert(last("zookeeper".toList) === 'r')
    assert(last(List("one")) === "one")
    assert(last((1 to MANY).toList) === MANY)
  }

  test("P02: penultimate") {
    assert(penultimate(List(1, 2, 3, 4)) === 3)
    assert(penultimate("zookeeper".toList) === 'e')
    assert(penultimate(List("one", "two")) === "one")
    assert(penultimate((1 to MANY).toList) === MANY-1)
  }

  test("P03: nth") {
    assert(nth(0, List("boy")) === "boy")
    assert(nth(0, List(1, 2, 4, 8)) === 1)
    assert(nth(1, List(1, 2, 4, 8)) === 2)
    assert(nth(2, List(1, 2, 4, 8)) === 4)
    assert(nth(3, List(1, 2, 4, 8)) === 8)
    assert(nth(MANY, (0 to MANY).toList) === MANY)
  }
  
  test("P04: length") {
    assert(length(List("boy")) === 1)
    assert(length(List(1, 2, 4, 8)) === 4)
    assert(length((1 to MANY).toList) === MANY)
  }
  
  test("P05: reverse") {
    assert(reverse(List()) === List())
    assert(reverse(List(1)) === List(1))
    assert(reverse(List(1, 2, 3)) === List(3, 2, 1))
    assert(reverse((1 to MANY).toList) === (MANY to 1 by -1).toList)
  }
  
  test("P06: isPalindrome") {
    assert(isPalindrome("madam".toList))
    assert(!isPalindrome("sir".toList))
    assert(!isPalindrome((1 to MANY).toList))
    assert(isPalindrome((-MANY to MANY).map(_.abs).toList))
  }
  
  test("P07: flatten") {
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8))))
      === List(1, 1, 2, 3, 5, 8))
    assert(flatten(List(List(List(List(List()))))) === List())
    assert(flatten(List(List(List(List(List(99)))))) === List(99))
  }

  test("P07 (alt): altflatten") {
    assert(altflatten(List(List(1, 1), 2, List(3, List(5, 8))))
      === List(1, 1, 2, 3, 5, 8))
    assert(altflatten(List(List(List(List(List()))))) === List())
    assert(altflatten(List(List(List(List(List(99)))))) === List(99))
  }

  test("P08: compress") {
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      === List('a, 'b, 'c, 'a, 'd, 'e))
    assert(compress(List('a, 'a, 'a))
      === List('a))
  }
  
  test("P09: pack") {
    assert(pack(
      List('a, 'a, 'a, 'a,
        'b,
        'c, 'c,
        'a, 'a,
        'd,
        'e, 'e, 'e, 'e))
      === List(List('a, 'a, 'a, 'a),
               List('b),
               List('c, 'c),
               List('a, 'a),
               List('d),
               List('e, 'e, 'e, 'e)))
  }
  
  test("P10: encode") {
    assert(encode(
      List('a, 'a, 'a, 'a,
        'b,
        'c, 'c,
        'a, 'a,
        'd,
        'e, 'e, 'e, 'e))
      === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }  

  test("P11: encodeModified") {
    assert(encodeModified(
      List('a, 'a, 'a, 'a,
           'b,
           'c, 'c,
           'a, 'a,
           'd,
           'e, 'e, 'e, 'e))
      === List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }
  
  test("P12: decode") {
    assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
      === List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }
  
  test("P13: encodeDirect") {
    assert(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
  
  test("P14: duplicate") {
    assert(duplicate(List('a, 'b, 'c, 'c, 'd))
      === List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }
  
  test("P15: duplicateN") {
    assert(duplicateN(0, List('a, 'b, 'c, 'c, 'd))
      === List())
    assert(duplicateN(1, List('a, 'b, 'c, 'c, 'd))
      === List('a, 'b, 'c, 'c, 'd))
    assert(duplicateN(3, List('a, 'b, 'c, 'c, 'd))
      === List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }
  
  test("P16: drop") {
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      === List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }
  
  test("P17: split") {
    assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        === (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
  
  test("P18: slice") {
    assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      === List('d, 'e, 'f, 'g))
    assert(slice(7, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      === List())
    assert(slice(0, 11, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      === List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    intercept[IndexOutOfBoundsException] { slice(-2, 0, List('a, 'b, 'c)) }
    intercept[IndexOutOfBoundsException] { slice(3, 5, List('a, 'b, 'c)) }
  }
  
  test("P19: rotate") {
    assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      === List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      === List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))

    val abcd = List('a, 'b, 'c, 'd)
    val bcda = List('b, 'c, 'd, 'a)
    val cdab = List('c, 'd, 'a, 'b)
    val dabc = List('d, 'a, 'b, 'c)
    assert(rotate(0, abcd) === abcd)
    assert(rotate(1, abcd) === bcda)
    assert(rotate(2, abcd) === cdab)
    assert(rotate(3, abcd) === dabc)
    assert(rotate(-1, abcd) === rotate(3, abcd))
    assert(rotate(-2, abcd) === rotate(2, abcd))
    assert(rotate(-3, abcd) === rotate(1, abcd))
    assert(rotate((4 * MANY) + 0, abcd) === abcd)
    assert(rotate((4 * MANY) + 1, abcd) === rotate(1, abcd))
    assert(rotate((4 * MANY) + 2, abcd) === rotate(2, abcd))
    assert(rotate((4 * MANY) + 3, abcd) === rotate(3, abcd))
    assert(rotate(-(4 * MANY) + 0, abcd) === abcd)
    assert(rotate(-(4 * MANY) + 1, abcd) === rotate(1, abcd))
    assert(rotate(-(4 * MANY) + 2, abcd) === rotate(2, abcd))
    assert(rotate(-(4 * MANY) + 3, abcd) === rotate(3, abcd))

    assert(rotate(-MANY, List()) == List())
    assert(rotate(0, List()) == List())
    assert(rotate(MANY, List()) == List())
  }

  test("P20: removeAt") {
    assert(removeAt(0, List('a, 'b, 'c, 'd)) === (List('b, 'c, 'd),'a))
    assert(removeAt(1, List('a, 'b, 'c, 'd)) === (List('a, 'c, 'd),'b))
    assert(removeAt(2, List('a, 'b, 'c, 'd)) === (List('a, 'b, 'd),'c))
    assert(removeAt(3, List('a, 'b, 'c, 'd)) === (List('a, 'b, 'c),'d))
    intercept[IndexOutOfBoundsException] { removeAt(-1, List('a, 'b, 'c, 'd)) }
    intercept[IndexOutOfBoundsException] { removeAt(4, List('a, 'b, 'c, 'd)) }
  }
  
  test("P21: insertAt") {
    assert(insertAt('new, 0, List('a, 'b, 'c, 'd)) === List('new, 'a, 'b, 'c, 'd))
    assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) === List('a, 'new, 'b, 'c, 'd))
    assert(insertAt('new, 4, List('a, 'b, 'c, 'd)) === List('a, 'b, 'c, 'd, 'new))
    intercept[IndexOutOfBoundsException] { insertAt('new, -1, List('a, 'b, 'c, 'd)) }
    intercept[IndexOutOfBoundsException] { insertAt('new, 5, List('a, 'b, 'c, 'd)) }
  }

  test("P22: range") {
    assert(range(4, 9) === List(4, 5, 6, 7, 8, 9))
    assert(range(4, 4) === List(4))
    intercept[IllegalArgumentException] { range(5, 4) }
  }

  test("P23: randomSelect") {
    val items = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j)

    assert(randomSelect(0, items) === List())

    val choose1 = randomSelect(1, items)
    assert(choose1.length === 1)
    assert(choose1.forall(items.contains(_)))

    val choose3 = randomSelect(3, items)
    assert(choose3.length === 3)
    assert(choose3.forall(items.contains(_)))

    val choose10 = randomSelect(10, items)
    assert(choose10.length === 10)
    assert(choose10.forall(items.contains(_)))

    intercept[IllegalArgumentException] { randomSelect(11, items) }
  }

  ignore("P23: randomSelect timing") { // should be O(n), is currently O(n^2)
    val items = (1 to MANY * 10).toList
    randomSelect(MANY * 10, items)
  }

  test("P24: lotto") {
    val numbers = lotto(6, 49)
    assert(numbers.length === 6)
    assert(numbers.forall(_ >= 1))
    assert(numbers.forall(_ <= 49))
  }

  test("P25: randomPermute") {
    val items = (1 to 12).toList
    val permutedItems = randomPermute(items)
    assert(items.length === permutedItems.length)
    assert(items.sorted === permutedItems.sorted)
    assert(items != permutedItems)

    val differentPermutation = randomPermute(items)
    assert(items.length === differentPermutation.length)
    assert(items.sorted === differentPermutation.sorted)
    assert(items != differentPermutation)
    assert(permutedItems != differentPermutation)
  }

  test("P26: combinations") {
    val cs = combinations(3, (1 to 12).toList)
    assert(cs.contains(List(1, 2, 3)))
    assert(cs.contains(List(10, 11, 12)))
    assert(cs.length === 220) // 12! / (9! * 3!) = 479001600 / (362880 * 6) = 220
    assert(cs.forall(_.length == 3))
  }

  test("P27a: group3") {
    val items = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val groupings = group3(items)
    assert(groupings.size === 1260) // 9! / (2! * 3! * 4!) = 362880 / (2 * 6 * 24) = 1260
    assert(groupings.contains(List(List("Aldo", "Beat"),
                                   List("Carla", "David", "Evi"),
                                   List("Flip", "Gary", "Hugo", "Ida"))))
    assert(groupings.contains(List(List("Hugo", "Ida"),
                                   List("Evi", "Flip", "Gary"),
                                   List("Aldo", "Beat", "Carla", "David"))))
  }

  test("P28: group") {
    val items = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val groupings = group(List(2, 2, 5), items)
    assert(groupings.size === 756) // 9! / (2! * 2! * 5!)) = 362880 / (2 * 2 * 120) = 756
    assert(groupings.contains(List(List("Aldo", "Beat"),
                                   List("Carla", "David"),
                                   List("Evi", "Flip", "Gary", "Hugo", "Ida"))))
    assert(groupings.contains(List(List("Hugo", "Ida"),
                                   List("Flip", "Gary"),
                                   List("Aldo", "Beat", "Carla", "David", "Evi"))))
  }

  test("P29: lsort") {
    assert(
      lsort(List(List('a, 'b, 'c),
                 List('d, 'e),
                 List('f, 'g, 'h),
                 List('d, 'e),
                 List('i, 'j, 'k, 'l),
                 List('m, 'n),
                 List('o)))
      === List(List('o),
               List('d, 'e),
               List('d, 'e),
               List('m, 'n),
               List('a, 'b, 'c),
               List('f, 'g, 'h),
               List('i, 'j, 'k, 'l)))
  }
  test("P30: lsortFreq") {
    assert(
      lsortFreq(List(List('a, 'b, 'c),
                     List('d, 'e),
                     List('f, 'g, 'h),
                     List('d, 'e),
                     List('i, 'j, 'k, 'l),
                     List('m, 'n),
                     List('o)))
      === List(List('i, 'j, 'k, 'l),
               List('o),
               List('a, 'b, 'c),
               List('f, 'g, 'h),
               List('d, 'e),
               List('d, 'e),
               List('m, 'n)))
  }
}
