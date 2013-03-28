package ninetynine

import scala.annotation.tailrec
import util.Random

// Notes from S-99: Ninety-Nine Scala Problems
//    http://aperiodic.net/phil/scala/s-99/


/**
 * In Scala, lists are objects of type List[A], where A can be any type. Lists are effective for many recursive algorithms, because it's easy to add elements to the head of a list, and to get the tail of the list, which is everything but the first element.

The solutions to the problems in this section will be in objects named after the problems (P01, P02, etc.). You can compile the source files with scalac and thereafter use import to bring the functions into scope. Some of the problems can be solved easily by using imported solutions to previous problems.

In many cases, there's more than one reasonable approach. The files linked here may include multiple solutions, with all but one commented out. They'll also indicate whether there's a builtin method in Scala that accomplishes the task.
 */
object WorkingWithLists {

  /**
   * P01 (*) Find the last element of a list.
   * Example:
   * scala> last(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 8
   */
  @tailrec
  def last[T](ts: List[T]): T = ts match {
    case t :: Nil => t
    case _ :: rest => last(rest)
  }

  List().last

  /**
   * P02 (*) Find the last but one element of a list.
   * Example:
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
  @tailrec
  def penultimate[T](ts: List[T]): T = ts match {
    case t :: _ :: Nil => t
    case _ :: rest => penultimate(rest)
  }

  /**
   * P03 (*) Find the Kth element of a list.
   * By convention, the first element in the list is element 0.
   * Example:
   *
   * scala> nth(2, List(1, 1, 2, 3, 5, 8))
   * res0: Int = 2
   */
  @tailrec
  def nth[T](n: Int, ts: List[T]): T =
    (n, ts) match {
      case (0, t :: _) => t
      case (m, _ :: rest) => nth(m - 1, rest)
      case _ => throw new IndexOutOfBoundsException
    }

  /**
   * P04 (*) Find the number of elements of a list.
   * Example:
   * scala> length(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 6
   */
  def length[T](ts: List[T]): Int = {
    @tailrec
    def helper(items: List[T], acc: Int): Int = items match {
      case _ :: rest => helper(rest, acc + 1)
      case Nil => acc
    }
    helper(ts, 0)
  }

  /**
   * P05 (*) Reverse a list.
   * Example:
   * scala> reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverse[T](ts: List[T]): List[T] = {
    @tailrec
    def helper(ts: List[T], acc: List[T]): List[T] = ts match {
      case t :: rest => helper(rest, t :: acc)
      case Nil => acc
    }
    helper(ts, List())
  }

  /**
   * P06 (*) Find out whether a list is a palindrome.
   * Example:
   * scala> isPalindrome(List(1, 2, 3, 2, 1))
   * res0: Boolean = true
   */
  def isPalindrome[T](ts: List[T]) = ts == reverse(ts)

  /**
   * P07 (**) Flatten a nested list structure.
   * Example:
   * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */
  def flatten(nested: List[Any]): List[Any] = {
    def helper(items: Any, rev: List[Any]): List[Any] = items match {
      case first :: rest => helper(rest, helper(first, rev))
      case Nil => rev
      case item => item :: rev
    }
    reverse(helper(nested, List()))
  }

  def altflatten(xs: List[Any]): List[Any] = xs.flatMap(_ match {
    case x: List[_] => altflatten(x)
    case x => List(x)
  })

  /**
   * P08 (**) Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with
   * a single copy of the element. The order of the elements should not be changed.
   * Example:
   * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */
  def compress[T](ts: List[T]): List[T] = ts match {
    case x :: y :: rest if x == y => compress(y :: rest)
    case x :: rest => x :: compress(rest)
    case x => x
  }

  /**
   * P09 (**) Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   * Example:
   *
   * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   */
  def pack[T](ts: List[T]): List[List[T]] = {
    @tailrec
    def helper(items: List[T], rev: List[List[T]]): List[List[T]] = (items, rev) match {
      case (first :: rest, (item :: b) :: packed) if item == first => helper(rest, (first :: (item :: b)) :: packed)
      case (first :: rest, packed) => helper(rest, List(first) :: packed)
      case (Nil, a) => reverse(a)
    }
    helper(ts, List())
  }

  /**
   * P10 (*) Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length
   * encoding data compression method. Consecutive duplicates of elements
   * are encoded as tuples (N, E) where N is the number of duplicates of
   * the element E.
   *
   * Example:
   *
   * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encode[T](ts: List[T]): List[(Int, T)] =
    pack(ts).map(ts => (ts.length, ts.head))

  /**
   * P11 (*) Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
   * Example:
   *
   * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   */
  def encodeModified[T](ts: List[T]): List[Any] =
    pack(ts).map(x => if (x.length == 1) x.head else (x.length, x.head))

  /**
   *     P12 (**) Decode a run-length encoded list.
   * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
   * Example:
   *
   * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
   * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   */
  def decode[T](ts: List[(Int, T)]): List[T] = {
    @tailrec
    def helper(counts: List[(Int, T)], rev: List[T]): List[T] = counts match {
      case (1, item) :: rest => helper(rest, item :: rev)
      case (n, item) :: rest => helper((n - 1, item) :: rest, item :: rev)
      case Nil => reverse(rev)
    }
    helper(ts, List())
  }
  /**
   * P13 (**) Run-length encoding of a list (direct solution).
   * Implement the so-called run-length encoding data compression method
   * directly. I.e. don't use other methods you've written (like P09's pack);
   * do all the work directly.
   *
   * Example:
   *
   * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encodeDirect[T](ts: List[T]): List[(Int, T)] = {
    @tailrec
    def helpReverse(in: List[(Int, T)], out: List[(Int, T)]): List[(Int, T)] = in match {
      case first :: rest => helpReverse(rest, first :: out)
      case _ => out
    }
    @tailrec
    def helpEncode(items: List[T], rev: List[(Int, T)]): List[(Int, T)] = (items, rev) match {
      case (first :: rest, (count, item) :: encoded) if first == item => helpEncode(rest, (count + 1, item) :: encoded)
      case (first :: rest, encoded) => helpEncode(rest, (1, first) :: encoded)
      case (Nil, encoded) => helpReverse(encoded, List())
    }
    helpEncode(ts, List())
  }
  /**
   * P14 (*) Duplicate the elements of a list.
   * Example:
   * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */
  def duplicate[T](ts: List[T]): List[T] = {
    @tailrec
    def helper(items: List[T], rev: List[T]): List[T] = items match {
      case first :: rest => helper(rest, first :: first :: rev)
      case _ => reverse(rev)
    }
    helper(ts, List())
  }
  /**
   * P15 (**) Duplicate the elements of a list a given number of times.
   * Example:
   * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
   */
  def duplicateN[T](n: Int, ts: List[T]): List[T] = {
    @tailrec
    def helper(nMore: Int, items: List[T], rev: List[T]): List[T] = (items, nMore) match {
      case (x :: xs, 0) => helper(n, xs, rev)
      case (x :: xs, m) => helper(m - 1, items, x :: rev)
      case _ => reverse(rev)
    }
    helper(n, ts, List())
  }

  /**
   * P16 (**) Drop every Nth element from a list.
   * Example:
   * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
   */
  def drop[T](n: Int, ts: List[T]): List[T] = {
    @tailrec
    def helper(take: Int, items: List[T], rev: List[T]): List[T] = items match {
      case first :: rest =>
        if (take > 1)
          helper(take - 1, rest, first :: rev)
        else
          helper(n, rest, rev)
      case Nil => reverse(rev)
    }
    helper(n, ts, List())
  }
  /**
   * P17 (*) Split a list into two parts.
   * The length of the first part is given. Use a Tuple for your result.
   * Example:
   *
   * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   */
  def split[T](n: Int, ts: List[T]): (List[T], List[T]) = {
    @tailrec
    def helper(nMore: Int, from: List[T], to: List[T]): (List[T], List[T]) = (nMore, from) match {
      case (_, Nil) => (reverse(to), from)
      case (n, _) if n <= 0 => (reverse(to), from)
      case (n, t :: ts) => helper(n - 1, ts, t :: to)
    }
    helper(n, ts, List())
  }

  /**
   * P18 (**) Extract a slice from a list.
   * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
   * Example:
   *
   * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g)
   */
  def slice[T](fromIncluding: Int, toExcluding: Int, ts: List[T]): List[T] = {
    @tailrec
    def helper(from: Int, to: Int, items: List[T], rev: List[T]): List[T] = (from, to, items) match {
      case (0, 0, _) => reverse(rev)
      case (0, n, x :: xs) if 0 < n => helper(0, n - 1, xs, x :: rev)
      case (m, n, x :: xs) if m < n => helper(m - 1, n - 1, xs, rev)
      case (_, _, Nil) => throw new IndexOutOfBoundsException
    }
    if (fromIncluding < 0 || fromIncluding > toExcluding) throw new IndexOutOfBoundsException
    else if (fromIncluding == toExcluding) List()
    else helper(fromIncluding, toExcluding, ts, List())
  }

  /**
   * P19 (**) Rotate a list N places to the left.
   * Examples:
   * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
   *
   * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
   */
  @tailrec
  def rotate[T](n: Int, ts: List[T]): List[T] =
    if (n == 0 || ts.isEmpty) ts
    else {
      val len = ts.length
      val nmod = n % len
      if (nmod < 0) rotate(nmod + len, ts)
      else ts.drop(nmod) ::: ts.take(nmod)
    }

  /**
   * P20 (*) Remove the Kth element from a list.
   * Return the list and the removed element in a Tuple. Elements are numbered from 0.
   * Example:
   *
   * scala> removeAt(1, List('a, 'b, 'c, 'd))
   * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
   */
  def removeAt[T](n : Int, ts : List[T]) : (List[T], T) =
    if (n < 0)
      throw new IndexOutOfBoundsException
    else
      ts.splitAt(n) match {
        case (list1, item :: list2) => (list1 ::: list2, item)
        case _ => throw new IndexOutOfBoundsException
      }
  
  
  /**
   * P21 (*) Insert an element at a given position into a list.
   * Example:
   * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
   * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
   */
  def insertAt[T](t : T, n : Int, ts : List[T]) : List[T] =
    if (n < 0 || n > ts.length)
      throw new IndexOutOfBoundsException
    else
      ts.splitAt(n) match {
        case (before, after) => before ::: (t :: after)
      }
  
  /**
   * P22 (*) Create a list containing all integers within a given range.
   * Example:
   * scala> range(4, 9)
   * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
   */
  def range(fromInclusive : Int, toInclusive: Int) : List[Int] = {
    def helper(m : Int, n : Int, accu : List[Int]) : List[Int] =
      if (m > n) accu else helper(m, n - 1, n :: accu)
    if (fromInclusive > toInclusive) throw new IllegalArgumentException
    else helper(fromInclusive, toInclusive, List())
  }

  /**
   * P23 (**) Extract a given number of randomly selected elements from a list.
   * Example:
   * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
   * res0: List[Symbol] = List('e, 'd, 'a)
   * Hint: Use the solution to problem P20
   *
   * TODO:  This could be O(n) if it didn't use P20.
   */
  def randomSelect[T](n : Int, ts : List[T]) : List[T] = {
    val r = new Random
    val tsLength = ts.length
    if (n > tsLength) throw new IllegalArgumentException
    else {
      @tailrec
      def helper(nWanted : Int, nAvailable : Int, available : List[T], chosen : List[T]) : List[T] =
        if (nWanted == 0)
          chosen
        else {
          val (remaining, item) = removeAt(r.nextInt(nAvailable), available)
          helper(nWanted - 1, nAvailable - 1, remaining, item :: chosen)
        }
      helper(n, tsLength, ts, List())
    }
  }

  /*
   * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
   * Example:
   * scala> lotto(6, 49)
   * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
   */
  def lotto(k : Int, n : Int) = randomSelect(k, (1 to n).toList)

  /*
   * P25 (*) Generate a random permutation of the elements of a list.
   * Hint: Use the solution of problem P23.
   * Example:
   *
   * scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
   * res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
   */
  def randomPermute[T](ts : List[T]) = randomSelect(ts.length, ts)

  /* P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
   * In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
   * Example:
   *
   * scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
   * res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
   */
  def combinations[T](n : Int, ts : List[T]) : List[List[T]] = (n, ts) match {
    case (0, _) => List(List()) // one combination for zero elements => that with no elements
    case (_, Nil) => List() // ran out of items!
    case (n, first :: rest) => combinations(n, rest) ::: (combinations(n-1, rest).map(first :: _))
  }

  /*
   * P27 (**) Group the elements of a set into disjoint subsets.
   * a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
   * Example:
   *
   * scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
   * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
   */
  def group3[T](items : List[T]) = group(List(2, 3, 4), items)

  /**
   * b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
   *
   * Example:
   *
   * scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
   * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
   * Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
   *
   * You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
   */
  def group[T](sizes : List[Int], ts : List[T]) : List[List[List[T]]] = sizes match {
    case Nil => List(List())
    case (pSize :: pSizes) => {
      // partitionsOfSize returns a list of all possible partitions of the items
      // into (List(included), List(excluded)), where exactly n items are included
      def partitionsOfSize(n : Int, items : List[T]) : List[(List[T], List[T])] =
        (n, items) match {
          case (0, _) => List((List() , items)) // one pair with nothing in, everything out
          case (_, Nil) => List() // no pairs
          case (k, item :: others) =>
            partitionsOfSize(k-1, others).map(p => (item :: p._1, p._2)) :::   // including the first item
            partitionsOfSize(k, others).map(p => (p._1, item :: p._2))     // not including the first item
        }
      partitionsOfSize(pSize, ts).flatMap(p => group(pSizes, p._2).map(ps => p._1 :: ps))
    }
  }

  /**
   * P28 (**) Sorting a list of lists according to length of sublists.
   * a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
   * Example:
   *
   * scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
   * res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
   */
  def lsort[T](lots : List[List[T]]) : List[List[T]] = lots.sortBy(_.length)

  /**
   * b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
   *
   * Example:
   *
   * scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
   * res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
   * Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
   */
  def lsortFreq[T](lots : List[List[T]]) : List[List[T]] = {
    val freq = lots.groupBy(_.length).mapValues(_.length)
    lots.sortBy(ts => freq(ts.length))
  }
}
