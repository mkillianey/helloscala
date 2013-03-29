package ninetynine

import annotation.tailrec

class S99Bool(val b : Boolean) {
  import S99Logic._
  /*
   * P46 (**) Truth tables for logical expressions.
   * Define functions and, or, nand, nor, xor, impl, and equ
   * (for logical equivalence) which return true or false
   * according to the result of their respective operations;
   * e.g. and(A, B) is true if and only if both A and B are true.
   * scala> and(true, true)
   * res0: Boolean = true
   *
   * scala> xor(true. true)
   * res1: Boolean = false
   * A logical expression in two variables can then be written
   * as an function of two variables, e.g:
   *   (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))
   *
   * Now, write a function called table2 which prints the truth
   * table of a given logical expression in two variables.
   *
   * scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
   * A     B     result
   * true  true  true
   * true  false true
   * false true  false
   * false false false
   */
  def and(that : => S99Bool) : Boolean = b && that.b
  def or(that : => S99Bool) : Boolean = b || that.b
  def nand(that : =>S99Bool) : Boolean = !(this and that)
  def nor(that : =>S99Bool) : Boolean = !(this or that)
  def xor(that : =>S99Bool) : Boolean = this.b != that.b
  def impl(that : =>S99Bool) : Boolean = (!this) or that
  def equ(that : =>S99Bool) : Boolean = this.b == that.b
}

object S99Logic {
  /*
   * P47 (*) Truth tables for logical expressions (2).
   * Continue problem P46 by redefining and, or, etc as operators. (i.e. make them methods of a new class with an implicit conversion from Boolean.) not will have to be left as a object method.
   * scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
   * A     B     result
   * true  true  true
   * true  false true
   * false true  false
   * false false false
   */
  implicit def boolean_to_S99Bool(b: Boolean): S99Bool = new S99Bool(b)
  implicit def s99Bool_to_Boolean(s: S99Bool): Boolean = s.b

  def not(x : Boolean) : Boolean = !x
  def and(x : Boolean, y : => Boolean) : Boolean = x and y
  def or(x : Boolean, y : => Boolean) : Boolean = x or y
  def nand(x : Boolean, y : => Boolean) : Boolean = x nand y
  def nor(x : Boolean, y : => Boolean) : Boolean = x nor y
  def xor(x : Boolean, y : => Boolean) : Boolean = x xor y
  def impl(x : Boolean, y : => Boolean) : Boolean = x impl  y
  def equ(x : Boolean, y : => Boolean) : Boolean = x equ y

  def table2(f : (Boolean, Boolean) => Boolean) {
    Console.out.println( "A      B      result")
    Console.out.println(s"true  true    ${f(true, true)}")
    Console.out.println(s"true  false   ${f(true, false)}")
    Console.out.println(s"false true    ${f(false, true)}")
    Console.out.println(s"false true    ${f(false, false)}")
  }

  /*
   *P48 (**) Truth tables for logical expressions (3).
   * Omitted for now.
   */

  /*
   * P49 (**) Gray code.
   * An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
   * n = 1: C(1) = ("0", "1").
   * n = 2: C(2) = ("00", "01", "11", "10").
   * n = 3: C(3) = ("000", "001", "011", "010", "110", "111", "101", "100").
   * Find out the construction rules and write a function to generate Gray codes.
   *
   * scala> gray(3)
   * res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
   * See if you can use memoization to make the function more efficient.
   */
  def gray(n : Int) : List[String] =
    if (n == 0) List("")
    else {
      val g = gray(n-1)
      g.map("0" + _) ::: g.reverse.map("1" + _)
    }

  /* P50 (***) Huffman code.
   * First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes!
   * We suppose a set of symbols with their frequencies, given as a list of (S, F) Tuples. E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)). Our objective is to construct a list of (S, C) Tuples, where C is the Huffman code word for the symbol S.
   *
   * scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
   * res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))
   */
  def huffman(freq : List[(Char, Int)]) : List[(Char, List[Int])] = {
    abstract class Node { val size: Int }
    case class Leaf(ch : Char, size : Int) extends Node
    case class Branch(left : Node, right : Node) extends Node {
      val size = left.size + right.size
    }
    @tailrec
    def makeTree(ns : List[Node]) : Node = ns.sortBy(_.size) match {
      case first :: Nil => first
      case n1 :: n2 :: rest => makeTree((Branch(n1, n2) :: rest))
    }
    def genCodes(n : Node, prefix : List[Int]) : List[(Char, List[Int])] = n match {
      case Leaf(ch, size) => List((ch, prefix.reverse))
      case Branch(left, right) => genCodes(left, 0 :: prefix) ::: genCodes(right, 1 :: prefix)
    }
    genCodes(makeTree(freq.map(p => Leaf(p._1, p._2))), List()).sortBy(_._1)
  }
}
