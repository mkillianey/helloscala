package ninetynine

import org.scalatest.Assertions
import org.junit.{Ignore, Test}

class LogicAndCodesTests extends Assertions {

  import S99Logic._

  @Test
  def P46_and() {
    assert(and(true, true) === true)
    assert(and(true, false) === false)
    assert(and(false, true) === false)
    assert(and(false, false) === false)
  }

  @Test
  def P46_or() {
    assert(or(true, true) === true)
    assert(or(true, false) === true)
    assert(or(false, true) === true)
    assert(or(false, false) === false)
  }

  @Test
  def P46_nand() {
    assert(nand(true, true) === false)
    assert(nand(true, false) === true)
    assert(nand(false, true) === true)
    assert(nand(false, false) === true)
  }

  @Test
  def P46_nor() {
    assert(nor(true, true) === false)
    assert(nor(true, false) === false)
    assert(nor(false, true) === false)
    assert(nor(false, false) === true)
  }

  @Test
  def P46_xor() {
    assert(xor(true, true) === false)
    assert(xor(true, false) === true)
    assert(xor(false, true) === true)
    assert(xor(false, false) === false)
  }

  @Test
  def P46_impl() {
    assert(impl(true, true) === true)
    assert(impl(true, false) === false)
    assert(impl(false, true) === true)
    assert(impl(false, false) === true)
  }

  @Test
  def P46_equ() {
    assert(equ(true, true) === true)
    assert(equ(true, false) === false)
    assert(equ(false, true) === false)
    assert(equ(false, false) === true)
  }

  @Test
  def P47_and() {
    assert((true and true) === true)
    assert((true and false) === false)
    assert((false and true) === false)
    assert((false and false) === false)
  }

  @Test
  def P47_or() {
    assert((true or true) === true)
    assert((true or false) === true)
    assert((false or true) === true)
    assert((false or false) === false)
  }

  @Test
  def P47_nand() {
    assert((true nand true) === false)
    assert((true nand false) === true)
    assert((false nand true) === true)
    assert((false nand false) === true)
  }

  @Test
  def P47_nor() {
    assert((true nor true) === false)
    assert((true nor false) === false)
    assert((false nor true) === false)
    assert((false nor false) === true)
  }

  @Test
  def P47_xor() {
    assert((true xor true) === false)
    assert((true xor false) === true)
    assert((false xor true) === true)
    assert((false xor false) === false)
  }

  @Test
  def P47_impl() {
    assert((true impl true) === true)
    assert((true impl false) === false)
    assert((false impl true) === true)
    assert((false impl false) === true)
  }

  @Test
  def P47_equ() {
    assert((true equ true) === true)
    assert((true equ false) === false)
    assert((false equ true) === false)
    assert((false equ false) === true)
  }

  @Test
  def P49_gray() {
    assert(gray(1) === List("0", "1"))
    assert(gray(2) === List("00", "01", "11", "10"))
    assert(gray(3) === List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

  @Test
  def P50_huffman() {
    assert(huffman(List(('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5)))
      === List(('a', List(0)),
               ('b', List(1,0,1)),
               ('c', List(1,0,0)),
               ('d', List(1,1,1)),
               ('e', List(1,1,0,1)),
               ('f', List(1,1,0,0))))
  }
}
