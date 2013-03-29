package ninetynine

import org.scalatest.Assertions
import org.junit.{Ignore, Test}
import ninetynine.binarytree._

class BinaryTreesTests extends Assertions {

  @Test
  def P55_cBalanced() {
    val fours = Tree.cBalanced(5, "x")
    Console.out.println(s"Fours: ${fours.length}")
    fours.foreach(Console.out.println(_))

    val fives = Tree.cBalanced(5, "x")
    Console.out.println(s"Fives: ${fives.length}")
    fives.foreach(Console.out.println(_))

    val sixes = Tree.cBalanced(6, "x")
    Console.out.println(s"Sixes: ${sixes.length}")
    sixes.foreach(Console.out.println(_))

    Console.out.flush()
  }


}
