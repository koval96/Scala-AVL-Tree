package tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AVLTreeSpec extends AnyFlatSpec with Matchers {
  private val tree = AVLTree(Node(10, Some(Node(5, Some(Node(4, None, None)), None)), Some(Node(11, None, None))))
  private val extendedTree = AVLTree(
    Node(10, Some(Node(4, Some(Node(3, None, None)), Some(Node(5, None, None)))), Some(Node(11, None, None)))
  )

  private val cutTreeFirst = AVLTree(
    Node(4, Some(Node(3, None, Some(Node(5, None, None)))), Some(Node(11, None, None)))
  )
  private val cutTreeSecond = AVLTree(
    Node(10, Some(Node(4, None, Some(Node(5, None, None)))), Some(Node(11, None, None)))
  )

  "add" should "extend tree with value" in {
    tree.add(3).compare(extendedTree) shouldEqual true
  }

  "delete" should "cuts tree by value" in {
    extendedTree.delete(10).compare(cutTreeFirst) shouldEqual true
    extendedTree.delete(0).compare(extendedTree) shouldEqual true
    extendedTree.delete(3).compare(cutTreeSecond) shouldEqual true
  }

  "foldLeft" should "traverse tree and accumulate value" in {
    extendedTree.foldLeft(0)((acc, v) => acc + v) shouldEqual 33
  }

  "breadthFirstSearch" should "traverse tree and accumulate value" in {
    extendedTree.breadthFirstSearch(0)((acc, v) => acc + v) shouldEqual 33
  }

  "deepFirstSearch" should "traverse tree and accumulate value" in {
    extendedTree.depthFirstSearch(0)((acc, v) => acc + v) shouldEqual 33
  }

  "max" should "find max value in tree" in {
    extendedTree.max(DepthFirstSearch) shouldEqual 11
  }

  "min" should "find min value in tree" in {
    extendedTree.min(DepthFirstSearch) shouldEqual 3
  }

  "size" should "calculates tree size" in {
    extendedTree.size shouldEqual 5
  }
}
