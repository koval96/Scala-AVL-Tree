package tree
import scala.collection.immutable.Queue
import scala.util.control.TailCalls._

case class Node private (value: Int, left: Option[Node], right: Option[Node], height: Int) {
  def copy(value: Int = value, left: Option[Node] = left, right: Option[Node] = right): Node = {
    Node(value, left, right)
  }
}
object Node {
  def apply(value: Int, left: Option[Node], right: Option[Node]): Node = {
    val zeroNode = Some(new Node(0, None, None, 0))
    val height = for {
      leftNode <- left.orElse(zeroNode)
      rightNode <- right.orElse(zeroNode)
    } yield 1 + leftNode.height.max(rightNode.height)

    new Node(value, left, right, height.getOrElse(0))
  }
}

sealed trait Search {
  def search(tree: Tree): List[Int]
}

trait Tree {
  val root: Node
  def add(value: Int): Tree
  def delete(value: Int): Tree
  def foldLeft(zero: Int)(fn: (Int, Int) => Int, searcher: Search): Int
  def breadthFirstSearch(zero: Int)(fn: (Int, Int) => Int): Int
  def depthFirstSearch(zero: Int)(fn: (Int, Int) => Int): Int
  def max(searcher: Search): Int
  def min(searcher: Search): Int
  def size(): Int
  def print(): List[Int]
}

object DepthFirstSearch extends Search {
  def search(tree: Tree): List[Int] = {
    def recDfs(nodeOpt: Option[Node], acc: List[Int]): TailRec[List[Int]] = {
      nodeOpt match {
        case Some(node) =>
          for {
            left <- tailcall(recDfs(node.left, node.value :: acc))
            right <- tailcall(recDfs(node.right, left))
          } yield right
        case None => done(acc)
      }
    }

    recDfs(Some(tree.root), List()).result.reverse
  }
}

object BreadthFirstSearch extends Search {

  def search(tree: Tree): List[Int] = {
    def getChildren(node: Node): List[Node] = {
      node.left.toList ++ node.right.toList
    }

    def recBfs(q: Queue[Node], acc: List[Int]): TailRec[List[Int]] = {
      if (q.isEmpty) {
        done(acc)
      } else {
        val (node, tail) = q.dequeue
        tailcall(recBfs(tail ++ getChildren(node), node.value :: acc))
      }
    }

    recBfs(Queue(tree.root), List()).result.reverse
  }
}

case class AVLTree(root: Node) extends Tree { self =>

  private def leftRotation(node: Node): Node = {
    val right = node.right
    right match {
      case Some(rightNode) =>
        Node(rightNode.value, Some(Node(node.value, node.left, rightNode.right)), rightNode.right)
      case _ => node
    }
  }
  private def rightRotation(node: Node): Node = {
    val left = node.left
    left match {
      case Some(leftNode) =>
        Node(leftNode.value, leftNode.left, Some(Node(node.value, leftNode.right, node.right)))
      case _ => node
    }
  }

  private def getBalance(node: Node): Int = {
    val zeroNode = Node(0, None, None, 0)
    node.left.getOrElse(zeroNode).height - node.right.getOrElse(zeroNode).height
  }

  def add(value: Int): AVLTree = {
    def recAdd(nodeOption: Option[Node]): Node = {

      val addedRaw = nodeOption match {
        case Some(node) =>
          if (value < node.value)
            node.copy(left = Some(recAdd(node.left)))
          else if (value > node.value)
            node.copy(right = Some(recAdd(node.right)))
          else
            node
        case _ => Node(value, None, None)
      }

      val balance = getBalance(addedRaw)

      if (balance > 1)
        addedRaw.left
          .map(leftNode =>
            if (value < leftNode.value) rightRotation(addedRaw)
            else rightRotation(addedRaw.copy(left = Some(leftRotation(leftNode))))
          )
          .getOrElse(addedRaw)
      else if (balance < -1)
        addedRaw.right
          .map(rightNode =>
            if (value > rightNode.value) leftRotation(addedRaw)
            else rightRotation(addedRaw.copy(right = Some(rightRotation(rightNode))))
          )
          .getOrElse(addedRaw)
      else
        addedRaw

    }

    AVLTree(recAdd(Some(root)))
  }

  def getMin(node: Node): Node = {
    node.left match {
      case Some(left) => getMin(left)
      case None       => node
    }
  }

  def delete(value: Int): AVLTree = {
    if (root.left.isEmpty && root.right.isEmpty) {
      AVLTree(root)
    } else {
      def recDelete(nodeOption: Option[Node]): Option[Node] = {
        val deletedRawOption: Option[Node] = nodeOption match {
          case Some(node) =>
            if (value < node.value) {
              Some(node.copy(left = recDelete(node.left)))
            } else if (value > node.value) {
              Some(node.copy(right = recDelete(node.right)))
            } else {
              (node.left, node.right) match {
                case (Some(_), Some(right)) =>
                  val min = getMin(right)
                  Some(
                    node.copy(
                      value = min.value,
                      right =
                        if (right.right.isDefined || right.left.isDefined)
                          Some(AVLTree(right).delete(min.value).root)
                        else None
                    )
                  )
                case (Some(left), None)  => Some(left)
                case (None, Some(right)) => Some(right)
                case _                   => None
              }
            }
          case _ => None
        }

        deletedRawOption match {
          case Some(deletedRaw) =>
            val balance = getBalance(deletedRaw)

            if (balance > 1)
              deletedRaw.left
                .map(leftNode =>
                  if (getBalance(leftNode) >= 0) rightRotation(deletedRaw)
                  else rightRotation(deletedRaw.copy(left = Some(leftRotation(leftNode))))
                )
            else if (balance < -1)
              deletedRaw.right
                .map(rightNode =>
                  if (getBalance(rightNode) <= 0) leftRotation(deletedRaw)
                  else rightRotation(deletedRaw.copy(right = Some(rightRotation(rightNode))))
                )
            else
              Some(deletedRaw)

          case _ => None
        }
      }

      AVLTree(recDelete(Some(root)).getOrElse(root))
    }
  }

  def foldLeft(zero: Int)(fn: (Int, Int) => Int, searcher: Search = DepthFirstSearch): Int = {
    searcher.search(self).fold(zero)(fn)
  }

  def breadthFirstSearch(zero: Int)(fn: (Int, Int) => Int): Int = foldLeft(zero)(fn, BreadthFirstSearch)

  def depthFirstSearch(zero: Int)(fn: (Int, Int) => Int): Int = foldLeft(zero)(fn, DepthFirstSearch)

  def max(searcher: Search): Int = {
    foldLeft(Int.MinValue)(
      (acc, value) => {
        if (acc < value) {
          value
        } else {
          acc
        }
      },
      searcher
    )
  }

  def min(searcher: Search): Int = {
    foldLeft(Int.MaxValue)(
      (acc, value) => {
        if (acc > value) {
          value
        } else {
          acc
        }
      },
      searcher
    )
  }

  def size(): Int = {
    BreadthFirstSearch.search(self).length
  }

  def compare(tree: Tree): Boolean = {
    BreadthFirstSearch.search(self) == BreadthFirstSearch.search(tree)
  }

  def print(): List[Int] = {
    println(root.value)
    root.value :: BreadthFirstSearch
      .search(self)
      .tail
      .scanLeft(Int.MinValue)((acc, value) => {
        if (acc > value) {
          println()
        }
        scala.Predef.print(s"${value} ")
        value
      })
  }
}
