package jc.types

sealed trait TernaryTrie[+A] {
  def insert[AA >: A](key: String, value: AA): TernaryTrie[AA] = {
    TernaryTrie.insert(this, key, value)
  }
  def search(key: String): Option[A] =
    TernaryTrie.search(this, key, 0)

  def keys: List[String] = TernaryTrie.keys(this)

  def keysWithPrefix(prefix: String): List[String] =
    TernaryTrie.keysWithPrefix(this, prefix)
}

case class Node[+A](key: Char,
                    value: Option[A] = None,
                    left: TernaryTrie[A] = Leaf,
                    eq: TernaryTrie[A] = Leaf,
                    right: TernaryTrie[A] = Leaf)
    extends TernaryTrie[A]

case object Leaf extends TernaryTrie[Nothing]

object TernaryTrie {
  def apply[A](key: String, value: A): TernaryTrie[A] = {
    insert(Leaf, key, value)
  }

  def apply[A]: TernaryTrie[A] = Leaf

  private def keys[A](root: TernaryTrie[A]): List[String] = collect(root, "")

  private def keysWithPrefix[A](root: TernaryTrie[A],
                                prefix: String): List[String] =
    get(root, prefix, 0) match {
      case None => Nil
      case Some(node) =>
        collect(node, prefix.dropRight(1))
    }

  private def collect[A](node: TernaryTrie[A], prefix: String): List[String] =
    node match {
      case Leaf => Nil
      case node: Node[A] if node.value.isDefined =>
        (prefix :+ node.key) +:
          (collect(node.left, prefix) ++
          collect(node.eq, prefix :+ node.key) ++
          collect(node.right, prefix))
      case node: Node[A] =>
        collect(node.left, prefix) ++
          collect(node.eq, prefix :+ node.key) ++
          collect(node.right, prefix)
    }

  @scala.annotation.tailrec
  private def get[A](root: TernaryTrie[A],
                     prefix: String,
                     step: Int): Option[TernaryTrie[A]] = root match {
    case Leaf => None
    case node: Node[A] if prefix(step) < node.key =>
      get(node.left, prefix, step)
    case node: Node[A] if prefix(step) > node.key =>
      get(node.right, prefix, step)
    case node: Node[A] if step < prefix.length - 1 =>
      get(node.eq, prefix, step + 1)
    case node: Node[A] => Some(node)
  }

  @scala.annotation.tailrec
  private def search[A](root: TernaryTrie[A],
                        key: String,
                        step: Int): Option[A] = root match {
    case Leaf                                  => None
    case node: Node[A] if key(step) < node.key => search(node.left, key, step)
    case node: Node[A] if key(step) > node.key => search(node.right, key, step)
    case node: Node[A] if step < key.length - 1 =>
      search(node.eq, key, step + 1)
    case node: Node[A] => node.value
  }

  private def insert[A](root: TernaryTrie[A],
                        key: String,
                        value: A): TernaryTrie[A] = {
    key match {
      case s if s.isEmpty =>
        root match {
          case Leaf       => Leaf
          case n: Node[A] => n.copy(value = Some(value))
        }
      case s =>
        val current = s.head
        root match {
          case Leaf => insert(Node(key.head), key, value)
          case n @ Node(k, _, left, _, _) if k < current =>
            n.copy(left = insert(left, key, value))
          case n @ Node(k, _, _, _, right) if k > current =>
            n.copy(right = insert(right, key, value))
          case n: Node[A] if s.length == 1 => n.copy(value = Some(value))
          case n: Node[A]                  => n.copy(eq = insert(n.eq, s.tail, value))
        }
    }
  }

}
