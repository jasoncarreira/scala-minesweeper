package jc.types
import org.scalatest._
import scala.collection.immutable.WrappedString

class TernaryTrieSpec extends FlatSpec with Matchers {
  "TernaryTrie" should "create a Trie for a String" in {
    val root1: TernaryTrie[String] = TernaryTrie("foo", "bar")
    val root2 = root1.insert("food", "bard")
    root1.keys shouldEqual List("foo")
    root2.keys shouldEqual List("foo", "food")
    root2.keysWithPrefix("foo") shouldEqual List("foo", "food")
    root2.search("foo") shouldEqual Some("bar")
    root2.search("food") shouldEqual Some("bard")
  }
}
