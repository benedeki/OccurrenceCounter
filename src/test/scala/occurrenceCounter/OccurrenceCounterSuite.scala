package occurrenceCounter

import org.scalatest.FunSuite

class OccurrenceCounterSuite extends FunSuite {
  test("Init state") {
    val oc = new OccurrenceCounter[Int]
    assert(oc.bottomWithoutCount(1).isEmpty)
    assert(oc.topWithCounts(2).isEmpty)
    assert(oc.bottom(3).isEmpty)
    assert(oc.top(4).isEmpty)
    assert(oc.itemCount == 0)
    assert(oc.itemKindCount == 0)
  }

  test("Add one item") {
    val oc = new OccurrenceCounter[String]
    oc.addItems(Seq("Hello"))
    val exp = Seq(("Hello", 1))
    val exp2 = Seq("Hello")
    assert(oc.bottomWithoutCount(1) == exp)
    assert(oc.topWithCounts(2) == exp)
    assert(oc.bottom(10) == exp2)
    assert(oc.top(100) == exp2)
    assert(oc.itemCount == 1)
    assert(oc.itemKindCount == 1)
  }

  test("Empty requests") {
    val oc = new OccurrenceCounter[Double]
    oc.addItems(Seq(3.14, 2.71, -1))
    oc.addItems(Seq())
    assert(oc.bottomWithoutCount(0).isEmpty)
    assert(oc.topWithCounts(0).isEmpty)
    assert(oc.bottom(0).isEmpty)
    assert(oc.top(0).isEmpty)
    assert(oc.bottomWithoutCount(-1).isEmpty)
    assert(oc.topWithCounts(-2).isEmpty)
    assert(oc.bottom(-3).isEmpty)
    assert(oc.top(-4).isEmpty)
    assert(oc.itemCount == 3)
    assert(oc.itemKindCount == 3)
  }

  test("Character occurrence testing") {
    val oc = new OccurrenceCounter[Char]
    oc.addItems("Lorem Ipsum")
    oc.addItems("If you end your training now - if you choose the quick and easy path as Vader did - you will become an agent of evil.")
    val expBottomWithCounts = Seq(('.', 1), ('v', 1))
    val expTopWithCounts = Seq((' ', 26), ('e', 10), ('o', 10))
    val expBottom = Seq('k', 'b', 'L', 'q',  'V', 'v', '.', 'w', 'p', 'g', '-', 'I')
    val expTop = Seq (' ', 'e', 'o', 'a', 'i', 'n', 'u')
    assert(oc.bottomWithoutCount(2) == expBottomWithCounts)
    assert(oc.topWithCounts(3) == expTopWithCounts)
    assert(oc.bottom(12) == expBottom)
    assert(oc.top(7) == expTop)
    assert(oc.itemCount == 128)
    assert(oc.itemKindCount == 29)
  }
}
