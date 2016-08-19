package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

//  trait Heap {
//    type H // type of a heap
//    type A // type of an element
//    def ord: Ordering[A] // ordering on elements
//
//    def empty: H // the empty heap
//    def isEmpty(h: H): Boolean // whether the given heap h is empty
//
//    def insert(x: A, h: H): H // the heap resulting from inserting x into h
//    def meld(h1: H, h2: H): H // the heap resulting from merging h1 and h2
//
//    def findMin(h: H): A // a minimum of the heap h
//    def deleteMin(h: H): H // a heap resulting from deleting a minimum of h
//  }

  lazy val genHeap: Gen[H] = Gen.frequency((1, genEmptyHeap), (3, genNonEmptyHeap))

  val genEmptyHeap: Gen[H] = Gen.const(empty)
  lazy val genNonEmptyHeap: Gen[H] = for {
    x <- Arbitrary.arbitrary[Int]
    h <- Gen.oneOf(genEmptyHeap, genNonEmptyHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // Irrelevant
  property("empty") = forAll { (a: Int) =>
    !isEmpty(insert(a, empty))
  }

  // Shows that Bogus 1 (One) and Bogus 2 (Two) are buggy
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // Irrelevant
  property("insert two mins") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty)) // TODO: suchThat or implication
    val m = findMin(h)
    if (a > b) m == b else m == a
  }

  // Irrelevant
  property("insert and delete") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }


  def isOrdered(h: H): Boolean = {
    def isOrderedHelper(x: Int, h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        x <= m && isOrderedHelper(m, deleteMin(h))
      }
    if(isEmpty(h)) true else isOrderedHelper(findMin(h), deleteMin(h))
  }

  // This shows that Bogus 5 (Five) is buggy
  property("heap sort") = forAll { (h: H) =>
    isOrdered(h)
  }

  // Irrelevant
  property("minimum of the meld") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) 0 else findMin(h1) // TODO: suchThat or implication
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
    val m = findMin(meld(insert(m1, h1), insert(m2, h2)))
    m == m1 || m == m2
  }

  // Shows that Bogus 3 (three) is buggy
  property("insert two mins and then delete") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val fstMin = findMin(h)
    val sndMin = findMin(deleteMin(h))
    if (a > b) (fstMin == b) && (sndMin == a) else (fstMin == a) && (sndMin == b)
  }

  // Irrelevant
  property("insert twice the same value") = forAll { (a: Int) =>
    val h = insert(a, insert(a, empty))
    val fstMin = findMin(h)
    val sndMin = findMin(deleteMin(h))
    (a == fstMin) && (a == sndMin) && isEmpty(deleteMin(deleteMin(h)))
  }

  // This shows that Bogus 4 (Four) is buggy
  property("skinnycatting") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(a, insert(b, empty)))
    //val fstMin = findMin(h)
    val sndMin = findMin(deleteMin(h))
    //val trdMin = findMin(deleteMin(deleteMin(h)))
    (a == sndMin)
    //if (a > b)
    //  (b == fstMin) &&  (a == trdMin)
    //else
    //  (a == fstMin) && (b == trdMin)
    //)
  }
}
