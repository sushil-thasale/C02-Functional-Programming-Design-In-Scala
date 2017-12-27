package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(genHeap, Gen.const(empty))
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  /**
    * covert Heap to List[Int]
    */
  def heapToList(h: H) : List[Int] = {
    if (h == empty) Nil
    else findMin(h) :: heapToList(deleteMin(h))
  }

  /**
    * check if List[Int] is sorted
    */
  def isSorted(list: List[Int]): Boolean = {
    if (list.length <= 1) true
    else !list.zip(list.tail).exists{ case(x, y) => x > y}
  }

  /**
    * Heap properties
    */

  /**
    * single element in he heap is always the smallest element
    */
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should get
    * the smallest of the two elements back.
    */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == Math.min(a, b)
  }

  /**
    * If you insert an element into an empty heap,
    * then delete the minimum, the resulting heap should be empty.
    */
  property("empty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  /**
    * Given any heap, you should get a sorted sequence of
    * elements when continually finding and deleting minima.
    */
  property("is sorted") = forAll { h: H =>
    isSorted(heapToList(h))
  }

  /**
    * Finding a minimum of the melding of any two heaps
    * should return a minimum of one or the other.
    */
  property("meld - 1") = forAll { (h1: H, h2: H) =>
    val a1 = findMin(h1)
    val a2 = findMin(h2)
    findMin(meld(h1, h2)) == math.min(a1, a2)
  }

  /**
    * After inserting an element greater than the min element
    * the findMin should return the min element
    */
  property("delete min - 1") = forAll { (a: Int) =>
    val b = if (a >= Int.MaxValue-1) a-2 else a
    val h1 = deleteMin(insert(b+2, insert(b+1, insert(b, empty))))
    val h2 = insert(b+1, insert(b+2, empty))
    h1 == h2
  }

  /**
    * inserting same element twice
    * and deleting it should result in empty heap
    */
  property("delete min - 2") = forAll { (a: Int) =>
    val h1 = deleteMin(insert(a, insert(a, empty)))
    !isEmpty(h1)
  }
}