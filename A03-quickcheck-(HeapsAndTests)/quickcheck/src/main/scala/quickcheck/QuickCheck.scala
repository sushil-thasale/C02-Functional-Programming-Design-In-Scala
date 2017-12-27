package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(genHeap, Gen.const(empty))
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def heapToList(h: H) : List[Int] =
    if(isEmpty(h)) Nil else findMin(h) :: heapToList(deleteMin(h))

  def isSorted(list: List[Int]): Boolean = {
    !list.zip(list.tail).exists{ case(x, y) => x > y}
  }

//  test cases

//  check if single element is always the min element
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

//  for any heap, adding the minimal element, and then finding it,
//  should return the element in question
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

//  If you insert any two elements into an empty heap,
//   finding the minimum of the resulting heap should get
//   the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == Math.min(a, b)
  }

//  If you insert an element into an empty heap,
//   then delete the minimum, the resulting heap should be empty.
  property("empty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

//  Given any heap, you should get a sorted sequence of
//   elements when continually finding and deleting minima.
  property("is sorted") = forAll { (h: H) =>
//    heapToList(h) == heapToList(h).sorted
    isSorted(heapToList(h))
  }

//  Finding a minimum of the melding of any two heaps should
//   return a minimum of one or the other
  property("meld1") = forAll { (h1: H, h2: H, h3: H) =>
    val a1 = findMin(h1)
    val a2 = findMin(h2)
    findMin(meld(h1, h2)) == math.min(a1, a2)
  }

}
