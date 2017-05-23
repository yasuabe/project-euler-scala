package util

sealed trait PairingHeapSet[+A] {
  import PairingHeapSet._
  def findMin: Option[A] = this match {
    case Empty => None
    case Heap(root, _) => Some(root)
  }
  def merge[B >: A](p2: PairingHeapSet[B])(implicit ord: Ordering[B]): PairingHeapSet[B] = (this, p2) match {
    case (_, Empty) => this
    case (Empty, _) => p2
    case (Heap(e1, sh1), Heap(e2, sh2)) => {
      import ord.mkOrderingOps
      if (e1 < e2) Heap(e1, p2::sh1)
      else if (e1 `equiv` e2) mergePairs[B](p2::sh1)
      else Heap(e2, this::sh2)
    }
  }
  def insert[B >: A](elem: B)(implicit ord: Ordering[B]): PairingHeapSet[B] = merge(Heap(elem, Nil))
  def deleteMin[B >: A](implicit ord: Ordering[B]): PairingHeapSet[B] = this match {
    case Empty => throw new Exception()
    case Heap(_, subHeaps) => mergePairs[B](subHeaps)
  }
  def pop[B >: A](implicit ord: Ordering[B]): (B, PairingHeapSet[B]) = this match {
    case Empty => throw new Exception()
    case Heap(elem, subHeaps) => (elem, mergePairs[B](subHeaps))
  }
}
object PairingHeapSet {
  case object Empty extends PairingHeapSet[Nothing]
  case class Heap[+A](elem: A, subHeaps: List[PairingHeapSet[A]]) extends PairingHeapSet[A]

  //  def apply[A](e: A): PairingHeap[A] = Heap(e, Nil)
//  def findMin[A](p: PairingHeap[A]): Option[A] = p.findMin
//  def merge[A](p1: PairingHeap[A], p2: PairingHeap[A])(implicit ord: Ordering[A]): PairingHeap[A] = p1.merge(p2)
//  def insert[A](elem: A, heap: PairingHeap[A])(implicit ord: Ordering[A]): PairingHeap[A] = heap.merge(Heap(elem, Nil))
//  def deleteMin[A](heap: PairingHeap[A])(implicit ord: Ordering[A]): PairingHeap[A] = heap.deleteMin
//  def pop[A](heap: PairingHeap[A])(implicit ord: Ordering[A]): (A, PairingHeap[A]) = heap.pop
  def empty[A]: PairingHeapSet[A] = Empty
  def mergePairs[A](ls: List[PairingHeapSet[A]])
                   (implicit ord: Ordering[A]): PairingHeapSet[A] = ls match {
    case Nil => Empty
    case (h::Nil) => h
    case (h0::h1::rest) => h0.merge(h1).merge(mergePairs(rest))
  }
}
