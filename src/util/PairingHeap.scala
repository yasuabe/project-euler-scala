package util

sealed trait PairingHeap[+A]
case object Empty extends PairingHeap[Nothing]
case class Heap[+A](elem: A, subHeaps: List[PairingHeap[A]]) extends PairingHeap[A]

object PairingHeap {
  def findMin[A](p: PairingHeap[A]): Option[A] = p match {
    case Empty => None
    case Heap(root, _) => Some(root)
  }

  def merge[A](p1: PairingHeap[A], p2: PairingHeap[A])
              (implicit ord: Ordering[A]): PairingHeap[A] = (p1, p2) match {
    case (_, Empty) => p1
    case (Empty, _) => p2
    case (Heap(e1, sh1), Heap(e2, sh2)) => {
      import ord.mkOrderingOps
      if (e1 < e2) Heap(e1, p2::sh1) else Heap(e2, p1::sh2)
    }
  }

  def insert[A](elem: A, heap: PairingHeap[A])
               (implicit ord: Ordering[A]): PairingHeap[A] = merge(Heap(elem, Nil), heap)

  def deleteMin[A](heap: PairingHeap[A])
                  (implicit ord: Ordering[A]): PairingHeap[A] = heap match {
    case Empty => throw new Exception()
    case Heap(_, subHeaps) => mergePairs(subHeaps)
  }

  def mergePairs[A](ls: List[PairingHeap[A]])
                   (implicit ord: Ordering[A]): PairingHeap[A] = ls match {
    case Nil => Empty
    case (h::Nil) => h
    case (h0::h1::rest) => merge(merge(h0, h1), mergePairs(rest))
  }
}
