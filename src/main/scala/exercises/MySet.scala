package exercises

import lectures.part1.Recap.theUnit
import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {
  def apply(elem: A): Boolean = contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def -(elem: A): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A=> MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {
  override def unary_! : MySet[A] = new ProperyBasedSet[A](_ => true)

  override def &(anotherSet: MySet[A]): MySet[A] = ???

  override def -(elem: A): MySet[A] = ???

  override def --(anotherSet: MySet[A]): MySet[A] = ???

  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def map[B](f: A => B): MySet[B] = EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = EmptySet[B]

  override def foreach(f: A => Unit): Unit = ()
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {

  override def contains(elem: A): Boolean = head == elem || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if this.contains(elem) then this else new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  override def filter(predicate: A => Boolean): MySet[A] =
    val filteredTail = tail filter predicate
    if predicate(head) then filteredTail + head else filteredTail

  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def foreach(f: A => Unit): Unit =
    f(head)
    tail foreach f

  def -(elem: A): MySet[A] =
    if head == elem then tail
    else (tail - elem) + head

  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  // new operator
  override def unary_! : MySet[A] = new ProperyBasedSet[A](x => !this.contains(x))
}

class AllInclusiveSet[A] extends MySet[A] {

  override def flatMap[B](f: A => MySet[B]): MySet[B] = ???

  override def unary_! : MySet[A] = new EmptySet[A]

  override def filter(predicate: A => Boolean): MySet[A] = ???

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def -(elem: A): MySet[A] = ???

  override def +(elem: A): MySet[A] = this

  override def contains(elem: A): Boolean = true

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def ++(anotherSet: MySet[A]): MySet[A] = this

  override def map[B](f: A => B): MySet[B] = ???

  override def foreach(f: A => Unit): Unit = ???
}

// all elements of type A which satisfy a property
// { x in A | property(X) }
class ProperyBasedSet[A](property: A => Boolean) extends MySet[A] {

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def unary_! : MySet[A] = new ProperyBasedSet[A](x => !property(x))

  override def filter(predicate: A => Boolean): MySet[A] =
    new ProperyBasedSet[A](x => property(x) && predicate(x))

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def -(elem: A): MySet[A] = filter(x => x != elem)

  override def +(elem: A): MySet[A] =
    new ProperyBasedSet[A](x => property(x) || x == elem)

  override def contains(elem: A): Boolean = property(elem)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    new ProperyBasedSet[A](x => property(x) || anotherSet(x))

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def foreach(f: A => Unit): Unit = politelyFail

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if valSeq.isEmpty then acc else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val aSet = MySet(1, 2, 3, 4)

  aSet.foreach(println)

  val negative = !aSet // all integers not in (1, 2, 3, 4)
  println(negative(1))
  println(negative(2))
  println(negative(3))
  println(negative(4))
  println(negative(5))
  println(negative(-1))
  println(negative(6))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEvenOr5 = negativeEven + 5
  println(negativeEvenOr5(5))
}
