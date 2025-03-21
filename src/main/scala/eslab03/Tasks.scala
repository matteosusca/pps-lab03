package eslab03
import u03.Sequences.*
import u03.Sequences.Sequence.*

object Task1 extends App:
  import u03.Optionals.*
  import u03.Optionals.Optional.*

  /*
       * Skip the first n elements of the sequence
       * E.g., [10, 20, 30], 2 => [30]
       * E.g., [10, 20, 30], 3 => []
       * E.g., [10, 20, 30], 0 => [10, 20, 30]
       * E.g., [], 2 => []
       */
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] =
    s match
      case Cons(_, t) if n > 0 => skip(t)(n - 1)
      case _ => s

  /*
   * Zip two sequences
   * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
   * E.g., [10], [] => []
   * E.g., [], [] => []
   */
  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
    (first, second) match
      case (Cons(h, t), Cons(h2, t2)) => Cons((h, h2), zip(t, t2))
      case _ => Nil()

  /*
   * Concatenate two sequences
   * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
   * E.g., [10], [] => [10]
   * E.g., [], [] => []
   */
  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] =
    s1 match
      case Cons(h, t) => Cons(h, concat(t, s2))
      case _ => s2

  /*
   * Reverse the sequence
   * E.g., [10, 20, 30] => [30, 20, 10]
   * E.g., [10] => [10]
   * E.g., [] => []
   */
  def reverse[A](s: Sequence[A]): Sequence[A] =
    def accumulator(s2: Sequence[A], acc: Sequence[A]): Sequence[A] = s2 match
      case Cons(h, t) => accumulator(t, Cons(h, acc))
      case _ => acc

    accumulator(s, Nil())

  /*
   * Map the elements of the sequence to a new sequence and flatten the result
   * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
   * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
   * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
   */
  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
    s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()


  /*
   * Get the minimum element in the sequence
   * E.g., [30, 20, 10] => 10
   * E.g., [10, 1, 30] => 1
   */
  def min(s: Sequence[Int]): Optional[Int] = s match
    case Cons(h, t) => min(t) match
      case Just(n) if n < h => Just(n)
      case _ => Just(h)
    case _ => Empty()

  /*
   * Get the elements at even indices
   * E.g., [10, 20, 30] => [10, 30]
   * E.g., [10, 20, 30, 40] => [10, 30]
   */
  def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => Cons(h, evenIndices(skip(t)(1)))
    case _ => Nil()

  /*
   * Check if the sequence contains the element
   * E.g., [10, 20, 30] => true if elem is 20
   * E.g., [10, 20, 30] => false if elem is 40
   */
  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Cons(h, t) => h == elem || contains(t)(elem)
    case _ => false

  /*
   * Remove duplicates from the sequence
   * E.g., [10, 20, 10, 30] => [10, 20, 30]
   * E.g., [10, 20, 30] => [10, 20, 30]
   */
  def distinct[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => Cons(h, distinct(filter(t)(_ != h)))
    case _ => Nil()

  /*
   * Group contiguous elements in the sequence
   * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
   * E.g., [10, 20, 30] => [[10], [20], [30]]
   * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
   */
  def group[A](s: Sequence[A]): Sequence[Sequence[A]] = ???


  /*
   * Partition the sequence into two sequences based on the predicate
   * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
   * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
   */
  def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
    (filter(s)(pred), filter(s)(!pred(_)))

object Task2 extends App:
  import Task1.*
  import u02.Modules.Person
  import u02.Modules.Person.*

  def getCourses(people: Sequence[Person]): Sequence[String] =
    flatMap(people)(p =>
      p match
        case Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
    )

  def foldLeft[A, B](seq: Sequence[A])(acc: B)(op: (B,A) => B): B = seq match
    case Cons(h, t) => foldLeft(t)(op(acc,h))(op)
    case _ => acc

  def countCourses(seq: Sequence[Person]): Int =
    foldLeft(map(getCourses(seq))(v => 1))(0)(_ + _)

// Task 3
object Task3 extends App:
  import u03.Streams.Stream
  import u03.Sequences.*
  import Sequence.*

  def fill[A](n: Int)(k: A): Sequence[A] = n match
    case n if n > 0 => Cons(k, fill(n - 1)(k))
    case _ => Nil()

  // Take while was already implemented in u03.Streams.Stream

  def getFibonacci(): Stream[Int] = {
    def fibFrom(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, fibFrom(b, a + b))

    fibFrom(0, 1)
  }