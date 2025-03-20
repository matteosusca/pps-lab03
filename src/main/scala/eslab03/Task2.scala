package eslab03

import u02.Modules.Person
import u02.Modules.Person.*
import u03.Sequences
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

object Task2 extends App:
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
