package eslab03

import eslab03.Task2.*
import eslab03.Task3.*
import org.junit.*
import org.junit.Assert.*
import u02.Modules.Person.*
import u03.Sequences.Sequence.*
import u03.Sequences.*
import u03.Streams.Stream

class Task2Test:

  @Test def testGetCourses() =
    val people = Cons(Teacher("John", "Math"), Cons(Student("Alice", 2021), Cons(Teacher("Bob", "Physics"), Nil())))
    val courses = getCourses(people)
    val expectedCourses = Cons("Math", Cons("Physics", Nil()))
    assertEquals(expectedCourses, courses)

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5 , Nil ()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test def testCountCourses() =
    val people = Cons(Teacher("John", "Math"), Cons(Student("Alice", 2021),   Cons(Teacher("Bob", "Physics"), Cons(Student("Alice", 2021), Nil()))))
    val count = countCourses(people)
    val x = filter(people)(Teacher => true)
    assertEquals(2, count)

class Task3Test:

  @Test def testFill() =
    val filled = fill(5)("x")
    val expected = Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil())))))
    assertEquals(expected, filled)

  @Test def testGetFibonacci() =
    val fib = getFibonacci()
    val expected = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Cons(21, Cons(34, Nil()))))))))))
    assertEquals(expected, Stream.toList(Stream.take(fib)(10)))