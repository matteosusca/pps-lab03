package eslab03

import eslab03.Task2.*
import org.junit.*
import org.junit.Assert.*
import u02.Modules.Person.*
import u03.Sequences.Sequence.*
import u03.Sequences.*

class Task2Test:

  @Test def testGetCourses() =
    val people = Cons(Teacher("John", "Math"), Cons(Student("Alice", 2021), Cons(Teacher("Bob", "Physics"), Nil())))
    val courses = getCourses(people)
    val expectedCourses = Cons("Math", Cons("Physics", Nil()))
    assertEquals(expectedCourses, courses)

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5 , Nil ()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
