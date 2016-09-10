package week5binarysearchtree

import java.util
import java.util.Scanner

/**
  * Created by davidtan on 9/10/16.
  */
object treeorders1 {

  var n: Int = 0
  var key: Array[Int] = null
  var left: Array[Int] = null
  var right: Array[Int] = null

  def read(): Unit = {
    val in = new Scanner(System.in)

    n = in.nextInt
    key = new Array[Int](n)
    left = new Array[Int](n)
    right = new Array[Int](n)

    for (i <- 0 until n) {
      key(i) = in.nextInt
      left(i) = in.nextInt
      right(i) = in.nextInt
    }
    //println(n, key.toList, left.toList, right.toList)
    //(5,List(4, 2, 5, 1, 3),List(1, 3, -1, -1, -1),List(2, 4, -1, -1, -1))
  }

  def inOrder(): util.ArrayList[Int] = {
    val result = new util.ArrayList[Int]
    val stack = new util.Stack[Int]
    pushAll(stack, 0)

    while (!stack.isEmpty) {
      var top: Int = stack.pop
      result.add(key(top))
      top = right(top)
      pushAll(stack, top)
    }

    result
  }

  def preOrder(): util.ArrayList[Int] = {
    val result = new util.ArrayList[Int]
    val stack = new util.Stack[Int]
    stack.push(0)

    while (!stack.isEmpty) {
      val top = stack.pop
      result.add(key(top))
      if (right(top) != -1) stack.push(right(top))
      if (left(top) != -1) stack.push(left(top))
    }

    result
  }

  def postOrder(): util.ArrayList[Int] = {
    val result = new util.ArrayList[Int]
    val stack = new util.Stack[Int]
    stack.push(0)
    while (!stack.isEmpty) {
      val top = stack.pop
      result.add(0, key(top))
      if (left(top) != -1) stack.push(left(top))
      if (right(top) != -1) stack.push(right(top))
    }
   result
  }

  /** Push all nodes on the path root->left-most into stack */
  private def pushAll(stack: util.Stack[Int], root: Int): Unit = {
    var _root = root
    while (_root != -1) {
      stack.push(_root)
      _root = left(_root)
    }
  }

  def prn(x: util.ArrayList[Int]): Unit = {
    import scala.collection.JavaConversions._
    for (a <- x) print(a + " ")
    println
  }


  def main(args: Array[String]) {
    read()
    prn(inOrder())
    prn(preOrder())
    prn(postOrder())
  }

}

/*
10 September 2016
Good job! (Max time used: 5.45/12.00, max memory used: 134062080/536870912.)
5

4 1 2

2 3 4

5 -1 -1

1 -1 -1

3 -1 -1


//////////
1 2 3 4 5
4 2 1 3 5
1 3 2 5 4



10

0 7 2

10 -1 -1

20 -1 6

30 8 9

40 3 -1

50 -1 -1

60 1 -1

70 5 4

80 -1 -1

90 -1 -1

///////////////////////
50 70 80 30 90 40 0 20 10 60
0 70 50 40 30 80 90 20 60 10
50 80 90 30 40 70 10 60 20 0

Input Format. The first line contains the number of vertices n. The vertices of the tree are numbered

from 0 to n − 1. Vertex 0 is the root.

The next n lines contain information about vertices 0, 1, ..., n−1 in order. Each of these lines contains

three integers

keyi, lefti and righti — keyi

is the key of the i-th vertex, lef ti

is the index of the left

child of the i-th vertex, and righti

is the index of the right child of the i-th vertex. If i doesn’t have

left or right child (or both), the corresponding lef ti or righti (or both) will be equal to −1.

 */