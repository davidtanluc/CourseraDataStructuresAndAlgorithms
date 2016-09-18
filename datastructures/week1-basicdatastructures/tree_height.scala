import scala.collection.mutable

/**
  * Created by davidtan on 7/19/16.
  */
object tree_height {

  def solution(n: Int, parent: Array[Int]): Int = {
    val heights = mutable.Map[Int, Int]()
    var maxHeight = 0
    for (vertex <- 0 until n) {
      var height:Int = 0
      var i = vertex //0 ,1,2,3,4

      //// bottom up approach //////
      while (i != -1) {
        height += 1
        i = parent(i) // Array(4, -1, 4, 1, 1);; 0->4
        if (heights.isDefinedAt(i)) {
          height += heights(i)
          i = - 1   //end
        }
      }//while
      maxHeight = maxHeight max height
      heights(vertex) = height
      //println(heights)

      /*
  Map(0 -> 3)
  Map(1 -> 1, 0 -> 3)
  Map(2 -> 3, 1 -> 1, 0 -> 3)
  Map(2 -> 3, 1 -> 1, 3 -> 2, 0 -> 3)
  Map(2 -> 3, 4 -> 2, 1 -> 1, 3 -> 2, 0 -> 3)
       */

    }
    maxHeight
  }
  def main(args: Array[String]) {
    val N = readLine.toInt
    val inputs =readLine.split(' ').map(_.toInt)
   println(solution(N, inputs))//solution(5, Array(4, -1, 4, 1, 1))//3

  }
}
/*
==============
Input:
==============
Explanation:

The input means that there are 5 nodes with numbers from 0 to 4, node 0 is a child of node 4, node 1

is the root, node 2 is a child of node 4, node 3 is a child of node 1 and node 4 is a child of node 1. To

see this, let us write numbers of nodes from 0 to 4 in one line and the numbers given in the input in

the second line underneath:

The height of this tree is 3, because the number of vertices on the path from root 1 to leaf 2 is 3.
0 1 2 3 4

4 -1 4 1 1

Now we can see that the node number 1 is the root, because −1 corresponds to it in the second line.

Also, we know that the nodes number 3 and number 4 are children of the root node 1. Also, we know

that the nodes number 0 and number 2 are children of the node 4.
5

child of below
4 -1 4 1 1
0  1 2 3 4

       root          1
                 /       \
                3         4
                        /   \
                       0     2
Output:

3


//////////////////////////
Sample 2.
Explanation:

The input means that there are 5 nodes with numbers from 0 to 4, node 0 is the root, node 1 is a child

of node 0, node 2 is a child of node 4, node 3 is a child of node 0 and node 4 is a child of node 3. The

height of this tree is 4, because the number of vertices on the path from root 0 to leaf 2 is 4.

Input:

5

-1 0 4 0 3

               root   0
                 /       \
                1         3
                         /
                       4
                      /
                     2
Output:




Output:

4



*/
