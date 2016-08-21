import java.util
import java.util.{Collections, Scanner}

/**
  * Created by davidtan on 8/14/16. //3 Problem: Determining an Order of Courses
  *
  *
  * Now, when you are sure that there are no cyclic dependencies in the given CS curriculum, you would like to

  *find an order of all courses that is consistent with all dependencies. For this, you find a topological ordering

   *of the corresponding directed graph.
  */
object topological1 {
  private def toposort(adj: Array[util.ArrayList[Int]]): util.ArrayList[Int] = {
    /** DFS + reverse postorder() */
    val used = new Array[Int](adj.length)
    val order: util.ArrayList[Int] = new util.ArrayList[Int]
    // Can store postorder into stack and pop all as well.
    for (v<-adj.indices) {
      if (used(v) == 0) dfs(adj, used, order, v)
    }
    Collections.reverse(order)
    order
  }
  private def dfs(adj: Array[util.ArrayList[Int]], used: Array[Int],
                  order: util.ArrayList[Int], s: Int): Unit = {
    /** Add each current vertex after traversing all adjacent nodes. */
    used(s) = 1
    import scala.collection.JavaConversions._
    for (t <- adj(s)) {
      if (used(t) == 0) dfs(adj, used, order, t)
    }
    // Sort vertices based on postorder.
    order.add(s)

  }

  ////////////////////////////////

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n: Int = scanner.nextInt
    val m: Int = scanner.nextInt
    val adj = new Array[util.ArrayList[Int]](n)
    for (i<-0 until n) {
      adj(i) = new util.ArrayList[Int]
    }
    for (i<-0 until m) {
      var x: Int = 0
      var y: Int = 0
      x = scanner.nextInt
      y = scanner.nextInt
      adj(x - 1).add(y - 1)
    }
    //println(adj.toList)//List([1], [], [0], [0])

    val order: util.ArrayList[Int] = toposort(adj)
    import scala.collection.JavaConversions._
    for (x <- order) {
      System.out.print((x + 1) + " ")
    }
  }
}
/*
15 August 2016 at 6:23 PM
Good job! (Max time used: 0.93/3.00, max memory used: 45416448/536870912.)
Output any topological ordering of its vertices.
4 3

1 2

4 1

3 1

////
4 3 1 2


5 7

2 1

3 2

3 1

4 3

4 1

5 2

5 3


http://www.cs.usfca.edu/~galles/visualization/TopoSortDFS.html
http://www.cs.usfca.edu/~galles/visualization/TopoSortIndegree.html

 */