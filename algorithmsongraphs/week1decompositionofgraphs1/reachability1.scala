import java.util
import java.util.Scanner

/**
  * Created by davidtan on 8/10/16.
  */
object reachability1 {

  private def reach(adj: Array[util.ArrayList[Int]], x: Int, y: Int): Int = {
    val visited: Array[Boolean] = new Array[Boolean](adj.length)
    dfs(adj, x, y, visited)
  }

  private def dfs(adj: Array[util.ArrayList[Int]], src: Int, dst: Int, visited: Array[Boolean]): Int = {
    if (src == dst) return 1
    visited(src) = true
    import scala.collection.JavaConversions._
    for (neighbor <- adj(src)) {
      if (!visited(neighbor)) if (dfs(adj, neighbor, dst, visited) == 1) return 1
    }
    0
  }
  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n: Int = scanner.nextInt
    val m: Int = scanner.nextInt
    val adj = new Array[util.ArrayList[Int]](n)

    for (i <- 0 until n) adj(i) = new util.ArrayList[Int]()
    for (i <- 0 until m) {
      var x = 0
      var y = 0
      x = scanner.nextInt
      y = scanner.nextInt
      adj(x - 1).add(y - 1)
      adj(y - 1).add(x - 1)
    }

    val x: Int = scanner.nextInt - 1
    val y: Int = scanner.nextInt - 1
    System.out.println(reach(adj, x, y))
    scanner.close


  }
}

/*


Input Format.

An undirected graph with n vertices and m edges.
The next line contains two vertices u and v of the graph.

Output 1 if there is a path between u and v and 0 otherwise.

10 August 2016
Good job! (Max time used: 0.76/3.00, max memory used: 45273088/536870912.)

4 4

1 2

3 2

4 3

1 4

1 4


                                4 -  3      3 - 2
                                |    |  ==> |   |
                                1 -  2      0 - 1
Output:
1

//////
4 2

1 2

3 2

1 4


                             4     3
                                   |
                             1  -  2
0
 */