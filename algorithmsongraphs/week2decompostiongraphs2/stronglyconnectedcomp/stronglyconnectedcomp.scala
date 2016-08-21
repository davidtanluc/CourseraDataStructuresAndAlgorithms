import java.util
import java.util.Scanner

/**
  * Created by davidtan on 8/15/16.
  http://www.cs.usfca.edu/~galles/visualization/ConnectedComponent.html

  Problem Introduction

  The police department of a city has made all streets one-way. Eou would like

  to check whether it is still possible to drive legally from any intersection to

  any other intersection. For this, you construct a directed graph: vertices are

  intersections, there is an edge (u, v) whenever there is a (one-way) street from

  u to v in the city. Then, it suffices to check whether all the vertices in the

  graph lie in the same strongly connected component.

  */
object stronglyconnectedcomp {

  private def numberOfStronglyConnectedComponents(adj: Array[util.ArrayList[Int]]): Int = {
    val adjR: Array[util.ArrayList[Int]] = reverseGraph(adj)
    // DFS on GR for all source CCs in postorder.
    val post: util.Stack[Int] = new util.Stack[Int]
    var visited = new Array[Boolean](adj.length)
    for (v<- adj.indices) {
      if (!visited(v)) dfs(adjR, v, visited, post)
    }
    var res: Int = 0
    visited = new Array[Boolean](adj.length)
    // Reverse from max post source in GR, i.e. sink CC in G.
    while (!post.isEmpty) {
      val v: Int = post.pop
      if (!visited(v)) {
        dfs(adj, v, visited, null)
        res += 1
      }

    }

    res
  }

  private def reverseGraph(adj: Array[util.ArrayList[Int]]): Array[util.ArrayList[Int]] = {
    /**Reverse G to GR: O(V+E)*/
    val res = new Array[util.ArrayList[Int]](adj.length)
    // Initialize empty adjacency list for each vertex. O(V)
    for (v<- adj.indices) res(v) = new util.ArrayList[Int]
    for (v<- adj.indices){
      import scala.collection.JavaConversions._
      for (w <- adj(v)) {
        res(w).add(v)
      }
    }

    res
  }

  private def dfs(adj: Array[util.ArrayList[Int]], v: Int, visited: Array[Boolean],
                  stack: util.Stack[Int]) {
    visited(v) = true
    import scala.collection.JavaConversions._
    for (w <- adj(v)) {
      if (!visited(w)) {
        dfs(adj, w, visited, stack)
      }
    }
    if (stack != null) stack.push(v)
  }


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
    //println(adj.toList)//List([1], [2], [0], [0])
    println(numberOfStronglyConnectedComponents(adj))
  }
}
/*
15 August 2016 at 6:27 PM
Good job! (Max time used: 1.70/3.00, max memory used: 71094272/536870912.)
4 4

1 2

4 1

2 3

3 1



2
/////



2 1

3 2

3 1

4 3

4 1

5 2

5 3


 */