import java.util.Scanner

import scala.collection.mutable

/**
  * An undirected graph is called bipartite if its vertices can be split into two parts such that each edge of the

  * graph joins to vertices from different parts. Bipartite graphs arise naturally in applications where a graph

  * is used to model connections between objects of two different types (say, boys and girls; or students and

  * dormitories).

  * An alternative definition is the following: a graph is bipartite if its vertices can be colored with two colors

  * (say, black and white) such that the endpoints of each edge have different colors.

  */
object BiPartite {

  type Vertex = Int
  type Graph = Map[Vertex, List[Vertex]]

  private def biPartite(adj: mutable.Map[Vertex, List[Vertex]]): Int = {

    /** BFS: add all vertices on same layer before next layer. */
    val dist = Array.fill(adj.size)(Int.MaxValue)
    val blackorWhite =Array.fill(adj.size)(false)
    dist(0) = 0

    val q = mutable.Queue[Int]()
    q.enqueue(0)

    while (q.nonEmpty) {
      val u = q.dequeue()

      for (v <- adj(u)) {
        if (dist(v) == Int.MaxValue) {
          q.enqueue(v)
          dist(v) = dist(u) + 1
          blackorWhite(v) = !blackorWhite(u)

        }else if( blackorWhite(v) == blackorWhite(u)){//if it is of same color proved it is not a bipartite
          return 0
        }
      }
    }
    1
  }


  /////////////////////////////
  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n = scanner.nextInt
    val m = scanner.nextInt
    val occ = mutable.Map[Vertex, List[Vertex]]()
    for (i <- 0 until n) occ(i) = List()

    for (i <- 0 until m) {
      var x = 0
      var y = 0
      x = scanner.nextInt
      y = scanner.nextInt
      if (occ.isDefinedAt(x - 1)) occ(x - 1) = y - 1 :: occ(x - 1) else occ(x - 1) = List(y - 1)
      if (occ.isDefinedAt(y - 1)) occ(y - 1) = x - 1 :: occ(y - 1) else occ(y - 1) = List(x - 1)
    }
    println(biPartite(occ))
  }
}


/*
25 August 2016
Good job! (Max time used: 2.65/6.00, max memory used: 138936320/536870912.)
4 4

1 2

4 1

2 3

3 1


0


///

5 4

5 2

4 2

3 4

1 4

1
 */