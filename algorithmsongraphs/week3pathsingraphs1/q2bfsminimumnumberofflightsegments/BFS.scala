import java.util.Scanner

import scala.collection.mutable

/**
  * Created by davidtan on 8/24/16.
  */
object BFS {
  type Vertex=Int
  type Graph=Map[Vertex,List[Vertex]]

  private def distance(adj:mutable.Map[Vertex,List[Vertex]], s: Int, t: Int): Int = {

    /**BFS: add all vertices on same layer before next layer.*/
    val dist = Array.fill(adj.size)(Int.MaxValue)
    dist(s) = 0

    val q = mutable.Queue[Int]()
    q.enqueue(s)

    while (q nonEmpty) {
      val u = q.dequeue()

      for (v <- adj(u);if dist(v) == Int.MaxValue) {
          q.enqueue(v)
          dist(v) = dist(u) + 1
      }
    }
    if (dist(t) == Int.MaxValue) -1 else dist(t)
  }


  /////////////////////////////
  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n = scanner.nextInt
    val m = scanner.nextInt
    val occ  = mutable.Map[Vertex,List[Vertex]]()
    for (i<-0 until n) occ(i) = List()

    for (i<-0 until m) {
      var x = 0
      var y = 0
      x = scanner.nextInt
      y = scanner.nextInt
      if(occ.isDefinedAt(x-1)) occ(x-1) = y-1 :: occ(x-1) else occ(x-1) = List(y-1)
      if(occ.isDefinedAt(y-1)) occ(y-1) = x-1 :: occ(y-1) else occ(y-1) = List(x-1)
    }
    val x = scanner.nextInt - 1
    val y = scanner.nextInt - 1
    println(distance(occ, x, y))

  }
}

/*
24 August 2016
Good job! (Max time used: 2.46/6.00, max memory used: 137281536/536870912.)
4 4

1 2

4 1

2 3

3 1

2 4


2

//////

5 4

5 2

1 3

3 4

1 4

3 5

-1

 */
