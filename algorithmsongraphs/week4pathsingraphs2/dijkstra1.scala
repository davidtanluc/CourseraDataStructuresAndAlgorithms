import java.util
import java.util.Scanner

import scala.collection.mutable

/**
  * Problem Introduction

  * Now, you are interested in minimizing not the number of segments, but the total cost of a flight. For this

  * you construct a weighted graph: the weight of an edge from one city to another one is the cost of the

  * corresponding flight.

  * http://www.cs.usfca.edu/~galles/visualization/Dijkstra.html
  */
object dijkstra1 {
  type Vertex = Int
  type Graph = Map[Vertex, List[Vertex]]
  val INF = Int.MaxValue


  def Dijkstra (adj: mutable.Map[Vertex, List[Vertex]], cost: Array[util.ArrayList[Int]], s: Int, t: Int):Int= {
    val n = adj.size
    val dist = new Array[Int](n)
    val prev = new Array[Int](n)
    val visited = new Array[Boolean](n)

    for (i<- 0 until n) {
      dist(i) = INF
      prev(i) = -1
    }
    ///
    dist(s) = 0

    // Traverse each vertex outside of known region R.
    for (j<- 0 until n) {
      val u = ExtractMin(dist, visited)

      if(u != -1){

        visited(u) = true

        for (i <- adj(u).indices;
             v = adj(u)(i);
             w = cost(u).get(i)
             if dist(v) > dist(u) + w) {

            dist(v) = dist(u) + w
            prev(v) = u
        }
      }
      ///
    }

    if (dist(t) == INF) -1 else dist(t)
  }


  def ExtractMin(dist: Array[Int], visited: Array[Boolean]): Int = {
    var minDist = INF
    var minVertex = -1

    for (v<- dist.indices
         if !visited(v)) {

      if (dist(v) < minDist) minVertex = v

      minDist = minDist min dist(v)

    }
    ///

    minVertex
  }

  /////
  def main(args: Array[String]) {

    val scanner: Scanner = new Scanner(System.in)
    val n: Int = scanner.nextInt
    val m: Int = scanner.nextInt
    val occ = mutable.Map[Vertex, List[Vertex]]()

    val cost = new Array[util.ArrayList[Int]](n)

    for (i<-0 until n) {
      occ(i) = List()
      cost(i) = new util.ArrayList[Int]
    }
    ////
    for (i<-0 until m) {
      val x = scanner.nextInt
      val y = scanner.nextInt
      val w = scanner.nextInt

      if (occ.isDefinedAt(x - 1)) occ(x - 1) =  occ(x - 1) :+ y - 1 else occ(x - 1) = List(y - 1)
      cost(x - 1).add(w)
    }
    ////

    val h = scanner.nextInt - 1
    val k = scanner.nextInt - 1
    println(Dijkstra(occ, cost, h, k))


  }


}

/*
26 August 2016
Good job! (Max time used: 2.65/6.00, max memory used: 156712960/536870912.)
Sample 1.
Input:

4 4

1 2 1

4 1 2

2 3 2

1 3 5

1 3



3

//Using array: 3

Sample 1.

Input:

5 9

1 2 4

1 3 2

2 3 2

3 2 1

2 4 2

3 5 4

5 4 1

2 5 3

3 4 4

1 5

6
 */