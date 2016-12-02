package week4.dijkstra

import java.util.Scanner

import scala.collection.mutable

/**
  * Created by davidtan on 12/2/16.
  */
object dijkstra4 {
  type Vertex = Int
  type Graph = Map[Vertex, List[Vertex]]
  val INF = Int.MaxValue


  def Dijkstra (adj: mutable.Map[Vertex, List[Vertex]],
                cost: mutable.Map[Vertex, List[Vertex]],
                start: Int, end: Int):Int= {

    val n = adj.size
    val dist = new Array[Int](n)
    val visited = new Array[Boolean](n)

    for (i<- 0 until n) dist(i) = INF
    ///
    dist(start) = 0

    // Traverse each vertex outside of known region R.
    for (j<- 0 until n) {

      val minVertex = ExtractMin(dist, visited)

      if(minVertex != -1){

        visited(minVertex) = true

        for (i <- adj(minVertex).indices;
             current_vertex = adj(minVertex)(i);
             weight = cost(minVertex)(i))
          dist(current_vertex) = dist(current_vertex) min dist(minVertex) + weight


      }
      ///
    }
    //
    if (dist(end) == INF) -1 else dist(end)
  }


  def ExtractMin(distances: Array[Int],
                 visited: Array[Boolean]): Int = {

    var minDist = INF
    var minVertex = -1

    for (i<- distances.indices
         if !visited(i)) {

      if (minDist > distances(i))minVertex = i

      minDist = minDist min distances(i)

    }
    ///

    minVertex
  }

  /////
  def main(args: Array[String]) {

    val scanner= new Scanner(System.in)
    val n = scanner.nextInt
    val m = scanner.nextInt

    val occ = mutable.Map[Vertex, List[Vertex]]()
    val rev = mutable.Map[Vertex, List[Vertex]]()
    val cost = mutable.Map[Vertex, List[Vertex]]()

    for (i<-0 until n) {
      occ(i) = List()
      rev(i) = List()
      cost(i) = List()
    }

    ////
    for (i<-0 until m) {
      val x = scanner.nextInt
      val y = scanner.nextInt
      val w = scanner.nextInt

      if (occ.isDefinedAt(x - 1)) occ(x - 1) =  occ(x - 1) :+ y - 1 else occ(x - 1) = List(y - 1)
      if (rev.isDefinedAt(y - 1)) rev(y - 1) =  rev(y - 1) :+ x - 1 else rev(y - 1) = List(x - 1)
      if (cost.isDefinedAt(x - 1)) cost(x - 1) =  cost(x - 1) :+ w else cost(x - 1) = List(w)

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

Input:

4 4

1 2 1

4 1 2

2 3 2

1 3 5

1 3



3

///
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

//
6
 */
