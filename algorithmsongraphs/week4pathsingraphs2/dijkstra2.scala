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
object dijkstra2 {
  type Vertex = Int
  type Graph = Map[Vertex, List[Vertex]]
  val INF = Int.MaxValue


  def Dijkstra (adj: mutable.Map[Vertex, List[Vertex]], cost: mutable.Map[Vertex, List[Vertex]], s: Int, t: Int):Int= {
    val n = adj.size
    val dist = new Array[Int](n)
    val visited = new Array[Boolean](n)

    for (i<- 0 until n) dist(i) = INF
    ///
    dist(s) = 0

    // Traverse each vertex outside of known region R.
    for (j<- 0 until n) {

      val minVertex = ExtractMin(dist, visited)
      //println("MIN VERTEX",minVertex)

      if(minVertex != -1){

        visited(minVertex) = true

        for (i <- adj(minVertex).indices;
             current_vertex = adj(minVertex)(i);
             weight = cost(minVertex)(i)
             if dist(current_vertex) > dist(minVertex) + weight) {

            dist(current_vertex) = dist(minVertex) + weight
          //println("minVertex ",minVertex,"current_vertex:",current_vertex)
          /*

          becomes 0,1 ;; 0,2
          becomes 0,2,1;;024;;023
          
MIN VERTEX 0
(minVertex ,0,current_vertex:,1)
(minVertex ,0,current_vertex:,2)(CHOSE)
MIN VERTEX 2
(minVertex ,2,current_vertex:,1)(CHOSE)
(minVertex ,2,current_vertex:,4) (OK end path1)
(minVertex ,2,current_vertex:,3) 3 -> List(), (OUT)
1
(minVertex ,1,current_vertex:,3) 3 -> List(), (OUT)
3 -->3 -> List(), (OUT)
4 (#END)
  1 → 3 → 5 and 1 → 3 → 2 → 5.
  0 2 4         0214
    Map(,
4 -> List(3),
3 -> List(),
2 -> List(1, 4, 3)
1 -> List(2, 3, 4)
0 -> List(1, 2))

    */
            //prev(current_vertex) = minVertex

        }
      }
      ///
    }
//println(dist.toList)//List(0, 3, 2, 5, 6)
    if (dist(t) == INF) -1 else dist(t)
  }

  // go to 1 -> 3
  def ExtractMin(dist: Array[Int], visited: Array[Boolean]): Int = {
    var minDist = INF
    var minVertex = -1

    for (v<- dist.indices
         if !visited(v)) {

      if (dist(v) < minDist){
        minVertex = v
        //println(v," dist",dist(v))
      }
      //println("Before",v," ",dist(v))
      minDist = minDist min dist(v)
      //if(minDist ==dist(v))println("chosen",v," ",dist(v))

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
    //println(rev)//Map(2 -> List(0, 1), 4 -> List(2, 1), 1 -> List(0, 2), 3 -> List(1, 4, 2), 0 -> List())
    //println(occ)
    //Map( ,
    // 3 -> List(0),
    // 2 -> List(),
    // 1 -> List(2)
    // 0 -> List(1, 2))
    //println(cost.toList)//List(
    // [2]
    // [],
    // [2],
    // [1, 5],   )
    val h = scanner.nextInt - 1
    val k = scanner.nextInt - 1
    println(Dijkstra(occ, cost, h, k))


  }


}

/*
2 September 2016
Good job! (Max time used: 3.77/6.00, max memory used: 152174592/536870912.)
Sample 1.
https://rosettacode.org/wiki/Dijkstra%27s_algorithm#Scala
Input:

4 4

1 2 1

4 1 2

2 3 2

1 3 5

1 3



3

//Using array: 3
(dist,List(0, 3, 2, 5, 6))
FROM 0 to 4
total weight 6: 1 → 3 → 5 and 1 → 3 → 2 → 5.
0 -2 -4
0 -2-1-4
/////////////////////////////
Map(,
4 -> List(3),
3 -> List(),
2 -> List(1, 4, 3)
1 -> List(2, 3, 4)
0 -> List(1, 2))
/////////////////////////////////////
0
2

1
3
4

PATHS ARE => 0,2,1,4 or 0,2,4 or 0,2,3# , 0, 1 ,4
0-> 2 ->4
0-> 2 ->3 -> 4
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