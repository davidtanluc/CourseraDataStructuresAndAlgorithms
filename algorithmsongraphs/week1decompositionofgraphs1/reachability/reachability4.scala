import java.util.Scanner

import scala.collection.mutable

/**
  * Created by davidtan on 8/14/16.
  */
object reachability4 {
  type Vertex=Int
  type Graph=Map[Vertex,List[Vertex]]

  private def Explore3(adj: Graph, src: Vertex, dst: Vertex, visited:List[Vertex]): Int = {
    if (src == dst) return 1 //#END

      if(adj.isDefinedAt(src))
        for(neighbour<-adj(src);if !visited.contains(neighbour))
            if (Explore3(adj,neighbour, dst, neighbour :: visited) == 1) return 1


    0 //#END
  }
  private def reach3(g:mutable.Map[Vertex,List[Vertex]],src: Vertex, dst: Vertex): Int = {
    // Map(2 -> List(3, 1), 1 -> List(0, 2), 3 -> List(0, 2), 0 -> List(1, 3))
    Explore3(g.toMap,src,dst,List())
  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n: Int = scanner.nextInt //4
    val m: Int = scanner.nextInt //4

    //// Adjacency list /////
    val occ  = mutable.Map[Vertex,List[Vertex]]()
    for (i <- 0 until m) {
      var x = 0
      var y = 0
      x = scanner.nextInt // 1,3,4
      y = scanner.nextInt // 2,2,3

      if(occ.isDefinedAt(x-1)) occ(x-1) = y-1 :: occ(x-1) else occ(x-1) = List(y-1)
      if(occ.isDefinedAt(y-1)) occ(y-1) = x-1 :: occ(y-1) else occ(y-1) = List(x-1)
    }
    //println(occ)
    //Map(2 -> List(3, 1), 1 -> List(2, 0), 3 -> List(0, 2), 0 -> List(3, 1))

    /*
       0 -> 1,3
       1 -> 0,2
       2 -> 1,3
       3 -> 2,0
       Map(0->List(1,3),1->List(0,2), 2->List(1,3),3-> List(2,0)

       0 1
       2 1
       3 2
       0 3
     */
    val x: Int = scanner.nextInt - 1
    val y: Int = scanner.nextInt - 1

    System.out.println(reach3(occ, x, y))

    scanner.close()

  }
}

/*
14 August 2016
Good job! (Max time used: 0.80/3.00, max memory used: 45883392/536870912.)
4 4

1 2

3 2

4 3

1 4

1 4
1

                                4 -  3      3 - 2
                                |    |  ==> |   |
                                1 -  2      0 - 1
 */
