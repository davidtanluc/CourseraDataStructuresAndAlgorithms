import java.util
import java.util.Scanner

import scala.collection.mutable

/**
  * Created by davidtan on 8/13/16.
  */
object reachability3 {

  type Vertex=Int
  type Graph=Map[Vertex,List[Vertex]]

  private def Explore3(adj: Graph, src: Vertex, dst: Vertex, visited:List[Vertex]): Int = {
    if (src == dst) return 1 //#END

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
    val adj = new Array[util.ArrayList[Int]](n)


    //// Adjacency list /////
    for (i <- 0 until n) adj(i) = new util.ArrayList[Int]()// Array([],[],[],[]) size 4
    for (i <- 0 until m) {
      var x = 0
      var y = 0
      x = scanner.nextInt // 1,3,4
      y = scanner.nextInt // 2,2,3
      adj(x - 1).add(y - 1) // adj(0) = 1 ;;adj(2)=1;;adj(3)=2
      adj(y - 1).add(x - 1) // adj(1) = 0 ;;adj(1)=2;;adj(2)=3
    }
    //println(adj.toList)//List([1, 3], [0, 2], [1, 3], [2, 0])

    val occ  = mutable.Map[Vertex,List[Vertex]]()
    import scala.collection.JavaConversions._
    for (i <- adj.indices) {
      occ(i) = adj(i).toList
    }
    //println(occ)//Map(2 -> List(1, 3), 1 -> List(0, 2), 3 -> List(2, 0), 0 -> List(1, 3))
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
13 August 2016 at 5:18 PM
Good job! (Max time used: 0.81/3.00, max memory used: 46313472/536870912.)
4 4

1 2

3 2

4 3

1 4

1 4

 */
