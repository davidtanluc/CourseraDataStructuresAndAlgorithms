import java.util.Scanner

import scala.collection.mutable

/**
  * Created by davidtan on 8/17/16.
  */
object acyclicity {
  type Vertex=Int
  type Graph=Map[Vertex,List[Vertex]]

  private def DFS(adj: Graph, start: Vertex, end: Vertex, visited:List[Vertex]): Int = {
    if (start == end) return end //#END

    //// start from ////////
    var src1 = start
    if(start == -1)src1 = end

    if(adj.isDefinedAt(src1))
      for(neighbour<-adj(src1);if !visited.contains(neighbour)){

        val last =  DFS(adj,neighbour, end, neighbour :: visited)
          if(last > 0)return last
      }


    0 //#END
  }
  private def acyclic(g:mutable.Map[Vertex,List[Vertex]]): Int = {

    val keys_list = g.keys.toList
    //println(keys_list)//List(2, 1, 3, 0)

    for(key<-keys_list){
      val tmp1= DFS(g.toMap,-1,key,List())
      if(tmp1>0)return 1
    }

    0
  }

  ///////////////////// MAIN //////////////////////////////
  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n: Int = scanner.nextInt
    val m: Int = scanner.nextInt
    val occ  = mutable.Map[Vertex,List[Vertex]]()
    for (i <- 0 until n) occ(i) = List()

    for (i <- 0 until m) {
      var x = 0
      var y = 0
      x = scanner.nextInt
      y = scanner.nextInt

      if(occ.isDefinedAt(x-1)) occ(x-1) = y-1 :: occ(x-1) else occ(x-1) = List(y-1)
    }
    //println(occ)//Map(2 -> List(0), 1 -> List(2), 3 -> List(0), 0 -> List(1))

    println(acyclic(occ))
    scanner.close()

  }
}
/*
17 August 2016 
Good job! (Max time used: 2.16/3.00, max memory used: 131661824/536870912.)
4 4

1 2

4 1

2 3

3 1



1

/////

5 7

1 2

2 3

1 3

3 4

1 4

2 5

3 5


0
 */