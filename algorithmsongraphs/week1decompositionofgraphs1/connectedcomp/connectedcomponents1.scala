import java.util.Scanner

import scala.collection.mutable

/**
  * Created by davidtan on 8/14/16.
  */
object connectedcomponents1 {
  type Vertex=Int
  type Graph=Map[Vertex,List[Vertex]]

  private def numberOfComponents(adj: Graph):Int = {

    def DFS0(v: Vertex, visited: List[Vertex]): List[Vertex] = {
      if (visited.contains(v))
        visited
      else {
        val neighbours:List[Vertex] = adj(v) filterNot visited.contains

        neighbours.foldLeft(v :: visited)((acc,a) => {DFS0(a,acc)})
      }
    }

    def loop(xs: List[Int], visited:List[Vertex]): Int = xs match {
      case Nil => 0
      case (head :: ys1) =>  {
        val tmp = DFS0(head,visited)
        if(tmp.sorted != visited.sorted)loop(ys1,tmp) + 1
        else loop(ys1,tmp)
      }
    }
    val keys_list = adj.keys.toList
    loop(keys_list,List())

  }

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
      if(occ.isDefinedAt(y-1)) occ(y-1) = x-1 :: occ(y-1) else occ(y-1) = List(x-1)
    }
    //println(occ)//Map(2 -> List(1), 1 -> List(2, 0), 3 -> List(), 0 -> List(1))

    System.out.println(numberOfComponents(occ.toMap))
    scanner.close()

  }
}
/*
14 August 2016
Good job! (Max time used: 1.67/3.00, max memory used: 66670592/536870912.)
with n vertices and m edges,
4 2
1 2
3 2

///

2


                   4   3   ==> 3 2
                       |         |
                   1 - 2       0-1

 */
