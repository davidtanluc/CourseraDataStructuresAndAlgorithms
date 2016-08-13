import java.util
import java.util.Scanner

/**
  * Created by davidtan on 8/10/16.
  */
object connectedcomponents {

  private def dfs(vertex:Int, adj: Array[util.ArrayList[Int]], visited: Array[Boolean]):Unit={
    visited(vertex) = true
    import scala.collection.JavaConversions._
    for (neighbor <- adj(vertex)) if (!visited(neighbor)) dfs(neighbor, adj, visited)
  }

  private def numberOfComponents(adj: Array[util.ArrayList[Int]]): Int = {
    val visited: Array[Boolean] = new Array[Boolean](adj.length)
    var result: Int = 1
    for (i <- adj.indices) {
      if (!visited(i)) {
        dfs(i, adj, visited)
        result += 1
      }
    }
    result -1
  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n: Int = scanner.nextInt
    val m: Int = scanner.nextInt
    val adj = new Array[util.ArrayList[Int]](n)

    for (i <- 0 until n) adj(i) = new util.ArrayList[Int]()
    for (i <- 0 until m) {
      var x = 0
      var y = 0
      x = scanner.nextInt
      y = scanner.nextInt
      adj(x - 1).add(y - 1)
      adj(y - 1).add(x - 1)
    }

    System.out.println(numberOfComponents(adj))
    scanner.close()

  }
}
/*
10 August 2016
Good job! (Max time used: 0.74/3.00, max memory used: 45596672/536870912.)

4 2

1 2

3 2
2


                   4   3   ==> 3 2
                       |         |
                   1 - 2       0-1

                   Map(0->List(1),1->(0,2),2->List(1),4->List())
///////

 */