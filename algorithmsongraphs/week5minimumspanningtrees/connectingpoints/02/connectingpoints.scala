import java.util.{PriorityQueue, Comparator, Scanner}

/**
In this problem, the goal is to build roads between some pairs of the

given cities such that there is a path between any two cities and the

total length of the roads is minimized.

  * Created by davidtan on 9/2/16.
  */
object connectingpoints {

  private class DisjointSets {

    private var parents: Array[Int] = null
    private var ranks: Array[Int] = null

    def this(n: Int) {
      this()
      parents = new Array[Int](n)
      ranks = new Array[Int](n)
    }

    def makeSet(i: Int) {
      parents(i) = i
      ranks(i) = 1
    }

    def find(i: Int): Int = {
      if (parents(i) == i) return i

      parents(i) = find(parents(i))

      parents(i)
    }

    def union(i: Int, j: Int) {
      val r1: Int = find(i)
      val r2: Int = find(j)
      if (r1 == r2) return
      if (ranks(r1) < ranks(r2)) {
        parents(r1) = r2
      }
      else if (ranks(r2) < ranks(r1)) {
        parents(r2) = r1
      }
      else {
        parents(r2) = r1
        ranks(r1) += 1
      }
    }
  }

  case class Edge(u:Int=0,v:Int=0,w:Double=0.00)  extends Ordered[Edge]{
    def compare(that: Edge) = if (that.w < this.w) -1 else 1

    override def toString: String = {
      u + "\t" + v + "\t" + w
    }
  }
  private def distance(x1: Int, y1: Int, x2: Int, y2: Int): Double =
    Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))


  private def minimumDistance (x: Array[Int], y: Array[Int]): Double= {

    var result: Double = 0.00
    val n: Int = x.length
    val sets = new DisjointSets(n)

    var pq = collection.mutable.PriorityQueue[Edge]()

    for (i<- 0 until n) sets.makeSet(i)

    // Permute all vertices to populate Edges.
    for (i <-0 until n;j <-i+1 until n) pq += Edge(i, j, distance(x(i), y(i), x(j), y(j)))

    // Traverse each edge in increasing order.
    while (pq nonEmpty) {
      val lightest = pq.dequeue

      //println(lightest.toString)
      /*
0	1	1.0
2	3	1.0
0	2	1.0
1	3	1.0
1	2	1.4142135623730951
0	3	1.4142135623730951
       */

      if (sets.find(lightest.u) != sets.find(lightest.v)) {
        sets.union(lightest.u, lightest.v)
        result += lightest.w
      }

    }
   result

  }


  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n = scanner.nextInt
    val x = new Array[Int](n)
    val y = new Array[Int](n)
    for (i <- 0 until n) {
      x(i) = scanner.nextInt
      y(i) = scanner.nextInt
    }
    println(minimumDistance(x, y))
  }


}

/*
2 September 2016
Good job! (Max time used: 0.80/6.00, max memory used: 43061248/536870912.)
Sample 1.

Input:

4

0 0

0 1

1 0

1 1





3.0


/////
5

0 0

0 2

1 1

3 0

3 2


7.06449510224598
 */

