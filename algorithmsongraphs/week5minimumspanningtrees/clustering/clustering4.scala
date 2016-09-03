import java.util.{Scanner}


/**
  * Created by davidtan on 9/3/16.
  */
object clustering4 {
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

  /////////////////////////////
  private def clustering (x: Array[Int], y: Array[Int], k: Int) :Double = {
    val n = x.length

    val sets = new DisjointSets(n)
    for (i<- 0 until n) sets.makeSet(i)

    val pq = collection.mutable.PriorityQueue[Edge]()

    val mst = collection.mutable.PriorityQueue.empty[Edge](implicitly[Ordering[Edge]].reverse)

    // Permute all vertices and load all Edges.
    for (i <- 0 until n;j <- i+1 until n) pq += Edge(i, j, distance(x(i), y(i), x(j), y(j)))
    //println(pq)

    ////////////////////////////////////////
    // Traverse each edge in increasing order.
    while (pq.nonEmpty) {
      val lightest = pq.dequeue ///PQ
      //println(lightest.toString)

      if (sets.find(lightest.u) != sets.find(lightest.v)) {
        sets.union(lightest.u, lightest.v)
        mst += lightest
      }

    }
    //
    // show k-1
    for (i<- 1 to k-2) mst.dequeue

    mst.head.w

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
    val k: Int = scanner.nextInt
    println(clustering(x, y, k))

  }
}
/*
Good job! (Max time used: 0.95/6.00, max memory used: 52973568/536870912.)
12

7 6

4 3

5 1

1 7

2 7

5 7

3 3

7 8

2 8

4 4

6 7

2 6

3


2.8284271247461903

///

8

3 1

1 2

4 6

9 8

9 9

8 9

3 11

4 12

4

Output:

5.0

 */