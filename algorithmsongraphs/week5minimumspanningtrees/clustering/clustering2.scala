import java.util.{Scanner, Comparator, PriorityQueue}

/**
Clustering is a fundamental problem in data mining. The goal is to partition

a given set of objects into subsets (or clusters) in such a way that any two

objects from the same subset are close (or similar) to each other, while any

two objects from different subsets are far apart.

  Task. Given n points on a plane and an integer k, compute the largest possible value of d such that the

given points can be partitioned into k non-empty subsets in such a way that the distance between any

two points from different subsets is at least d.

  * Created by davidtan on 9/3/16.
  */
object clustering2 {

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
    var pq = collection.mutable.PriorityQueue[Edge]()
    // Permute all vertices to populate Edges.
    for (i <-0 until n;j <-i+1 until n) pq += Edge(i, j, distance(x(i), y(i), x(j), y(j)))
    ////////////////////////////////////////
    val cmp = new Comparator[Edge]  {
      @Override def compare(e1: Edge, e2: Edge): Int = {
        if (e1.w < e2.w) -1 else 1
      }
    }
    import java.util.Collections
    val mst = new PriorityQueue[Edge](k - 1, Collections.reverseOrder(cmp))

    // Traverse each edge in increasing order.
    while (pq.nonEmpty) {
      val lightest = pq.dequeue ///PQ
      //println(lightest.toString)
      if (sets.find(lightest.u) != sets.find(lightest.v)) {
        sets.union(lightest.u, lightest.v)
        mst.offer(lightest)//add
      }
    }
    for (i<- 1 to k-2) mst.poll
    mst.peek.w
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
3 September 2016
Good job! (Max time used: 1.12/6.00, max memory used: 52404224/536870912.)

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