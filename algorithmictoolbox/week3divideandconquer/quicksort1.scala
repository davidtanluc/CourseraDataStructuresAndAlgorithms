import java.util.Scanner

/**
  * Created by davidtan on 8/13/16.
  * Big O efficiency:

       Best Case Sort: Merge Sort: O(n)
       Average Case Sort: Merge Sort: O(n log n)
       Worst Case Sort: Merge Sort: O(n^2)
  */
object quicksort1 {
  private def swap(a: Array[Int], i: Int, j: Int) {
    val tmp: Int = a(i)
    a(i) = a(j)
    a(j) = tmp
  }
  //////// threes /////////////////////////
  private def partition3 (a: Array[Int], l: Int, r: Int):Array[Int]={
    var m1: Int = l
    var m2: Int = l
    val pivot: Int = a(l)

    for (j <- l + 1 to r; if a(j) <= pivot) {
      if (a(j) < pivot) {
        swap(a, m1, j)
        m1 += 1
      }
      swap(a, m2 + 1, j)
      m2 += 1
    }
    val m: Array[Int] = Array(m1, m2)

    m
  }

  private def randomizedQuickSort3(a: Array[Int], l: Int, r: Int) {
    if (l >= r) {
      return
    }

    import util.Random
    val k: Int = Random.nextInt(r - l + 1) + l
    val t: Int = a(l)
    a(l) = a(k)
    a(k) = t
    val ms: Array[Int] = partition3(a, l, r)
    val m1: Int = ms(0)
    val m2: Int = ms(1)
    randomizedQuickSort3(a, l, m1 - 1)
    randomizedQuickSort3(a, m2 + 1, r)
  }

  //////// twos /////////////////////////
  private def partition2(a: Array[Int], l: Int, r: Int): Int = {
    val x: Int = a(l)
    var j: Int = l
    for (i <- l + 1 to r) {
      if (a(i) <= x) {
        j += 1
        val t: Int = a(i)
        a(i) = a(j)
        a(j) = t
      }
    }

    val t: Int = a(l)
    a(l) = a(j)
    a(j) = t

    j
  }

  private def randomizedQuickSort2(a: Array[Int], l: Int, r: Int) {
    if (l >= r) {
      return
    }
    import util.Random
    val k: Int = Random.nextInt(r - l + 1) + l
    val t: Int = a(l)
    a(l) = a(k)
    a(k) = t
    val m: Int = partition2(a, l, r)
    randomizedQuickSort2(a, l, m - 1)
    randomizedQuickSort2(a, m + 1, r)
  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)

    val an: Int = scanner.nextInt
    val a: Array[Int] = new Array[Int](an)
    for (i <- 0 until an) a(i) = scanner.nextInt
    //randomizedQuickSort2(a, 0, an - 1)
    randomizedQuickSort3(a, 0, an - 1)
    for (elem <-a) print(elem + " ")


  }

}
/*
13 August 2016
Good job! (Max time used: 2.10/6.60, max memory used: 90071040/536870912.)
5

2 3 9 2 2

List(2, 2, 2, 3, 9)

 */