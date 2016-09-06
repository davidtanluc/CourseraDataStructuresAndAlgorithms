import java.util.Scanner

/** Runtime
    T(n) = T(n/2) + c
    Using master theorem
    a = 1,b=2,d=0
    Since d = logb (a)
    O(n^d log (n) )
    = log (n)

  * Created by davidtan on 8/12/16.
  */
object bsearch {

}
  private def linearSearch(a: Array[Int], target: Int): Int = {

    for (i <- a.indices) if (a(i) == target) return i

    -1
  }

  def binarySearch01[A](xs: Seq[A], x: A)(implicit ord: Ordering[A]): Int = {
    var (low, high) = (0, xs.size - 1)

    while (low <= high)

      (low + high) / 2 match {
        case mid if ord.gt(xs(mid), x) => high = mid - 1
        case mid if ord.lt(xs(mid), x) => low = mid + 1
        case mid => return mid
      }

    -1
  }


  def main(args: Array[String]): Unit = {
    val scanner: Scanner = new Scanner(System.in)
    val an: Int = scanner.nextInt
    val a: Array[Int] = new Array[Int](an)
    for (i <- 0 until an) a(i) = scanner.nextInt

    val bn: Int = scanner.nextInt
    for (i <- 0 until bn) print(binarySearch01(a, scanner.nextInt) + " ")

  }
}

/*
12 August 2016
Good job! (Max time used: 2.01/6.00, max memory used: 104865792/536870912.)
5 1 5 8 12 13

5 8 1 23 1 11


List(2, 0, -1, 0, -1)
*/
