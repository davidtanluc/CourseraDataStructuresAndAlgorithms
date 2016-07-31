import java.util.Scanner

/**
  * Created by davidtan on 7/15/16.
  */
object GCD {
  private def gcd_naive(a: Int, b: Int): Int = {
    var current_gcd = 1
    for (d <- 2 to a) {
      if (d <= a && a % d == 0 && b % d == 0 && d > current_gcd) {
        current_gcd = d;
      }
    }
    current_gcd
  }

  private def gcd(smaller:Int,bigger:Int):Int={
    if(bigger % smaller == 0) smaller else gcd(bigger % smaller,smaller)
  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val a: Int = scanner.nextInt
    val b: Int = scanner.nextInt
    //System.out.println(gcd_naive(a, b))
    System.out.println(gcd(a, b))
  }

}

/*
30 July 2016 at 7:10 PM
Good job! (Max time used: 0.59/3.00, max memory used: 38080512/536870912.)

3 30
3
 */
