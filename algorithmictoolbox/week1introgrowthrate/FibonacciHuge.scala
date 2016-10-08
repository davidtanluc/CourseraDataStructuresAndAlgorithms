import java.util
import java.util.Scanner

/**
  * Created by davidtan on 9/12/16.
  */
object FibonacciHuge {

  def getFibonacciHuge(n: Long, m: Long): Long = {
    val modulos = new util.ArrayList[Int]
    modulos.add(0)
    modulos.add(1)
    var i = 0
    while (!(i > 0 && modulos.get(i) == 0 && modulos.get(i + 1) == 1)) {
      modulos.add(((modulos.get(i) % m + modulos.get(i + 1) % m) % m).toInt)
      i += 1
    }
    //println("pisano period=" + i) //1176
    modulos.get((n % i).toInt)
  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n: Long = scanner.nextLong
    val m: Long = scanner.nextLong
    println(getFibonacciHuge(n, m))
  }

}
/*
19 September 2016
Good job! (Max time used: 0.66/3.00, max memory used: 42987520/536870912.)
Sample 1.

Input:

281621358815590 30524

Output:

11963



i  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15

Fi 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610

Fi mod 2 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0

Fi mod 3 0 1 1 2 0 2 2 1 0 1 1 2 0 2 2 1


Take a detailed look at this table. Do you see? Both these sequences are periodic! For m = 2, the period

is 011 and has length 3, while for m = 3 the period is 01120221 and has length 8. Therefore, to compute,

say, F2015 mod 3 we just need to find the remainder of 2015 when divided by 8. Since 2015 = 251 · 8 + 7, we

conclude that F2015 mod 3 = F7 mod 3 = 1.


This is true in general: for any integer m ≥ 2, the sequence Fn mod m is periodic. The period always

starts with 01 and is known as Pisano period.
//http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibmaths.html#section6.2

 */