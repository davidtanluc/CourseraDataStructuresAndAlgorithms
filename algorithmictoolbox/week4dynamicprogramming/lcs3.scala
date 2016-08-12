package week4dynamicprogramming

import java.util.Scanner

/**
  * Created by davidtan on 8/9/16.
  */
object lcs3 {
  def lcs3[T](str1: Array[T], str2: Array[T], str3: Array[T], len1: Int, len2: Int, len3: Int): Int = {

    if (len1 == str1.length || len2 == str2.length || len3 == str3.length) return 0

    if (str1(len1) == str2(len2) && str2(len2) == str3(len3)) lcs3(str1, str2, str3, len1 + 1, len2 + 1, len3 + 1) + 1

    else lcs3(str1, str2, str3, len1 + 1, len2, len3) max lcs3(str1, str2, str3, len1, len2 + 1, len3) max lcs3(str1, str2, str3, len1, len2, len3 + 1)
  }


  def lcsDynamic3[T](str1: Array[T], str2: Array[T], str3: Array[T]): Int = {
    val DP = Array.ofDim[Int](str1.length + 1, str2.length + 1, str3.length + 1)

    var max = 0

    for (i <- 1 until DP.length; prev_i = i - 1;
         j <- 1 until DP(i).length; prev_j = j - 1;
         k <- 1 until DP(i)(j).length;prev_k = k - 1) {

          if (str1(prev_i) == str2(prev_j) && str2(prev_j) == str3(prev_k))

                 DP(i)(j)(k) = DP(prev_i)(prev_j)(prev_k) + 1

          else DP(i)(j)(k) = DP(prev_i)(j)(k) max DP(i)(prev_j)(k) max DP(i)(j)(prev_k)

          max = max max DP(i)(j)(k)
      }
    //#END
    max

  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)

    val an: Int = scanner.nextInt
    val a: Array[Int] = new Array[Int](an)
    for (i <- 0 until an) a(i) = scanner.nextInt

    val bn: Int = scanner.nextInt
    val b: Array[Int] = new Array[Int](bn)

    for (i <- 0 until bn) b(i) = scanner.nextInt

    val cn: Int = scanner.nextInt
    val c: Array[Int] = new Array[Int](cn)

    for (i <- 0 until cn) c(i) = scanner.nextInt
    //println(a.toList,b.toList,c.toList)
    //println(lcs3(a,b,c,0,0,0)) //2
    println(lcsDynamic3(a, b, c)) //2
    //System.out.println(lcsDynamic3(a, b, c))
    scanner.close()
  }
}

/*
2 August 2016
Good job! (Max time used: 1.05/6.00, max memory used: 71749632/536870912.)

3

1 2 3

3

2 1 3

3

1 3 5

Output
2

Input:
5

8 3 2 1 7

7

8 2 1 3 8 10 7

6

6 8 3 1 4 7

3
 */