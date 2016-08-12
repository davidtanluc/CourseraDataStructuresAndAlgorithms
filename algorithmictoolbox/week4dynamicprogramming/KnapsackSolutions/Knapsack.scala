import java.util.Scanner

/**
  * Created by davidtan on 8/7/16.
  */
object Knapsack {
  private def optimalWeight(W: Int, w: Array[Int]): Int = {// greedy
    var result: Int = 0
    for (i <- w.indices) {
      if (result + w(i) <= W) {
      result += w(i)
      }
    }
    result
  }
  // compute in bottom up manner
  private def optimalWeight_NoRepetition(weight: Array[Int],profit: Array[Int], maxweight: Int): Int = {
    val n = weight.length

    val Value = Array.ofDim[Int](n+1,maxweight+1)
    for (i<-0 to n)Value(i)(0) = 0
    for (i<-0 to maxweight) Value(0)(i) = 0

    /*
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
     */
    for (cur<-1 to n) //current_element
      for (w <-1 to maxweight){
        val prev_elem = cur -1

        if (weight(prev_elem) <= w) {

          Value(cur)(w) =
            Value(prev_elem)(w - weight(prev_elem)) + profit(prev_elem) max
              Value(prev_elem)(w)

        }else Value(cur)(w) = Value(prev_elem)(w)
      }
//    for (row <- B) {
//      for (elem <- row) print(elem + " ")
//      println()
//    }
    /*
0 0 0 0 0 0 0 0 0 0 0
0 1 1 1 1 1 1 1 1 1 1
0 1 1 1 4 5 5 5 5 5 5
0 1 1 1 4 5 5 5 8 9 9
     */
    Value(n)(maxweight)
  }
  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    var maxweight: Int = 0
    var n: Int = 0
    maxweight = scanner.nextInt //10
    n = scanner.nextInt // 3
    val weight = Array.fill(n)(0) // 1 4 8
    val profit =Array.fill(n)(0) // 1 4 8
    for (i<-0 until n) {
      val tmp = scanner.nextInt
      weight(i) = tmp// 1 2 3
      profit(i) = tmp //1 4 8

    }
    //println(optimalWeight(maxweight, weight)) //5
    println(optimalWeight_NoRepetition(weight,profit, maxweight)) //9
  }
}
/*
8 August 2016 at 7:37 PM
Good job! (Max time used: 0.73/6.00, max memory used: 52215808/536870912.)

10 3
1 4 8
Output
5
Explanation:

Here, the sum of the weights of the first and the last bar is equal to 9.

 */