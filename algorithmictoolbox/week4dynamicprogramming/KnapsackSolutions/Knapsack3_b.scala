import java.util.Scanner

/**
  * Created by davidtan on 8/12/16.
  */
object Knapsack3_b {

  private def optimalWeight(W: Int, w: Array[Int]): Int = {// greedy
  var result: Int = 0
    for (i <- w.indices) {
      if (result + w(i) <= W) {
        result += w(i)
      }
    }
    result
  }
  private def optimalWeight_Repetition(w: Array[Int],v: Array[Int],W: Int): Int = {
    val Value = Array.ofDim[Int](W+1)
    //println(Value.toList)//List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    for(weight <- 1 to W){
      for(i<-w.indices){
        if(w(i) <= weight){//1,4,8
          Value(weight) =  Value(weight) max Value(weight - w(i)) + v(i)
        }
      }
    }
    Value(W)
  }

  private def optimalWeight_Repetition2(weights: Array[Int],profits: Array[Int],maxweight: Int): Int = {
    val Value = Array.ofDim[Int](maxweight+1)

    for(one_to_maxweight <- 1 to maxweight; i<-weights.indices)
      if(weights(i) <= one_to_maxweight)
        Value(one_to_maxweight) =  Value(one_to_maxweight) max Value(one_to_maxweight - weights(i)) + profits(i)

    Value(maxweight)
  }

  private def optimalWeight_NoRepetition2(weights: Array[Int],profits: Array[Int], maxweight: Int): Int = {
    val n = weights.length

    val Value = Array.ofDim[Int](n+1,maxweight+1)
    //cur -->current_element
    for (cur<-1 to n ; one_to_maxweight <-1 to maxweight){
        val prev_elem = cur -1
        if (weights(prev_elem) <= one_to_maxweight) {

          Value(cur)(one_to_maxweight) =
            Value(prev_elem)(one_to_maxweight - weights(prev_elem)) + profits(prev_elem) max
              Value(prev_elem)(one_to_maxweight)

        }else Value(cur)(one_to_maxweight) = Value(prev_elem)(one_to_maxweight)
      }
    Value(n)(maxweight)
  }

    // compute in bottom up manner
  private def optimalWeight_NoRepetition(weight: Array[Int],profit: Array[Int], maxweight: Int): Int = {
    val n = weight.length

    val Value = Array.ofDim[Int](n+1,maxweight+1)
//    for (i<-0 to n)Value(i)(0) = 0
//    for (i<-0 to maxweight) Value(0)(i) = 0

    /*
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
     */
    for (cur<-1 to n) //current_element
      for (one_to_maxweight <-1 to maxweight){
        val prev_elem = cur -1

        if (weight(prev_elem) <= one_to_maxweight) {

          Value(cur)(one_to_maxweight) =
            Value(prev_elem)(one_to_maxweight - weight(prev_elem)) + profit(prev_elem) max
              Value(prev_elem)(one_to_maxweight)

        }else Value(cur)(one_to_maxweight) = Value(prev_elem)(one_to_maxweight)
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
      weight(i) = tmp// 1 4 8
      profit(i) = tmp //1 4 8

    }
    //println(optimalWeight(maxweight, weight)) //5
    //println(optimalWeight_NoRepetition(weight,profit, maxweight)) //9
    println(optimalWeight_Repetition(weight,profit, maxweight)) //10
  }

}
/*


10 3
1 4 8
Output
5
Explanation:

Here, the sum of the weights of the first and the last bar is equal to 9.
 */