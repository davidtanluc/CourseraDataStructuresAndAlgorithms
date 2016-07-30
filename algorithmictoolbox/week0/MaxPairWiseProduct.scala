
/**
  * Created by davidtan on 6/28/16.
  */
object MaxPairWiseProduct {

  def main(args: Array[String]) {
    val N = readLine.toInt
    val inputs =readLine.split(' ').map(_.toInt)
    val sorted = inputs.sorted
    //println(sorted)
    println(sorted(N-2)*sorted(N-1).toLong)

  }

}
/*
5
4 6 2 6 1
2
 */