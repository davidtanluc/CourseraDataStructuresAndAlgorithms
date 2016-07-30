import scala.collection.mutable

/**
  * Created by davidtan on 7/19/16.
  */
object tree_height {

  def solution(n: Int, parent: Array[Int]): Int = {
    val heights = mutable.Map[Int, Int]()
    var maxHeight = 0
    for (vertex <- 0 until n) {
      var height:Int = 0
      var i = vertex //0 ,1,2,3,4

      while (i != -1) {
        height += 1
        i = parent(i) // Array(4, -1, 4, 1, 1);; 0->4
        if (heights.isDefinedAt(i)) {
          height += heights(i)
          i = - 1   //end
        }
      }//while
      maxHeight = maxHeight max height
      heights(vertex) = height
      //println(heights)

      /*
  Map(0 -> 3)
  Map(1 -> 1, 0 -> 3)
  Map(2 -> 3, 1 -> 1, 0 -> 3)
  Map(2 -> 3, 1 -> 1, 3 -> 2, 0 -> 3)
  Map(2 -> 3, 4 -> 2, 1 -> 1, 3 -> 2, 0 -> 3)
       */

    }
    maxHeight
  }
  def main(args: Array[String]) {
    val N = readLine.toInt
    val inputs =readLine.split(' ').map(_.toInt)
   println(solution(N, inputs))//solution(5, Array(4, -1, 4, 1, 1))//3

  }
}
