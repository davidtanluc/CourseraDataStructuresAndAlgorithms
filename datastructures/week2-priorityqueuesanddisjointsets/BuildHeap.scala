/**
  * Created by davidtan on 7/24/16.
  */
object BuildHeap {
  private var H: Array[Int] = null
  private var swaps: java.util.List[Swap] = null

  case class Swap(val index1: Int, val index2: Int)

  def main(args: Array[String]) {
    val n = readLine.toInt
    H = readLine.split(' ').map(_.toInt)
    buildHeap
    writeResponse
  }

  ///// helper functions ////
  private def writeResponse {
    println(swaps.size)
    import scala.collection.JavaConversions._
    for (swap <- swaps) {
      println(swap.index1 + " " + swap.index2)
    }
  }
  private def siftDown(k: Int) {//// Methods are public by default
  val n1  = H.length
    var i = k
    while (i * 2 + 1 < n1) {
      var left_idx = i * 2 + 1
      val right_idx = left_idx + 1
      if (right_idx < n1 && H(right_idx) < H(left_idx)) left_idx = right_idx
      if (H(i) <= H(left_idx)) return
      swaps.add(Swap(i, left_idx))
      //      println("#k",k,"swaps",swaps)
      /*
(#k,1,swaps,[Swap(1,4)])
(#k,0,swaps,[Swap(1,4), Swap(0,1)])
(#k,0,swaps,[Swap(1,4), Swap(0,1), Swap(1,3)])
       */
      //// swap action //////
      val tmp: Int = H(i)
      H(i) = H(left_idx)
      H(left_idx) = tmp

      i = left_idx
    }
  }

  private def buildHeap{//BuildHeap(A[1 . . . n])
    swaps = new java.util.ArrayList[Swap]()
    for (i <- H.length / 2 to 0 by -1) {
      siftDown(i)
    }
  }


}
/*
5
5 4 3 2 1
3
1 4
0 1
1 3


Start

         5
       /   \
      4     3
     /\
    2  1
       5,4,3,2,1
  Step 1
       i = 1 left_idx = 2*i+1 = 3; H(3)-> 2 right_idx = 4; H(4)->1
       if(H(right_idx) < H(left_idx)left_idx = right_idx
       swap(i,left_idx)-->swap(1,4)
         5
       /   \
      1     3
     /\
    2  4
     5,1,3,2,4
   Step 2
       i = 0 left_idx = 1; H(1)-> 1 right_idx = 2; H(2)->3
       if(H(right_idx) < H(left_idx)left_idx = right_idx #YES
       swap(i,left_idx)-->swap(0,1)
         1
       /   \
      5     3
     /\
    2  4
       1,5,3,2,4
   Step 3
       i = 1 H(1)->5 ;;left_idx = 3; H(3)-> 2 right_idx = 4; H(4)->4
       if(H(right_idx) < H(left_idx)left_idx = right_idx #NO
       swap(i,left_idx)-->swap(1,3)
         1
       /   \
      2     3
     /\
    5  4
       1,2,3,5,4

   Step 4
       i = 1 H(1)->2 left_idx = 3 H(3)-> 3 right_idx =4 H(4)->5
        if (H(i) <= H(left_idx)) return #STOP
       already a heap, because a0 = 1 < 2 = a1,
                               a0 = 1 < 3 = a2,
                               a1 = 2 < 5 = a3,
                               a1 = 2 < 4 = a4.
       hence stop.
Good job! (Max time used: 2.00/3.00, max memory used: 83288064/536870912.)


*/

