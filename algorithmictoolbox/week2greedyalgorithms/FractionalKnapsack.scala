import java.util
import java.util.Scanner

/**
  * Created by davidtan on 8/6/16.
  */
object FractionalKnapsack {

  class Item protected (val value:Int, val weight:Int,val value_per_unit:Double)
  object Item {
    def apply(value:Int,weight: Int) = {
      //val pp1 = Math.round(value.toDouble / weight.toDouble * 10000.0) / 10000.0
      val pp1 =value.toDouble / weight.toDouble
      new Item(value,weight,pp1)
    }
  }

  private def getOptimalValue (capacity: Int, values: Array[Int], weights: Array[Int]) :Double= {
    var value = 0.00
    val items = new Array[Item](values.length)
    var capacity1 = capacity
    for (i<-items.indices) items(i) = Item(values(i),weights(i))

    val items2 = items.sortWith(_.value_per_unit>_.value_per_unit)

    var i = 0
    while (i < items2.length && capacity1 > 0) {
      val fraction: Int = items2(i).weight min capacity1  // a <- min (wi, W)

      value += items2(i).value_per_unit * fraction // V <- V + a(vi/wi)
      capacity1 -= fraction
      i += 1
    }
    value
  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n: Int = scanner.nextInt
    val capacity: Int = scanner.nextInt
    val values: Array[Int] = new Array[Int](n)
    val weights: Array[Int] = new Array[Int](n)
    for(i<-0 until n){
      values(i) = scanner.nextInt()
      weights(i) = scanner.nextInt()
    }

    println(getOptimalValue(capacity, values, weights))
  }
}
/*
6 August 2016 at 4:48 PM
Good job! (Max time used: 0.76/3.00, max memory used: 47988736/671088640.)

3,2,4

3 50

60 20

100 50

120 30

180.0

///////////
1 10

500 30
166.66699999999997
 */