import java.util
import java.util.Scanner

import scala.annotation.tailrec

/**
  * Created by davidtan on 8/3/16.
  */
object Change {
  private def getChange_naive(amount: Int): util.ArrayList[Int] = {
    val coins = List(1,5,10)
    val change = new util.ArrayList[Int]
    var total = 0
    for (i<-coins.length-1 to 0 by -1){
      var coin = coins(i)
      while (total + coin <= amount) {
        change.add(coin)
        total += coin
      }
    }
    change
  }

  private def getChange4(amount: Int) = {
    var amount1 = amount
    val coins = List(1,5,10)
    val result = new util.ArrayList[(Int,Int)]
    var total = 0
    for (i<-coins.length-1 to 0 by -1){
      result.add((coins(i),amount1/coins(i)))
      amount1 = amount1 % coins(i)

    }
    result
  }
  /*
  def greedyCoinChanging(M, k):
    n = len(M)
    result = []
    for i in xrange(n - 1, -1, -1):
        result += [(M[i], k // M[i])]
        k %= M[i]
    return result
   */

  private def getChange3(amount: Int) = {
    var amount1 = amount
    val coins = List(1,5,10)
    var total = 0
    for (i<-coins.length-1 to 0 by -1){
      total += amount1/coins(i)
      amount1 = amount1 % coins(i)
    }
    total
  }

  private def getChange(amount: Int): Int = {
    val coins1 = List(1,5,10).sortWith(_>_)
    @tailrec
    def loop(money:Int,result:List[Int],coins:List[Int]):Int= {
      //println(money+" ",result)
/*
28
(0 ,List())
(10 ,List(10))
(20 ,List(10, 10))
(20 ,List(10, 10))
(25 ,List(5, 10, 10))
(25 ,List(5, 10, 10))
(26 ,List(1, 5, 10, 10))
(27 ,List(1, 1, 5, 10, 10))
(28 ,List(1, 1, 1, 5, 10, 10))
6
 */
      if(coins.isEmpty || money==amount) return result.size
      val total = money + coins.head
      if(total<= amount)loop(total,coins.head :: result, coins) else loop(money,result, coins.tail) //skip

    }
    loop(0,List(),coins1)

  }

  private def getChange2(amount: Int): Int = {
    val coins1 = List(1,5,10).sortWith(_>_)

    @tailrec
    def loop(money:Int,result:List[Int],coins:List[Int]):Int= {
      //println(money+" ",result)
/*
28
(0 ,List())
(10 ,List(10))
(20 ,List(10, 10))
(20 ,List(10, 10))
(25 ,List(5, 10, 10))
(25 ,List(5, 10, 10))
(26 ,List(1, 5, 10, 10))
(27 ,List(1, 1, 5, 10, 10))
(28 ,List(1, 1, 1, 5, 10, 10))
6
 */
      if(coins.isEmpty || money==amount) return result.size
      val total = money + coins.head

      total match {
        case x if x<=amount =>loop(total,coins.head :: result, coins)
        case y =>  loop(money,result, coins.tail) //skip
      }

    }
    loop(0,List(),coins1)

  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n: Int = scanner.nextInt
    //println(getChange_naive(n))
   // println(getChange(n))
    //println(getChange2(n))
    println(getChange3(n))
    //println(getChange4(n))
  }
}
/*
 August 2016 at 3:26 PM
Good job! (Max time used: 0.60/3.00, max memory used: 38215680/536870912.)
28
6



*/