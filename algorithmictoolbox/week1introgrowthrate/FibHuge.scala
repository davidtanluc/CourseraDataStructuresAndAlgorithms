import java.util.Scanner

import scala.annotation.tailrec

/**
  * Created by davidtan on 7/15/16.
  */
object FibHuge {
  /**
    * F [ i ]=( F [i -1]+ F [i -2]) mod 10
    *        = F [i -1] mod 10 + F [i -2] mod 10
    * @param n input with n size
    * @return the last digit
    */
  private def getFibonacciLastDigit(n: Long): Long = {
    @tailrec
    def loop1(next:Long,result:Long,acc:Long):Long ={
      if(next==n)return result
      loop1(next+1,acc % 10,acc %10 +result%10)
    }
    loop1(0,0,1)
  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val n: Long = scanner.nextLong
    //val m: Long = scanner.nextLong
    println(getFibonacciLastDigit(n))
  }

}
/*
Good job! (Max time used: 0.61/3.00, max memory used: 38211584/536870912.)

 */