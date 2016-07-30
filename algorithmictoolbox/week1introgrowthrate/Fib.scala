import java.util.Scanner

/**
  * Created by davidtan on 7/15/16.
  */
object Fib {
  def calc_fib_naive(n : Int) : Long ={
    if(n <=1)return n
    calc_fib_naive(n-1)+calc_fib_naive(n-2)
  }

  def calc_fib(n : Int) : Long ={
    if(n==0)return 0
    if(n<3)return 1

    val A = Array.fill(n+1)(0)
    A(1)=1
    A(2)=1
    for(i<- 3 to n){
      A(i)= A(i-1) + A(i-2)
    }
    A(n)
  }

  def main(args: Array[String]) {
    val s: Scanner = new Scanner(System.in)
    val a: Int = s.nextInt

    //println(calc_fib_naive(a))
    println(calc_fib(a))
  }

}
/*
30 July 2016 at 2:45 PM

Good job! (Max time used: 0.61/3.00, max memory used: 38297600/536870912.)
1
1

5
5

10
55
 */