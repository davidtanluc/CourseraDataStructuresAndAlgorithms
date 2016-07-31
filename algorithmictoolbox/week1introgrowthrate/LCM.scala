import java.util.Scanner

/**
  * Created by davidtan on 7/15/16.
  */
object LCM {
  private def lcm_naive(a: Int, b: Int): Long = {
    a * b
  }
  private def gcd(a:Long,b:Long):Long=  if(a%b ==0)b else gcd(b,a%b)

  private def lcm(a:Long,b:Long):Long= a*b / gcd(a,b)

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val a: Int = scanner.nextInt
    val b: Int = scanner.nextInt
    //System.out.println(lcm_naive(a, b))
    System.out.println(lcm(a, b))
  }

}


/*
14159572 63967072
226436590403296
 */