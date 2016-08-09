import java.util.Scanner

/**
  * Created by davidtan on 8/8/16.
  */
object Edit_Distance {

  def editDistance (s: String, t: String):Int= {
    val m = s.length
    val n = t.length
    //  "" with the other string
    val table = Array.ofDim[Int](m + 1, n + 1)
    // initialize first row/col by i/j as distance from ""
    for(j<-1 to n) table(0)(j) = j
    for(i<-1 to m) table(i)(0) = i
        // bottom-up from all smaller sub problems row-by-row
    for(i<- 1 to m) {
      for(j<-1 to n) {
        val insertion = table(i)(j - 1) + 1
        val deletion = table(i - 1)(j) + 1
        val mismatch = table(i - 1)(j - 1) + (if (s.charAt(i - 1) == t.charAt(j - 1)) 0 else 1)
        table(i)(j) = insertion min (deletion min mismatch)
      }}
   table(m)(n)
  }

  def main(args: Array[String]) {
    val scan: Scanner = new Scanner(System.in)
    val s: String = scan.next
    val t: String = scan.next
    println(editDistance(s, t))
  }

}
/*
ab

ab

0


short
ports
3

8 August 2016
Good job! (Max time used: 0.59/3.00, max memory used: 38440960/536870912.)

 */