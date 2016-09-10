/**
  * Created by davidtan on 9/7/16.
  */
object rabin_karp {

  def ord(c: Char): Int = {
    var res = 0
    var i = 0
    var mul = 1

    while (i >= 0) {
      res += c * mul
      mul *= 31
      i -= 1
    }
    //
    res
  }

  case class RollingHash(string:String,size:Int) {

    var str = string
    var hash = 0

    for(i<- 0 until size) hash += ord(str(i))

    var init = 0
    var end = size


    def update(): Unit = {

      if (end <= str.length - 1){

        hash -= ord(str(init))
        hash += ord(str(end))
        init += 1
        end += 1

      }

    }

    def digest = hash

    def text = str.substring(init,end)

  }

//////////////////////

  //println("HASH",hashP.hash)//292
  //println(hashT.hash)//292
  def get_occurrences(pattern:String, text:String): List[Int] = {

    val p = pattern.length
    val t = text.length
    val hashP = RollingHash(pattern, p)
    val hashT = RollingHash(text, p)

    val l1 = (0 until t-p+1).toList

    def loop(l1: List[Int], result: List[Int], hashUpdate: Unit):List[Int] = l1 match {
      case Nil =>result
      case(x::xs1) => if(hashP.digest == hashT.digest && hashT.text == pattern)loop(xs1,x::result,hashT.update())
      else loop(xs1,result,hashT.update())
    }

    loop(l1,List(),Unit)

  }

  import java.util.Scanner
  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val pattern = scanner.next
    val text = scanner.next
    val result =  get_occurrences(pattern,text).reverse
    for(a <- result)print(a+" ")
    println()
  }


}

/*

Sample 1.

Input:

aba

abacaba

Output:

0 4

Explanation:

The pattern aba can be found in positions 0 (abacaba) and 4 (abacaba) of the text abacaba.

Sample 2.

Input:

Test

testTesttesT

Output:

4

Explanation:

Pattern and text are case-sensitive in this problem. Pattern T est can only be found in position 4 in

the text testT esttesT.

Sample 3.

Input:

aaaaa

baaaaaaa

Output:

1 2 3

Explanation:
*/
