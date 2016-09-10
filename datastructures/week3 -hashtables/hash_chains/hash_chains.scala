import java.io.IOException
import java.util
import java.util.Scanner

/**
  * Created by davidtan on 9/10/16.
  */
object hash_chains {

  private var elems: util.ArrayList[String] = null
  private var bucketCount: Int = 0
  private val prime: Int = 1000000007
  private val multiplier: Int = 263
  val scanner = new Scanner(System.in)


  case class Query(t1:String, t2: String) {
    var _type = t1
    var ind = 0
    var s = ""
    if (_type == "check") ind = t2.toInt else s = t2

  }

  private def hashFunc2(s: String): Int = {
    var hash: Long = 0
    for (i <- s.length-1 to 0 by -1){
      hash = (hash * multiplier + s.charAt(i)) % prime
    }
    hash.toInt % bucketCount
  }
  def hashFunc(s:String):Int ={
    //// PolyHash ////////
    ((for(i <- s.indices) yield s.charAt(i)* Math.pow(multiplier,i)).sum % prime % bucketCount).toInt
  }

  private def readQuery: Query = {
    val _type: String = scanner.next
    val _type2: String = scanner.next
    Query(_type, _type2)
  }

  private def writeSearchResult(wasFound: Boolean) = println(if (wasFound)"yes" else "no")


  private def processQuery(query: Query) {

    query._type match {

      case "add" => if (!elems.contains(query.s)) elems.add(query.s)

      case "del" => if (elems.contains(query.s)) elems.remove(query.s)

      case "find" => writeSearchResult(elems.contains(query.s))

      case "check" =>
        import scala.collection.JavaConversions._
        val tmp = new util.ArrayList[String]()
        for (cur <- elems; if hashFunc(cur) == query.ind) tmp.add(cur)
        for(el <-tmp.toArray.reverse.toList)print(el+" ")
        println

      case _ => throw new RuntimeException("Unknown query: " + query._type)
    }
  }

  def processQueries():Unit = {
    elems = new util.ArrayList[String]
    bucketCount = scanner.nextInt
    val queryCount = scanner.nextInt
    for (i<-0 until queryCount) processQuery(readQuery)

  }

  def main(args: Array[String]) {
    processQueries()
  }
}
/*
Problem Introduction
h(‘‘world") = (119 + 111×263 + 114×263^2 + 108×263^3 + 100×263^4 mod 1 000 000 007) mod 5 = 4.

In this problem you will implement a hash table using the chaining scheme.


////
Input Format. There is a single integer m in the first line — the number of buckets you should have. The

next line contains the number of queries N. It’s followed by N lines, each of them contains one query

in the format described above.


Sample 1.

Input:

5

12

add world

add HellO

check 4

find World

find world

del world

check 4

del HellO

add luck

add GooD

check 2

del good

///// expected result
Correct output:
HellO world
no
yes
HellO
GooD luck



Sample 2.

Input:

4

8

add test

add test

find test

del test

find test

find Test

add Test

find Test


//////
Output:

yes

no

no

yes
*/
