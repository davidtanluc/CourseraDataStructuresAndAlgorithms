package week3knorris

import java.util
import java.util.Scanner

/**
  * Created by davidtan on 9/5/16.
  */
object KMP {

  private def computePrefixFunction(pattern: String): Array[Int] = {
    val s = new Array[Int](pattern.length)
    s(0) = 0
    var border = 0

    for (i <- 1 until pattern.length) {

      while (border > 0 && pattern(i) != pattern(border)) border = s(border - 1)

      if (pattern(i) == pattern(border)) border = border + 1 else border = 0

      s(i) = border

    }
    //
    s
  }

  private def findAllOccurances(pattern: String, text: String):List[Int] = {

    val s = pattern + "$" + text
    val prefix = computePrefixFunction(s)

    (for(i <- pattern.length + 1 until s.length
          if prefix(i) == pattern.length) yield i - 2 * pattern.length).toList

  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)
    val pattern = scanner.next
    val text = scanner.next
    val result =  findAllOccurances(pattern, text)
    for(a <- result)print(a+" ")
    println()

  }

  //
}
/*
Good job! (Max time used: 9.65/24.00, max memory used: 164761600/536870912.)
Input:

ATA

ATATA


0 2

/////
ATAT

GATATATGCATATACTT

1 3 9
 */
