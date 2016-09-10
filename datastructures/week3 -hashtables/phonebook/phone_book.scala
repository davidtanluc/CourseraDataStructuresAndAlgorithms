import java.util
import java.util.Scanner

/**
  * Created by davidtan on 9/10/16.
  */
object phone_book {

  def nextPrime (n: Int):Int= {
    var prime = n + 1
    while (!isPrime(prime)) prime +=1
    prime
  }

  def isPrime(n:Int):Boolean ={
    for(i<-2 to Math.sqrt(n).toInt;if n % i==0) return false
    true
  }

  // Cardinality of hash table
  val m = 101
  var a: Int = -1
  var b: Int = -1
  val random = scala.util.Random
  // Separate Chaining to occupy O(n+m) space
  private var hashtable :Array[util.ArrayList[Contact]] = null
  private val result = new util.ArrayList[String]
  val scanner = new Scanner(System.in)

  //Universal Hash Family.
  def hash (number: Int):Int  = (34 * number + 2) % 10000019 % m

  case class Query(_type:String,  name:String, number: Int=0)
  case class Contact(var name:String, number :Int)

  def PhoneBook() = hashtable = Array.fill(m)(new util.ArrayList[Contact])

  private def insert(query: Query) {
    val index = hash(query.number)
    import scala.collection.JavaConversions._
    //// if found ; update else new
    for (contact <- hashtable(index)) {
      if (contact.number == query.number) {
        contact.name = query.name
        return
      }
    }
    hashtable(index).add(new Contact(query.name, query.number))
  }

  private def remove(query: Query) {
    val index = hash(query.number)
    val iter = hashtable(index).iterator

    while (iter.hasNext) {
      val contact = iter.next
      if (contact.number == query.number) {
        iter.remove
        return
      }
    }
  }

  private def search(query: Query): String = {
    val index = hash(query.number)

    import scala.collection.JavaConversions._
    for (contact <- hashtable(index)) {
      if (contact.number == query.number) {
        return contact.name
      }
    }
    "not found"
  }

  def processQueries {
    val queryCount = scanner.nextInt
    for (i<- 0 until queryCount){
      processQuery(readQuery)
    }

  }

  private def readQuery: Query = {
    val _type = scanner.next
    val number = scanner.nextInt

    if (_type == "add") {
      val name = scanner.next
      Query(_type, name, number)
    }
    else {
      Query(_type,"" ,number)
    }
  }

  private def writeResponse(response: String) = println(response)

  def processQuery(query: Query) {
    if (query._type == "add") {
      insert(query)
    }
    else if (query._type == "del") {
      remove(query)
    }
    else { //search
    val response = search(query)
      writeResponse(response)
      if (result != null) result.add(response)
    }
  }

  def main(args: Array[String]) {

    PhoneBook()

    processQueries
  }
}
/*
10 September 2016
Good job! (Max time used: 4.56/9.00, max memory used: 119422976/671088640.)
12

add 911 police

add 76213 Mom

add 17239 Bob

find 76213

find 910

find 911

del 910

del 911

find 911

find 76213

add 76213 daddy

find 76213

//////
Output:


Mom

not found

police

not found

Mom

daddy

Explanation:


Sample 2.

Input:

8

find 3839442

add 123456 me

add 0 granny

find 0

find 123456

del 0

del 0

find 0

Output:

not found

granny

me

not found

Explanation:

Recall that deleting a number that doesn’t exist in the phone book doesn’t change anything.
 */