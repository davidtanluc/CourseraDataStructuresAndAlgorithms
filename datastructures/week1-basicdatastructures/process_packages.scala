import java.io._
import java.util
import java.util._
/**
  * Created by davidtan on 7/22/16.
  */


/**
  * Network packet processing simulation.
  * Line 1
  * Buffer size, and incoming size
  * Line 2 ... to i th arrival_time, processing_time
  */

case class Request (val arrival_time: Int=0, val process_time: Int=0)
case class Response (val dropped: Boolean, val start_time: Int=0)

class Buffer {
  def this(size: Int) {
    this()
    this.size_ = size
    this.finish_time_ = new util.ArrayList[Integer]()
  }

  /**
    * 1) Check if finish_time_ if not empty and arrival_time >= this time remove this
    * 2) if finish_time_ is full drop this incoming
    * 3) find new current_start_time,if finish_time not empty get tail value
    * 4) add this new current_start_time + process_time
    * 5) set this process as false(not dropped) with the current_start time
    * 6) move to the next ith line incoming data
    * 7) repeat until the end of incoming data
    */
  def Process(request: Request): Response = {
    while (!finish_time_.isEmpty && finish_time_.get(0) <= request.arrival_time) {
      finish_time_.remove(0)
    }
    if (finish_time_.size == size_) {
      return Response(true, -1)
    }
    val current_start_time: Int = if (finish_time_.isEmpty) request.arrival_time
    else finish_time_.get(finish_time_.size - 1)

    finish_time_.add(current_start_time + request.process_time)
    Response(false, current_start_time)
  }

  private var size_ : Int = 0
  private var finish_time_ : util.ArrayList[Integer] = null
}

object process_packages {
  @throws(classOf[IOException])
  private def ReadQueries(scanner: Scanner): util.ArrayList[Request] = {
    val requests_count: Int = scanner.nextInt
    val requests: util.ArrayList[Request] = new util.ArrayList[Request]()

    for(i<-0 until requests_count){
      val arrival_time = scanner.nextInt()
      val process_time = scanner.nextInt()
      requests.add(Request(arrival_time, process_time))
    }
    requests
  }

  private def ProcessRequests(requests: util.ArrayList[Request], buffer: Buffer): util.ArrayList[Response] = {
    val responses: util.ArrayList[Response] = new util.ArrayList[Response]
    for(i<-0 until requests.size){
      responses.add(buffer.Process(requests.get(i)))
    }
    responses
  }

  private def PrintResponses(responses: util.ArrayList[Response]) {
    for(i<-0 until responses.size){
      val response = responses.get(i)
      if (response.dropped) {
        println(-1)
      } else {
        println(response.start_time)
      }
    }
  }

  def main(args: Array[String]) {
    val scanner: Scanner = new Scanner(System.in)

    val buffer_max_size: Int = scanner.nextInt
    val buffer: Buffer = new Buffer(buffer_max_size)

    val requests: util.ArrayList[Request] = ReadQueries(scanner)
    val responses: util.ArrayList[Response] = ProcessRequests(requests, buffer)
    PrintResponses(responses)

  }


}

/*
1 1
0 0
0

1 2
0 1
0 1
0
-1


1 2
0 1
1 1
0
1
22 July 2016 at 8:34 AM
Good job! (Max time used: 2.33/6.00, max memory used: 96120832/536870912.)
 */