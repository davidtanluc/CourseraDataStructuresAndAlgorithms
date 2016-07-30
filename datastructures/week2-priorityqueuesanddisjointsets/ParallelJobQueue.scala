import java.util.{Comparator, PriorityQueue}

/**
  * Created by davidtan on 7/24/16.
  */

object ParallelJobQueue {
  private var numWorkers: Int = 0
  private var jobs: Array[Int] = null
  private var assignedWorker: Array[Int] = null
  private var startTime: Array[Long] = null

  def main(args: Array[String]) {
    val inputs1 = readLine.split(' ').map(_.toInt)
    numWorkers = inputs1(0) //num threads available
    val m = inputs1(1)
    jobs = readLine.split(' ').map(_.toInt)
    //assignJobs_naive
    assignJobs
    writeResponse

  }

  ///// helper functions ////////
  case class Worker(val id: Int = 0, var nextFreeTime: Long = 0)

  private def writeResponse {
    for (i <- jobs.indices) {
      println(assignedWorker(i) + " " + startTime(i))
    }

  }

  private def assignJobs {
    assignedWorker = new Array[Int](jobs.length)
    startTime = new Array[Long](jobs.length)

    val pq: PriorityQueue[Worker] = new PriorityQueue[Worker](numWorkers, new Comparator[Worker]() {
      def compare(w1: Worker, w2: Worker): Int = if (w1.nextFreeTime == w2.nextFreeTime) w1.id - w2.id else (w1.nextFreeTime - w2.nextFreeTime).toInt
    })
    // Push all new threads with initial free time 0
    for (i <- 0 until numWorkers) {
      pq.offer(Worker(i))
    }
    // Process each job by the 1st free thread
    for (i <- jobs.indices) {
      val freeThread = pq.poll() //remove head
      // Record job i's assigned worker and start_time
      assignedWorker(i) = freeThread.id
      startTime(i) = freeThread.nextFreeTime
      // Update next free time and offer back
      freeThread.nextFreeTime += jobs(i)
      pq.offer(freeThread)
      // This thread will be sorted again according to
      // its next free time, by next job to be processed
    }

  }
/*
2 5
1 2 3 4 5
0 0
1 0
0 1
1 2
0 4
 */
  private def assignJobs_naive {
    assignedWorker = new Array[Int](jobs.length)
    startTime = new Array[Long](jobs.length)
    val nextFreeTime: Array[Long] = new Array[Long](numWorkers)
    for (i <- jobs.indices) {
      var duration = jobs(i)
      var bestWorker = 0
      for (j <- 0 until numWorkers) {
        if (nextFreeTime(j) < nextFreeTime(bestWorker))
          bestWorker = j
      }
      assignedWorker(i) = bestWorker
      startTime(i) = nextFreeTime(bestWorker)
      nextFreeTime(bestWorker) += duration
    }

  }
}

/*
2 5
1 2 3 4 5
0 0
1 0
0 1
1 2
0 4
24 July 2016 at 5:51 PM
Good job! (Max time used: 2.28/6.00, max memory used: 79626240/536870912.)
 */