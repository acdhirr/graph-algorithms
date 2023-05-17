package paths1

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

object BFS {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)

    // read an undirected graph ------------------------
    val verticesCount = s.nextInt()
    val edgesCount = s.nextInt()
    val adjacencies = new Array[ArrayBuffer[Int]](verticesCount + 1) // 1-based

    for (i <- 1 until verticesCount + 1) // initialize an empty list for each vertex
      adjacencies(i) = new ArrayBuffer[Int]()

    for (_ <- 0 until edgesCount) {
      val v1 = s.nextInt()
      val v2 = s.nextInt()
      adjacencies(v1).append(v2)
      adjacencies(v2).append(v1)
    }
    // done reading a graph ----------------

    val start = s.nextInt()
    val end = s.nextInt()

    val hops = minHops(adjacencies, start, end)

    println(hops)
  }

  private def minHops(adjacencies: Array[ArrayBuffer[Int]], start: Int, end: Int): Int = {

      // initialize distances
      val distances: Array[Int] = Array.fill(adjacencies.length + 1)(-1) // 1-based
      distances(start) = 0

      // initialize a queue with the start node
      val queue = Queue[Int]()
      queue.enqueue(start)

      while (queue.nonEmpty) {

        val node = queue.dequeue()
        for (neighbour <- adjacencies(node))
          if (distances(neighbour) == -1) { // distance will be set only once, on first encounter!
            distances(neighbour) = distances(node) + 1 // distance between a node and its neighbour is 1
            queue.enqueue(neighbour) // save the neighbour to be explored later
          }
      }

      distances(end)
  }

}

/*

4 4
1 2
4 1
2 3
3 1
2 4
-> 2

5 4
5 2
1 3
3 4
1 4
3 5
-> -1

*/