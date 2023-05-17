package paths1

import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Bipartite {

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

    val bipartite = isBipartite(adjacencies)

    println(if(bipartite) 1 else 0)

  }

  private def isBipartite(adjacencies: Array[ArrayBuffer[Int]]): Boolean = {

    // initialize distances
    val colors: Array[Integer] = Array.fill(adjacencies.length + 1)(0) // 1-based

    // Disjoint regions must be ALL explored!
    // So here we record which nodes are still to be visited
    val visited: Array[Boolean] = Array.fill(adjacencies.length + 1)(false)

    for (node <- 1 until visited.length-1 if !visited(node)) {

      // initialize a queue with the start node
      val queue = mutable.Queue[Int]()
      colors(node) = 1
      queue.enqueue(node)

      while (queue.nonEmpty) {

        val node = queue.dequeue()
        visited(node) = true
        for (neighbour <- adjacencies(node)) {
          if (colors(neighbour) == colors(node)) return false
          else if (colors(neighbour) == 0) {
            colors(neighbour) = -colors(node) // flip color for neighbour of node
            queue.enqueue(neighbour) // save the neighbour to be explored later
          }
        }
      }
    }

    true
  }

}

/*

4 4
1 2          4   3
4 1           \ / \
2 3            1---2
3 1
-> 0

5 4
5 2          3---4   5
4 2             / \ /
3 4            1   2
1 4
-> 1

8 6
5 2          3---4   5    6   7
4 2             / \ /      \ /
3 4            1   2        7
1 4
6 7
7 8
-> 1

8 7
5 2          3---4   5    6---7
4 2             / \ /      \ /
3 4            1   2        7
1 4
6 7
7 8
6 8
-> 0


*/