package paths2

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object NegativeCycle {

  private class EdgeTo(val end: Int, var weight: Int)

  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    // read a DIRECTED graph ------------------------
    val nodesCount = s.nextInt()
    val edgesCount = s.nextInt()
    val adjacencies = new Array[ArrayBuffer[EdgeTo]](nodesCount + 1) // 1-based

    for (i <- 1 until nodesCount + 1) // initialize an empty list for each vertex
      adjacencies(i) = new ArrayBuffer[EdgeTo]()

    for (_ <- 0 until edgesCount) {
      val node = s.nextInt()
      val end = s.nextInt()
      val weight = s.nextInt()
      adjacencies(node).append(new EdgeTo(end, weight))
      // Directed, so no back edges
    }
    // done reading a directed graph ----------------

    println( if (hasCycle( adjacencies )) 1 else 0 )

  }

  /**
   * Test if there is a cycle. ( Dasgupta 4.6.2 p119 )
   * */
  private def hasCycle(graph: Array[ArrayBuffer[EdgeTo]]): Boolean = {

    val distances = new Array[Long](graph.length)
    for (i <- 1 until graph.length if i != 1) distances(i) = 100000
    distances(1) = 0 // start node

    var isUpdated = false
    // Repeat |graph|-1 times, plus one round extra (makes |graph| times).
    // If in this final round still nodes are updated, we have a cycle.
    // (Dasgupta 4.6.2, p 119)
    for (_ <- 1 until graph.length) {

      isUpdated = false

      for (i <- 1 until graph.length) {
        for (edge: EdgeTo <- graph(i)) {
          val neighbour = edge.end
          val newDist = distances(i) + edge.weight
          if (newDist < distances(neighbour)) {
            distances(neighbour) = newDist
            isUpdated = true
          }
        }
      }
    }

    isUpdated
  }

}

/*

4 4
1 2 5
4 1 2
2 3 2
3 1 1
-> 0

4 4
1 2 -5
4 1 2
2 3 2
3 1 1
-> 1

10 9
1 2 1
5 6 1
6 7 1
8 9 1
9 10 1
3 4 1
7 8 1
4 5 1
2 3 1
-> 0

*/