package paths2

import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Dijkstra {

  // Entry (edge) in an adjacency list (node + weight)
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
      val weight  = s.nextInt()
      adjacencies(node).append(new EdgeTo(end,weight))
      // Directed, so no back edges
    }
    // done reading a directed graph ----------------

    // read start end end point
    val from: Int = s.nextInt()
    val to: Int = s.nextInt()

    val pathLength: Long = dijkstra(adjacencies, from, to)

    if (pathLength != Long.MaxValue)
      println( pathLength )
    else println(-1)

  }

  private def dijkstra(graph: Array[ArrayBuffer[EdgeTo]], start: Int, end: Int): Long = {

    // prev property not used for this application
    class Node(val name:Int, var minDist: Long, var prev: Int) {
      override def toString = s"Node(name=$name, minDist=$minDist, prev=$prev)"
    }

    // All nodes are unvisited
    val unvisited = mutable.Set[Int]()
    for (i <- 1 until graph.length ) unvisited.add(i)

    val nodes = new Array[Node](graph.length)
    // Ordering is done on minDist (smallest first)
    val queue = mutable.PriorityQueue.empty[Node](Ordering.by(node => -node.minDist))

    // Start node has distance 0, all others infinity
    for (i <- 1 until graph.length if i != start) {
      nodes(i) = new Node(i, Long.MaxValue, -1)
      queue.enqueue(nodes(i))
    }
    nodes(start) = new Node(start, 0, -1)
    queue.enqueue(nodes(start))

    // Dijkstra
    while ( unvisited.nonEmpty && queue.nonEmpty ) {

      val curNode: Node = queue.dequeue()
      // println(curNode)
      // must be reachable from start node!
      if (curNode.minDist != Long.MaxValue) {
        for (edge: EdgeTo <- graph(curNode.name) ) {
          val dist: Long = curNode.minDist + edge.weight
          val neighbour: Int = edge.end
          if ( dist < nodes(neighbour).minDist) {
            nodes(neighbour).minDist = dist
            nodes(neighbour).prev = curNode.name
            // Just add to the queue anew (though the old value lingers somewhere deep down in the queue)
            queue.enqueue(nodes(neighbour))
          }
          unvisited.remove(curNode.name)
        }
      }
    }

    nodes(end).minDist
  }

}

/*

2 0
1 2
-> -1

2 1
1 2 3
1 2
-> 3

4 4
1 2 1
4 1 2
2 3 2
1 3 5
1 3
-> 3

5 9
1 2 4
1 3 2
2 3 2
3 2 1
2 4 2
3 5 4
5 4 1
2 5 3
3 4 4
1 5
-> 6

3 3
1 2 7
1 3 5
2 3 2
3 2
-> -1

*/