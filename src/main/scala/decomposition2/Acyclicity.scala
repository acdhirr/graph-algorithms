package decomposition2

import java.util.Stack
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object Acyclicity {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)

    // read a DIRECTED graph ------------------------
    val verticesCount = s.nextInt()
    val edgesCount = s.nextInt()
    val adjacencies = new Array[ArrayBuffer[Int]](verticesCount + 1) // 1-based

    for (i <- 1 until verticesCount + 1) // initialize an empty list for each vertex
      adjacencies(i) = new ArrayBuffer[Int]()

    for (_ <- 0 until edgesCount) {
      val v1 = s.nextInt()
      val v2 = s.nextInt()
      adjacencies(v1).append(v2)
      // Directed, so no back edges
    }
    // done reading a directed graph ----------------

    if (hasCycles(adjacencies)) println(1) else println(0)
  }

  def hasCycles(graph: Array[ArrayBuffer[Int]]): Boolean = {

    // for all vertices set visited = false
    val visited = Array.fill[Boolean](graph.length)(false)
    var hasCycles = false

    def _dfs(): Unit = {
      for (v <- 1 until graph.length)
        if (!visited(v) && graph(v) != null ) {
          val stack = new Stack[Int]()
          _explore(v, stack)
        }
    }

    def _explore(v: Int, stack: Stack[Int]): Unit = {

      visited(v) = true
      stack.push(v) // keep track of the current path
      val children = graph(v)

      for (j <- 0 until children.length) {
        val u = children(j)
        if (stack.contains(u)) hasCycles = true // there's a back edge!
        if (!visited(u)) _explore(u, stack)
      }
      stack.pop()
    }

    _dfs()
    hasCycles
  }

  /*
    4 4
    1 2
    4 1
    2 3
    3 1
    -> 1

    5 7
    1 2
    2 3
    1 3
    3 4
    1 4
    2 5
    3 5
    -> 0

  */
}