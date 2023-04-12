package decomposition2

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object TopologicalSort {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)

    // NB The given graph is guaranteed to be acyclic

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

    topologicalSort(adjacencies).foreach(x => printf(s"$x "))

  }

  def topologicalSort(graph: Array[ArrayBuffer[Int]]): Array[Int] = {

    // for all vertices set visited = false
    val visited = Array.fill[Boolean](graph.length)(false)
    val pre = Array.fill[Int](graph.length)(-1)
    val post = Array.fill[Int](graph.length)(-1)
    var counter = 0

    def _dfs(): Unit = {
      for (v <- 1 until graph.length)
        if (!visited(v))
          _explore(v)
    }

    def _explore(v: Int): Unit = {

      visited(v) = true
      previsit(v)
      val children = graph(v)

      for (j <- 0 until children.length) {
        val u = children(j)
        if (!visited(u)) _explore(u)
      }
      postvisit(v)
    }

    def previsit(v: Int) = {
      pre(v) = counter
      counter += 1
    }

    def postvisit(v: Int) = {
      post(v) = counter
      counter += 1
    }

    _dfs

    // smallest post value corresponds to a sink and comes at the end
    // order the graph indices by the corresponding postvisit values, smallest last
    val arq = new Array[(Int,Int)](graph.length)
    for (v <- 0 until graph.length)  arq(v) = (v,post(v))
    val r = arq.drop(1).sortBy(_._2).reverse.map(_._1)

    r

  }


}
