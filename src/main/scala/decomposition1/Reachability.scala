package decomposition1

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object Reachability {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)

    // read an undirected graph ------------------------
    val verticesCount = s.nextInt()
    val edgesCount = s.nextInt()
    val adjacencies = new Array[ArrayBuffer[Int]](verticesCount+1) // 1-based

    for (i <- 1 until verticesCount+1) // initialize an empty list for each vertex
      adjacencies(i) = new ArrayBuffer[Int]()

    for (_ <- 0 until edgesCount) {
      val v1 = s.nextInt()
      val v2 = s.nextInt()
      adjacencies(v1).append(v2)
      adjacencies(v2).append(v1)
    }
    // done reading a graph ----------------

    // nodes for which we want to establish a path
    val u = s.nextInt()
    val v = s.nextInt()

    // print graph
    // for (i <- 1 until verticesCount+1) println(s"$i -> ${adjacencies(i)}")

    val connected = explore(adjacencies, u, v)

    println( if(connected) 1 else 0 )
  }

  def explore(adjacencies: Array[ArrayBuffer[Int]], node: Int, target: Int): Boolean = {

    // nodes already checked (prepopulate with false)
    val checked = Array.fill[Boolean](adjacencies.size+1)(false)

    def _explore(node: Int): Boolean = {

      var found = false
      if (node == target) found = true
      else for (neighbour <- adjacencies(node) if checked(neighbour) == false) {
        checked(node) = true
        if (_explore(neighbour)) found = true
      }

      found
    }

    _explore(node)
  }

  /*

4 4
1 2
3 2
4 3
1 4
1 4
-> 1

4 2
1 2
3 2
1 4
-> 0

4 3
1 2
3 2
4 3
1 4
-> 1

  */

}
