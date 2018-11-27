package gol

import math.{ min, max }

object GameOfLife {
  def generation(culture: Vector[Vector[Boolean]]): Vector[Vector[Boolean]] = {
    val nRows = culture.size
    val nCols = if (nRows > 0) culture(0).size else 0

    def nextGen(i: Int, j: Int) = {
      val alive = culture(i)(j)
      val nNeighbors = (neigborStart(i) until neighborEnd(i, nRows)).foldLeft(0) { (acc, neighorRowI) =>
        val nAliveInRow = culture(neighorRowI).slice(neigborStart(j), neighborEnd(j, nCols)).count(identity)
        acc + nAliveInRow
      } - (if (alive) 1 else 0)
      nextLife(nNeighbors, alive)
    }

    Vector.tabulate(nRows, nCols)(nextGen)
  }
  
  private def neigborStart(k: Int) = max(0, k - 1)
  
  private def neighborEnd(k: Int, n: Int) = min(n, k + 2)

  private def nextLife(nNeighbors: Int, alive: Boolean) =
    if (alive) nNeighbors == 2 || nNeighbors == 3
    else nNeighbors == 3
}