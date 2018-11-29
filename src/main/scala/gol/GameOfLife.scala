package gol

import math.{ min, max }

object GameOfLife {
  def generation(culture: Vector[Vector[Boolean]]): Vector[Vector[Boolean]] = {
    val nRows = culture.size
    val nCols = if (nRows > 0) culture(0).size else 0

    def nextLife(i: Int, j: Int) = {
      val alive = culture(i)(j)
      val nAlive = (neigborStart(i) until neighborEnd(i, nRows)).foldLeft(0) { (acc, neighorRowI) =>
        val row = culture(neighorRowI)
        val neighborRow = row.slice(neigborStart(j), neighborEnd(j, nCols))
        val nAliveInNeighborRow = neighborRow.count(identity)
        acc + nAliveInNeighborRow
      }

      // don't count yourself
      val nNeighbors = if (alive) nAlive - 1 else nAlive

      nextLifeFrom(nNeighbors, alive)
    }

    Vector.tabulate(nRows, nCols)(nextLife)
  }

  private def neigborStart(k: Int) = max(0, k - 1)

  private def neighborEnd(k: Int, n: Int) = min(n, k + 2)

  private def nextLifeFrom(nNeighbors: Int, alive: Boolean) =
    if (alive) nNeighbors == 2 || nNeighbors == 3
    else nNeighbors == 3
}