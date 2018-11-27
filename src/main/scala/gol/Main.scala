package gol

import io.Source.fromFile

import GameOfLife.generation

object Main {
  def main(args: Array[String]): Unit = {
    val in = args(0)
    val culture = fromFile(in).getLines.map(_.map(_ == '1').toVector).toVector
    generation(culture).foreach { row =>
      println(row.map { alive =>
        if (alive) 1 else 0
      }.mkString)
    }
  }
}