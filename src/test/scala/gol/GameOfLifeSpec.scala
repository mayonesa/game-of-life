package gol

import org.scalatest.FlatSpec

import GameOfLife.generation

class GameOfLifeSpec extends FlatSpec {
  "1x1 alive" should "return 1x1 dead" in {
    assert(generation(Vector(Vector(true))) === Vector(Vector(false)))
  }
  "1x1 dead" should "return 1x1 dead" in {
    assert(generation(Vector(Vector(false))) === Vector(Vector(false)))
  }
  "empty" should "return empty" in {
    assert(generation(Vector.empty) === Vector.empty)
  }
  "if alive, 2 neighbors and, if dead, 3 neighbors" should "return alives" in {
    assert(generation(Vector(Vector(true, true), Vector(true, false))) === Vector(Vector(true, true), Vector(true, true)))
  }
  "if alive, 3 neighbors" should "return alives" in {
    assert(generation(Vector(Vector(true, true), Vector(true, true))) === Vector(Vector(true, true), Vector(true, true)))
  }
  "if alive, 4 neighbors" should "return dead" in {
    assert(generation(Vector(Vector(true, true, true), Vector(true, true, true))) === Vector(Vector(true, false, true), Vector(true, false, true)))
  }
}