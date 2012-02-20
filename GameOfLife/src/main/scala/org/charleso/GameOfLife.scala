package org.charleso

import scala.util.Random

object GameOfLife {

  def main(args: Array[String]) {
    run(40, 1000)
  }

  def run(size: Int, count: Int) {
    val u = new Universe(size)
    for (i <- loop(u).take(count)) {
      println(i)
      println("")
    }
  }

  def loop(universe: Universe): Stream[Universe] = universe #:: loop(universe.transition)

  /**
   * The universe of the Game of Life is an infinite two-dimensional orthogonal grid of square cells,
   * each of which is in one of two possible states, alive or dead.
   */
  case class Universe(private val cells: Vector[Vector[Cell]]) {

    def this(i: Int) = this(Vector.tabulate(i, i)(Cell.random))

    def map(f: Cell => Cell) = Universe(cells map (_ map (f)))

    def transition = {

      /**
       * Every cell interacts with its eight neighbours, which are the cells that are horizontally, vertically, or diagonally adjacent.
       */
      def neighbours(c: (Int, Int)) = {
        def abs(x: Int) = (x + cells.length) % cells.length
        def get(x: Int, y: Int) = cells(abs(x))(abs(y))
        val perms = { val l = List(0, -1, 1); for (x <- l; y <- l; if x != 0 || y != 0) yield (x, y) }
        perms.map { case (x, y) => get(c._1 + x, c._2 + y) }
      }

      def mapStateAndAliveCount(f: (State, Int) => State) = {
        map { c =>
          c.copy(state = f(c.state, neighbours(c.coords).map(_.state).count(!_.isDead)))
        }
      }

      /**
       * At each step in time, the following transitions occur:
       * Any live cell with fewer than two live neighbours dies, as if caused by under-population.
       * Any live cell with two or three live neighbours lives on to the next generation.
       * Any live cell with more than three live neighbours dies, as if by overcrowding.
       * Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
       */
      mapStateAndAliveCount {
        case (Alive, x) if x < 2 => Dead
        case (Alive, x) if x > 3 => Dead
        case (Dead, 3) => Alive
        case (s, _) => s
      }
    }

    override def toString = cells.map(_.map(_.state).mkString("")) mkString ("\n")
  }

  case class Cell(coords: (Int, Int), state: State)
  object Cell {
    def random(x: Int, y: Int) = Cell((x, y), State(Random.nextBoolean()))
  }
  sealed trait State {
    def isDead = this match { case Dead => true; case Alive => false }
  }
  object State {
    def apply(alive: Boolean) = if (alive) Alive else Dead
  }
  case object Alive extends State {
    override def toString = "*"
  }
  case object Dead extends State {
    override def toString = " "
  }
}