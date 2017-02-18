package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Bloxorz._

import scala.collection.convert.WrapAsJava.seqAsJavaList

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level2 extends SolutionChecker {
      /* terrain for level 2*/
    val level =
    """ooo-------
      |oooooo----
      |oooSooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
  }

  test("newNeighborsOnly"){
    new Level1 {
      val newNeighbors = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      )

      val result = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      println(result.toString())
      println(newNeighbors.toString())

      assert(newNeighbors.toString() == result.toString())
    }
  }

  test("pathsFromStart"){
    new Level1{
      val t = pathsFromStart
      println(t.toString())
    }
  }

  test("legalNeighbors"){
    new Level1{
      val t = startBlock.legalNeighbors
      assert(t.toString() == "List((Block(Pos(1,2),Pos(1,3)),Right), (Block(Pos(2,1),Pos(3,1)),Down))")
    }
  }


  test("neighborsWithHistory"){
    new Level1{
      val streamResult = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      val setResult = streamResult.toSet
      assert(setResult.toString() == "Set((Block(Pos(1,2),Pos(1,3)),List(Right, Left, Up)), " +
                                         "(Block(Pos(2,1),Pos(3,1)),List(Down, Left, Up)))")
    }
  }

  test("Check Pos and terrainFunction for -"){
    new Level1 {
      assert(false == terrain(Pos(0,3)))
    }
  }

  test("Check Pos and terrainFunction for o"){
    new Level1 {
      assert(true == terrain(Pos(5,7)))
    }
  }

  test("Find different Start pos"){
    new Level2 {
      assert(startPos == Pos(2, 3))
    }
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
