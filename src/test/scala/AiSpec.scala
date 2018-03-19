package bugs

import org.scalatest.{FlatSpec, Matchers}

class AiSpec extends FlatSpec with Matchers {
  it should "moves down given a rule and a map" in {
    val world = World(
      List(
        Piece(Empty, (0, 0)), Piece(Empty, (0, 1)), Piece(Empty, (0, 2)), Piece(Empty, (0, 3)), Piece(Empty, (0, 4)),
        Piece(Empty, (1, 0)), Piece(Empty, (1, 1)), Piece(Empty, (1, 2)), Piece(Empty, (1, 3)), Piece(Empty, (1, 4)),
        Piece(Empty, (2, 0)), Piece(Empty, (2, 1)), Piece(Player(100,100), (2, 2)), Piece(Empty, (2, 3)), Piece(Empty, (2, 4)),
        Piece(Empty, (3, 0)), Piece(Empty, (3, 1)), Piece(Food, (3, 2)), Piece(Empty, (3, 3)), Piece(Empty, (3, 4)),
        Piece(Empty, (4, 0)), Piece(Empty, (4, 1)), Piece(Empty, (4, 2)), Piece(Empty, (4, 3)), Piece(Empty, (4, 4))
      ),
      5
    )

    val ai = Ai("eeeeeeeeeeeepeeeefeeeeeeed")

    ai.generateMove(world) shouldBe Down
  }

  it should "moves up given a rule and a map" in {
    val world = World(
      List(
        Piece(Empty, (0, 0)), Piece(Empty, (0, 1)), Piece(Empty, (0, 2)), Piece(Empty, (0, 3)), Piece(Empty, (0, 4)),
        Piece(Empty, (1, 0)), Piece(Empty, (1, 1)), Piece(Food, (1, 2)), Piece(Empty, (1, 3)), Piece(Empty, (1, 4)),
        Piece(Empty, (2, 0)), Piece(Empty, (2, 1)), Piece(Player(100,100), (2, 2)), Piece(Empty, (2, 3)), Piece(Empty, (2, 4)),
        Piece(Empty, (3, 0)), Piece(Empty, (3, 1)), Piece(Empty, (3, 2)), Piece(Empty, (3, 3)), Piece(Empty, (3, 4)),
        Piece(Empty, (4, 0)), Piece(Empty, (4, 1)), Piece(Empty, (4, 2)), Piece(Empty, (4, 3)), Piece(Empty, (4, 4))
      ),
      5
    )

    val ai = Ai("eeeeeeefeeeepeeeeeeeeeeeeu")

    ai.generateMove(world) shouldBe Up
  }
}
