package bugs

import org.scalatest.{FlatSpec, Matchers}

class WorldSpec extends FlatSpec with Matchers {
  it should "moves the player down in the world" in {
    val world = World.generateRandom(10)
    val player = Piece(Player(100, 100), (0, 0))
    val worldWithPlayer = world.withPlayer(player)

    val newWorld = worldWithPlayer.movePlayer(Down, player)

    newWorld._2.coordinates shouldBe((1, 0))

    val filteredPlayer: List[Piece] = newWorld._1.pieces.filter((piece: Piece) => piece.coordinates == (1, 0))

    filteredPlayer shouldBe List(newWorld._2)
  }
}
