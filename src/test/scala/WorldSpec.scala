package bugs

import org.scalatest.{FlatSpec, Matchers}

class WorldSpec extends FlatSpec with Matchers {
  val world: World = World.generateRandom(10)
  val player: Piece = Piece(Player(100, 100), (5, 5))
  val worldWithPlayer: World = world.withPlayer(player)

  it should "moves the player down in the world" in {
    val newWorld = worldWithPlayer.movePlayer(Down, player)

    newWorld._2.coordinates shouldBe((6, 5))

    val filteredPlayer: List[Piece] = newWorld._1.pieces.filter((piece: Piece) => piece.coordinates == (6, 5))

    filteredPlayer shouldBe List(newWorld._2)
  }

  it should "moves the player up in the world" in {
    val newWorld = worldWithPlayer.movePlayer(Up, player)

    newWorld._2.coordinates shouldBe((4, 5))

    val filteredPlayer: List[Piece] = newWorld._1.pieces.filter((piece: Piece) => piece.coordinates == (4, 5))

    filteredPlayer shouldBe List(newWorld._2)
  }

  it should "moves the player left in the world" in {
    val newWorld = worldWithPlayer.movePlayer(Left, player)

    newWorld._2.coordinates shouldBe((5, 4))

    val filteredPlayer: List[Piece] = newWorld._1.pieces.filter((piece: Piece) => piece.coordinates == (5, 4))

    filteredPlayer shouldBe List(newWorld._2)
  }

  it should "moves the player right in the world" in {
    val newWorld = worldWithPlayer.movePlayer(Right, player)

    newWorld._2.coordinates shouldBe((5, 6))

    val filteredPlayer: List[Piece] = newWorld._1.pieces.filter((piece: Piece) => piece.coordinates == (5, 6))

    filteredPlayer shouldBe List(newWorld._2)
  }

  it should "not move the player down in the world when at the bottom of the map" in {
    val player: Piece = Piece(Player(100, 100), (9, 5))
    val worldWithPlayer: World = world.withPlayer(player)
    val newWorld = worldWithPlayer.movePlayer(Down, player)

    newWorld._2.coordinates shouldBe((9, 5))

    val filteredPlayer: List[Piece] = newWorld._1.pieces.filter((piece: Piece) => piece.coordinates == (9, 5))

    filteredPlayer shouldBe List(newWorld._2)
  }


  it should "not move the player up in the world when at the top of the map" in {
    val player: Piece = Piece(Player(100, 100), (0, 5))
    val worldWithPlayer: World = world.withPlayer(player)
    val newWorld = worldWithPlayer.movePlayer(Up, player)

    newWorld._2.coordinates shouldBe((0, 5))

    val filteredPlayer: List[Piece] = newWorld._1.pieces.filter((piece: Piece) => piece.coordinates == (0, 5))

    filteredPlayer shouldBe List(newWorld._2)
  }

  it should "not move the player left in the world when at the left most side of the map" in {
    val player: Piece = Piece(Player(100, 100), (5, 0))
    val worldWithPlayer: World = world.withPlayer(player)
    val newWorld = worldWithPlayer.movePlayer(Left, player)

    newWorld._2.coordinates shouldBe((5, 0))

    val filteredPlayer: List[Piece] = newWorld._1.pieces.filter((piece: Piece) => piece.coordinates == (5, 0))

    filteredPlayer shouldBe List(newWorld._2)
  }

  it should "not move the player right in the world when at the right most side of the map" in {
    val player: Piece = Piece(Player(100, 100), (5, 9))
    val worldWithPlayer: World = world.withPlayer(player)
    val newWorld = worldWithPlayer.movePlayer(Right, player)

    newWorld._2.coordinates shouldBe((5, 9))

    val filteredPlayer: List[Piece] = newWorld._1.pieces.filter((piece: Piece) => piece.coordinates == (5, 9))

    filteredPlayer shouldBe List(newWorld._2)
  }
}
