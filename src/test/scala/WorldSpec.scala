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

  it should "reduce the player health and thirst levels" in {
    val player: Player = Player(100, 100)
    val newPlayer = player.reduce()

    newPlayer.health shouldBe 90
    newPlayer.thirst shouldBe 90
  }

  it should "increase the players health when they eat" in {
    val player: Player = Player(100, 100)
    val newPlayer = player.eat()

    newPlayer.health shouldBe 120
  }

  it should "increase the players thirst when they drink" in {
    val player: Player = Player(100, 100)
    val newPlayer = player.drink()

    newPlayer.thirst shouldBe 120
  }

  it should "returns the world as a string with a vision of 2" in {
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

    world.asStringWithVisionOf(2) shouldBe "eeeeeeefeeeepeeeeeeeeeeee"
  }
}
