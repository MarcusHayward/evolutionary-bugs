package bugs

import scala.util.Random

case class World(pieces: List[Piece], dimension: Int) {

  val random: Random.type = scala.util.Random

  def this(sort: Boolean, pieces: List[Piece], dimension: Int) =
    this(pieces.sortBy((piece: Piece) => (piece.coordinates._1, piece.coordinates._2)), dimension)

  def asStringWithVisionOf(vision: Int): String = {
    val player: Option[Piece] = pieces.find(piece => piece.pieceType match {
      case Player(_, _) => true
      case _ => false
    })

    val coordinatesToCheck: List[(Int, Int)] = for {
      x <- List.range(-vision, vision + 1)
      y <- List.range(-vision, vision + 1)
    } yield {
      (x + player.get.coordinates._1, y + player.get.coordinates._2)
    }

    val chars: List[Char] = for {
      coordinates <- coordinatesToCheck
      piece <- findPieceByCoordinates(coordinates)
    } yield {
      piece.pieceType match {
        case Player(_, _) => 'p'
        case Food => 'f'
        case Empty => 'e'
      }
    }

    chars.mkString
  }

  def findPieceByCoordinates(searchingForCoordinates: (Int, Int)): Option[Piece] = {
    pieces.find(searchingForCoordinates == _.coordinates)
  }

  def withPlayer(player: Piece): World = {
    World(
      for {
        piece <- pieces
      } yield {
        piece.coordinates match {
          case player.coordinates => player
          case _ => piece
        }
      },
      dimension
    )
  }

  def movePlayer(move: Move, player: Piece): (World, Piece) = {
    val piecesWithPlayerSwappedForAnEmptyPiece: List[Piece] = pieces.map { piece: Piece =>
      piece.pieceType match {
        case Player(_, _) => Piece(Empty, (piece.coordinates._1, piece.coordinates._2))
        case _ => piece
      }
    }

    val movedPlayer: Piece = move match {
      case Up if player.coordinates._1 == 0 => player
      case Down if player.coordinates._1 == this.dimension - 1 => player
      case Left if player.coordinates._2 == 0 => player
      case Right if player.coordinates._2 == this.dimension - 1 => player
      case Up => player.move(-1, 0)
      case Down => player.move(+1, 0)
      case Left => player.move(0, -1)
      case Right => player.move(0, +1)
      case RandomMove =>
        random.nextInt(4) match {
          case 0 => player.move(-1, 0)
          case 1 => player.move(+1, 0)
          case 2 => player.move(0, -1)
          case 3 => player.move(0, +1)
        }
    }

    val worldWithEmptyPlayerSpace: List[Piece] =
      piecesWithPlayerSwappedForAnEmptyPiece.filterNot((p: Piece) => movedPlayer.coordinates == p.coordinates)

    (new World(
      true,
      movedPlayer :: worldWithEmptyPlayerSpace,
      this.dimension
    ),
      movedPlayer)
  }

  def isWorldResourcesConsumed: Boolean = {
    val foodAndWater = pieces.filter((piece: Piece) => piece.pieceType match {
      case Food => true
      case _ => false
    })

    foodAndWater.isEmpty
  }
}

sealed trait Move

case object Up extends Move

case object Down extends Move

case object Left extends Move

case object Right extends Move

case object RandomMove extends Move

case class Piece(pieceType: PieceType, coordinates: (Int, Int)) {
  def move(x: Int, y: Int): Piece = {
    Piece(pieceType, (coordinates._1 + x, coordinates._2 + y))
  }
}

sealed trait PieceType {
  override def toString: String = this match {
    case Food => "F"
    case Empty => "_"
    case Player(_, _) => "P"
  }

  def imagePath: String = this match {
    case Food => "assets/food.png"
    case Empty => "assets/earth.png"
    case Player(_, _) => "assets/player.png"
  }
}

case class Player(health: Int, thirst: Int) extends PieceType {
  def reduce(): Player = {
    Player(health - 10, thirst - 10)
  }

  def eat(): Player = {
    Player(health + 20, thirst)

  }

  def drink(): Player = {
    Player(health, thirst + 20)
  }

  def isDead: Boolean = {
    health == 0 || thirst == 0
  }
}

case object Food extends PieceType

case object Empty extends PieceType

object World {
  val random: Random.type = scala.util.Random

  def generateRandom(dimension: Int) = World(
    for {
      x <- List.range(0, dimension)
      y <- List.range(0, dimension)
    } yield {
      random.nextInt(10) match {
        case r if r < 2 => Piece(Food, (x, y))
        case _ => Piece(Empty, (x, y))
      }
    },
    dimension
  )
}
