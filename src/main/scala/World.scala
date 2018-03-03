import scala.util.Random

case class World(pieces: List[Piece], dimension: Int) {
  def this(sort: Boolean, pieces: List[Piece], dimension: Int) =
    this(pieces.sortBy((piece: Piece) => (piece.coordinates._1, piece.coordinates._2)), dimension)

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

  def movePlayer(move: Move): World = {
    val player = this.pieces.find {
      piece =>
        piece.pieceType match {
          case Player => true
          case _ => false
        }
    }

    move match {
      case Up if player.get.coordinates._1 == 0 => return this
      case Down if player.get.coordinates._1 == this.dimension - 1 => return this
      case Left if player.get.coordinates._2 == 0 => return this
      case Right if player.get.coordinates._2 == this.dimension - 1 => return this
      case _ =>
    }

    val worldWithNoPlayer = new World(
      true,
      for {
        piece <- this.pieces
      } yield {
        piece.pieceType match {
          case Player => Piece(Empty, (piece.coordinates._1, piece.coordinates._2))
          case _ => piece
        }
      },
      dimension
    )

    val movedPlayer = move match {
      case Up => Piece(Player, (player.get.coordinates._1 - 1, player.get.coordinates._2))
      case Down => Piece(Player, (player.get.coordinates._1 + 1, player.get.coordinates._2))
      case Left => Piece(Player, (player.get.coordinates._1, player.get.coordinates._2 - 1))
      case Right => Piece(Player, (player.get.coordinates._1, player.get.coordinates._2 + 1))
    }

    val worldWithEmptyPlayerSpace = worldWithNoPlayer.pieces.filter(piece => piece.coordinates match {
      case movedPlayer.coordinates => false
      case _ => true
    })

    new World(
      true,
      movedPlayer :: worldWithEmptyPlayerSpace,
      this.dimension
    )
  }
}

sealed trait Move

case object Up extends Move

case object Down extends Move

case object Left extends Move

case object Right extends Move


case class Piece(pieceType: PieceType, coordinates: (Int, Int))

sealed trait PieceType {
  override def toString: String = this match {
    case Food => "F"
    case Water => "W"
    case Empty => "_"
    case Player => "P"
  }

  def imagePath: String = this match {
    case Food => "assets/food.png"
    case Water => "assets/water.png"
    case Empty => "assets/earth.png"
    case Player => "assets/player.png"
  }
}

case object Food extends PieceType

case object Water extends PieceType

case object Empty extends PieceType

case object Player extends PieceType

object World {
  val random: Random.type = scala.util.Random

  def generateRandom(dimension: Int) = World(
    for {
      x <- List.range(0, dimension)
      y <- List.range(0, dimension)
    } yield {
      random.nextInt(10) match {
        case r if r < 1 => Piece(Food, (x, y))
        case r if r < 2 => Piece(Water, (x, y))
        case _ => Piece(Empty, (x, y))
      }
    },
    dimension
  )
}
