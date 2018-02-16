case class World(pieces: List[Piece], dimension: Int)
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
    case Player => "P"
  }
}

case object Food extends PieceType

case object Water extends PieceType

case object Empty extends PieceType

case object Player extends PieceType

object World {
  val random = scala.util.Random

  def generateRandom(dimension: Int) = World(
    for {
      x <- List.range(0, dimension)
      y <- List.range(0, dimension)
    } yield {
      random.nextInt(10) match {
        case x if (x < 1) => Piece(Food, (x, y))
        case x if (x < 2) => Piece(Water, (x, y))
        case _ => Piece(Empty, (x, y))
      }
    },
    dimension
  )
}
