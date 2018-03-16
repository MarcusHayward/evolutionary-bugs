package bugs

import scala.util.Random

case class Ai(bitstring: String) {
  val random: Random.type = scala.util.Random
  def generateMove(world: World): Move = {
    random.nextInt(4) match {
      case 0 => Up
      case 1 => Down
      case 2 => Left
      case 3 => Right
    }
  }
}
