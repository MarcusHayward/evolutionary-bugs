package bugs

import scala.util.Random

case class Ai(bitstring: String) {
  val random: Random.type = scala.util.Random
  def generateMove(world: World): Move = {
    val worldAsStringWithVisionOf = world.asStringWithVisionOf(2)

    val indexOfString = bitstring.indexOf(worldAsStringWithVisionOf)
    if (indexOfString >= 0) {
      val charAfterRule = bitstring.charAt(indexOfString + worldAsStringWithVisionOf.length)

      charAfterRule match {
        case 'u' => Up
        case 'd' => Down
        case 'l' => Left
        case 'r' => Right
        case 'o' => Random.shuffle(List(Up, Down, Left, Right)).head
      }
    } else {
      Random.shuffle(List(Up, Down, Left, Right)).head
    }
  }
}
