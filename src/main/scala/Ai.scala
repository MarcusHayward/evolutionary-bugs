package bugs

import scala.collection.immutable
import scala.util.Random

case class Ai(rules: Array[String]) {
  val random: Random.type = scala.util.Random

  def generateMove(world: World): Move = {
    val worldAsStringWithVisionOf = world.asStringWithVisionOf(1)
    val charAfterRule = getMatch(worldAsStringWithVisionOf);

    charAfterRule match {
      case 'u' => Up
      case 'd' => Down
      case 'l' => Left
      case 'r' => Right
      case 'o' => Random.shuffle(List(Up, Down, Left, Right)).head
    }
  }

  def getMatch(vision: String): Char = {
    for (rule <- rules) {
      val indexOfString = rule.indexOf(vision)
      if (isMatch(vision, rule)) {
        return rule.reverse.head
      }
    }

    return 'o'
  }

  def isMatch(vision: String, rule: String): Boolean = {
    if (vision.length != rule.length - 1) {
      return false
    }

    for ((piece, index) <- vision.view.zipWithIndex) {
      if (!(rule.charAt(index) == piece || rule.charAt(index) == 'd')) {
        return false
      }
    }

    return true
  }
}

object Ai {
  def fromFile(fileName: String): Ai = {
    val values: immutable.Seq[Array[String]] = for {
      line <- io.Source.fromFile(s"ai/$fileName").getLines().toVector
      values: Array[String] = line.split(",").map(_.trim)
    } yield {
      values
    }

    Ai(values.head)
  }
}
