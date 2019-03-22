package bugs

import scala.collection.immutable
import scala.util.Random

case class Ai(rules: Array[String]) {
  val random: Random.type = scala.util.Random

  def generateMove(world: World): Move = {
    val worldAsStringWithVisionOf = world.asStringWithVisionOf(2)
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

  def random(): Ai = {
    val numberOfRules = 100
    val lengthOfRule = 25
    val rules: Array[String] = getRandomRules(numberOfRules, lengthOfRule)
    Ai(rules)
  }

  def getRandomRules(numberOfRules: Int, lengthOfRule: Int): Array[String] = {
    def loop(accum: Array[String]): Array[String] = {
      if (accum.length == numberOfRules) {
        return accum
      }

      loop(accum :+ getRandomRule(lengthOfRule))
    }

    loop(Array())
  }

  def getRandomRule(lengthOfRule: Int): String = {

    val rule = "dddddddddddddddddddddddd"

    val (first, second) = rule.splitAt(Random.shuffle(List.range(0, lengthOfRule)).head)

    return first + 'f' + second + Random.shuffle(List("u", "d", "l", "r")).head

    def loop(accum: String): String = {
      if (accum.length == lengthOfRule) {
        return accum
      }

      if (accum.length == (lengthOfRule - 1) / 2) {
        loop(accum.concat("p"))
      } else {
        loop(accum.concat(Random.shuffle(List("f", "d")).head))
      }
    }

    loop("") + Random.shuffle(List("u", "d", "l", "r")).head
  }
}
