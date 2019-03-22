package bugs

import javax.swing.ImageIcon

import scala.swing._

object Main extends App {
  val dimension = 30
  val startingHealth = 100
  val startingThirst = 100
  val world = World.generateRandom(dimension)
  val ui = new UI

  def drawWorld(world: World, player: Piece, score: Int): Unit = {
    ui.renderWorld(world, player, score, false, false)
    ui.visible = true
  }

  val player = Piece(Player(startingHealth, startingThirst), (dimension / 2, dimension / 2))
  val worldWithPlayer = world.withPlayer(player)
  drawWorld(worldWithPlayer, player, 0)


  def run(ai: Ai, world: World, player: Piece, score: Int, displayUi: Boolean): Int = {
    //    Thread.sleep(500)
    //    val ai = Ai.fromFile("iteration_1.csv")
    val move = ai.generateMove(world)
    val newWorld: (World, Piece) = world.movePlayer(move, player)
    val isComplete = newWorld._1.isWorldResourcesConsumed

    val isDead: Boolean = player.pieceType match {
      case Player(h, t) => h == 0
      case _ => false
    }

    if (displayUi) {
      ui.renderWorld(newWorld._1, newWorld._2, score, isComplete, isDead)
    }

    if (!isComplete && !isDead) {
      run(ai, newWorld._1, newWorld._2, score + 1, displayUi)
    } else {
      score
    }
  }


  def getAi(numberOfAi: Int): List[Ai] = {
    def loop(accum: List[Ai]): List[Ai] = {
      if (accum.length == numberOfAi) {
        return accum
      }

      loop(Ai.random :: accum)
    }

    loop(List())
  }

  val ais: List[Ai] = getAi(1000)

  def runSimulationForAi(ai: Ai): (Ai, Int) = {
    val score = run(ai, worldWithPlayer, player, 0, false)
    (ai, score)
  }

  def simulate(ais: List[Ai]): List[(Ai, Int)] = {
    def loop(remainingAis: List[Ai], accum: List[(Ai, Int)]): List[(Ai, Int)] = {
      if (remainingAis.length == 0) {
        return accum
      }

      loop(remainingAis.tail, runSimulationForAi(remainingAis.head) :: accum)
    }

    loop(ais, List())
  }

  val reproducer = Reproducer()


  val timesToRun = 100

  println("Average,Average of top 10%, Top Score")
  def iterateGenerations(ais: List[Ai], onGeneration: Int): Int = {
    if (onGeneration != timesToRun) {
      val aisWithScores: List[(Ai, Int)] = simulate(ais)
      return iterateGenerations(reproducer.repoduce(aisWithScores), onGeneration + 1)
    }
    println("Simulation complete")
    return 1
  }

  iterateGenerations(ais, 0)
}

case class Reproducer() {
  val percentageToKeep = 0.1

  def repoduce(ais: List[(Ai, Int)]): List[Ai] = {

    val numberOfAis = ais.length
    val numberToKeep: Int = (numberOfAis * percentageToKeep).toInt
    val aisSortedByScores = scala.util.Sorting.stableSort(ais, (e1: (Ai, Int), e2: (Ai, Int)) => e1._2 > e2._2)

    println(s"${getTotalScores(aisSortedByScores) / aisSortedByScores.length}, ${getTotalScores(aisSortedByScores.take(10)) / 10}, ${aisSortedByScores.head._2}")

    def discardBadAis(ais: Array[(Ai, Int)]): List[Ai] = {
      def loop(currentAi: Ai, remainingAis: Array[(Ai, Int)], accum: List[Ai]): List[Ai] = {
        if (accum.length == numberToKeep) {
          return accum
        }

        loop(remainingAis.head._1, remainingAis.tail, currentAi :: accum)
      }

      loop(ais.head._1, ais.tail, List())
    }

    val goodAis = discardBadAis(aisSortedByScores)

    val aisAfterMerge = goodAis
//  val aisAfterMerge = merge(goodAis)

    aisAfterMerge ::: getNextGeneration(numberOfAis - aisAfterMerge.length)
  }

  def merge(ais: List[Ai]): List[Ai] = {
    def loop(currentAi: Ai, nextAi: Ai, remainingAi: List[Ai], accum: List[Ai]): List[Ai] = {
      if (remainingAi.length == 0) {
        return accum
      }

      val currentAiRules = currentAi.rules
      val nextAiRules = nextAi.rules

      val mergedAi = Ai(currentAiRules.take((currentAiRules.length / 2).toInt) ++ nextAiRules.take((nextAiRules.length / 2).toInt))

      loop(remainingAi.head, remainingAi.tail.head, remainingAi.tail.tail, mergedAi :: accum)
    }

    loop(ais.head, ais.tail.head, ais.tail.tail, List())
  }

  def getTotalScores(ais: Array[(Ai, Int)]): Int = {
    def loop(accum: Int, remainingAis: Array[(Ai, Int)]): Int = {
      if (remainingAis.length == 0) {
        return accum
      }

      loop(accum + remainingAis.head._2, remainingAis.tail)
    }

    loop(0, ais)
  }

  def getNextGeneration(amountToGenerate: Int): List[Ai] = {
    def loop(accum: List[Ai]): List[Ai] = {
      if (accum.length == amountToGenerate) {
        return accum
      }

      loop(Ai.random :: accum)
    }

    loop(List())
  }
}

class UI extends MainFrame {
  title = "Evolutionary Bugs"
  preferredSize = new Dimension(20 * Main.dimension, 24 * Main.dimension)

  def renderWorld(world: World, player: Piece, score: Int, isComplete: Boolean, isDead: Boolean): Unit = {
    val components = world.pieces.map {
      piece => {
        new Label {
          icon = new ImageIcon(piece.pieceType.imagePath)
        }
      }
    }
    val box = new BoxPanel(Orientation.Vertical)

    if (isComplete) {
      box.contents += new Label("Simulation Complete!")
      box.contents += new Label(s"You took: $score turns to complete the game.")
    } else if (isDead) {
      box.contents += new Label(s"Simulation Complete! You lasted $score turns!")
      player.pieceType match {
        case Player(h, t) => box.contents += new Label(s"Health: $h")
        case _ =>
      }
    } else {
      box.contents += new Label(s"Turns used: $score")

      player.pieceType match {
        case Player(h, t) => box.contents += new Label(s"Health: $h")
        case _ =>
      }
    }

    val gridPanel = new GridPanel(Main.dimension, Main.dimension)
    gridPanel.contents.appendAll(components)
    gridPanel.border = Swing.EmptyBorder(10, 10, 10, 10)

    box.contents += gridPanel
    box.border = Swing.EmptyBorder(10, 10, 10, 10)

    contents = box
  }
}
