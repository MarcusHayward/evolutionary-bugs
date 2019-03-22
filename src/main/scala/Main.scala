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

  val ai = Ai.random
  def run(world: World, player: Piece, score: Int): Unit = {
    Thread.sleep(1000)
//    val ai = Ai.fromFile("iteration_1.csv")
    val move = ai.generateMove(world)
    val newWorld: (World, Piece) = world.movePlayer(move, player)
    val isComplete = newWorld._1.isWorldResourcesConsumed

    val isDead: Boolean = player.pieceType match {
      case Player(h, t) => h == 0
      case _ => false
    }

    ui.renderWorld(newWorld._1, newWorld._2, score, isComplete, isDead)

    if (!isComplete && !isDead) {
      run(newWorld._1, newWorld._2, score + 1)
    }
  }

  run(worldWithPlayer, player, 0)
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
