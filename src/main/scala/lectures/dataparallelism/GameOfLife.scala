package lectures
package dataparallelism

import scala.swing._
import java.awt.image._
import collection._

trait GameOfLifeUtils {
  
  def blockSize: Int
  
  val grid = concurrent.TrieMap[(Int, Int), Block]()
  
  private def getBlock(g: Int, x: Int, y: Int) = {
    val pos = (x / blockSize, y / blockSize);
    grid.get(pos) match {
      case Some(block) =>
        block
      case None =>
        grid.put(pos, new Block(pos._1, pos._2, blockSize))
        grid(pos)
    }
  }
  
  def update(g: Int, x: Int, y: Int, v: Boolean) {
    val block = getBlock(g, x, y)
    block.cells((g - 1) % 2)((y % blockSize) * blockSize + (x % blockSize)) = v
    block.cells(g % 2)((y % blockSize) * blockSize + (x % blockSize)) = v
  }
  
  def apply(g: Int, x: Int, y: Int) = {
    val block = getBlock(g, x, y)
    block.cells(g % 2)((y % blockSize) * blockSize + (x % blockSize))
  }
  
  final class Block(val xp: Int, val yp: Int, val size: Int) {
    lazy val cells = Array(
      new Array[Boolean](size * size),
      new Array[Boolean](size * size)
    )
    lazy val emptiness = Array(false, false)
    
    def isEmpty(generation: Int) = emptiness(generation % 2)
    
    def apply(generation: Int, x: Int, y: Int) = {
      cells(generation % 2)(y * size + x)
    }
    
    def simulate(generation: Int) {
      updateCells(generation)
      addNeighbours(generation)
      checkRemove(generation)
    }
    
    def checkRemove(g: Int) {
      def empty(b: Block) = (b eq null) || (b.isEmpty(g - 1))
      if (
        empty(grid.lookup((xp - 1, yp - 1))) &&
        empty(grid.lookup((xp - 0, yp - 1))) &&
        empty(grid.lookup((xp + 1, yp - 1))) &&
        empty(grid.lookup((xp - 1, yp - 0))) &&
        empty(grid.lookup((xp + 1, yp - 0))) &&
        empty(grid.lookup((xp - 1, yp + 1))) &&
        empty(grid.lookup((xp - 0, yp + 1))) &&
        empty(grid.lookup((xp + 1, yp + 1)))
      ) grid.remove((xp, yp))
    }
    
    def addNeighbours(g: Int) = if (!isEmpty(g)) {
      def add(xp: Int, yp: Int) {
        val pos = (xp, yp)
        if (grid.lookup(pos) eq null) grid.putIfAbsent(pos, new Block(xp, yp, blockSize))
      }
      add(xp - 1, yp - 1)
      add(xp - 0, yp - 1)
      add(xp + 1, yp - 1)
      add(xp - 1, yp - 0)
      add(xp + 1, yp - 0)
      add(xp - 1, yp + 1)
      add(xp - 0, yp + 1)
      add(xp + 1, yp + 1)
    }
    
    def updateCells(g: Int) {
      val lasta = cells((g - 1) % 2)
      val curra = cells(g % 2)
      @inline def last(x: Int, y: Int) = lasta(y * size + x)
      @inline def curr(x: Int, y: Int, v: Boolean) = curra(y * size + x) = v
      def countMiddle(x: Int, y: Int) = {
        var count = 0
        if (last(x - 1, y - 1)) count += 1
        if (last(x - 0, y - 1)) count += 1
        if (last(x + 1, y - 1)) count += 1
        if (last(x - 1, y - 0)) count += 1
        if (last(x + 1, y - 0)) count += 1
        if (last(x - 1, y + 1)) count += 1
        if (last(x - 0, y + 1)) count += 1
        if (last(x + 1, y + 1)) count += 1
        count
      }
      def countUp(x: Int, above: Block) = {
        var count = 0
        if (above(g - 1, x - 1, size - 1)) count += 1
        if (above(g - 1, x - 0, size - 1)) count += 1
        if (above(g - 1, x + 1, size - 1)) count += 1
        if (last(x - 1, 0)) count += 1
        if (last(x + 1, 0)) count += 1
        if (last(x - 1, 1)) count += 1
        if (last(x - 0, 1)) count += 1
        if (last(x + 1, 1)) count += 1
        count
      }
      def countDown(x: Int, below: Block) = {
        var count = 0
        if (last(x - 1, size - 1 - 1)) count += 1
        if (last(x - 0, size - 1 - 1)) count += 1
        if (last(x + 1, size - 1 - 1)) count += 1
        if (last(x - 1, size - 1 - 0)) count += 1
        if (last(x + 1, size - 1 - 0)) count += 1
        if (below(g - 1, x - 1, 0)) count += 1
        if (below(g - 1, x - 0, 0)) count += 1
        if (below(g - 1, x + 1, 0)) count += 1
        count
      }
      def countLeft(y: Int, left: Block) = {
        var count = 0
        if (left(g - 1, size - 1, y - 1)) count += 1
        if (last(0, y - 1)) count += 1
        if (last(1, y - 1)) count += 1
        if (left(g - 1, size - 1, y - 0)) count += 1
        if (last(1, y - 0)) count += 1
        if (left(g - 1, size - 1, y + 1)) count += 1
        if (last(0, y + 1)) count += 1
        if (last(1, y + 1)) count += 1
        count
      }
      def countRight(y: Int, right: Block) = {
        var count = 0
        if (last(size - 1 - 1, y - 1)) count += 1
        if (last(size - 1 - 0, y - 1)) count += 1
        if (right(g - 1, 0, y - 1)) count += 1
        if (last(size - 1 - 1, y - 0)) count += 1
        if (right(g - 1, 0, y - 0)) count += 1
        if (last(size - 1 - 1, y + 1)) count += 1
        if (last(size - 1 - 0, y + 1)) count += 1
        if (right(g - 1, 0, y + 1)) count += 1
        count
      }
      def countUpLeft(up: Block, left: Block, upleft: Block) = {
        var count = 0
        if (upleft(g - 1, size - 1, size - 1)) count += 1
        if (up(g - 1, 0, size - 1)) count += 1
        if (up(g - 1, 1, size - 1)) count += 1
        if (left(g - 1, size - 1, 0)) count += 1
        if (last(1, 0)) count += 1
        if (left(g - 1, size - 1, 1)) count += 1
        if (last(0, 1)) count += 1
        if (last(1, 1)) count += 1
        count
      }
      def countUpRight(up: Block, right: Block, upright: Block) = {
        var count = 0
        if (up(g - 1, size - 1 - 1, size - 1)) count += 1
        if (up(g - 1, size - 1 - 0, size - 1)) count += 1
        if (upright(g - 1, 0, size - 1)) count += 1
        if (last(size - 1 - 1, 0)) count += 1
        if (right(g - 1, 0, 0)) count += 1
        if (last(size - 1 - 1, 1)) count += 1
        if (last(size - 1 - 0, 1)) count += 1
        if (right(g - 1, 0, 1)) count += 1
        count
      }
      def countDownLeft(down: Block, left: Block, downleft: Block) = {
        var count = 0
        if (left(g - 1, size - 1, size - 1 - 1)) count += 1
        if (last(0, size - 1 - 1)) count += 1
        if (last(1, size - 1 - 1)) count += 1
        if (left(g - 1, size - 1, size - 1 - 0)) count += 1
        if (last(1, size - 1 - 0)) count += 1
        if (downleft(g - 1, size - 1, 0)) count += 1
        if (down(g - 1, 0, 0)) count += 1
        if (down(g - 1, 1, 0)) count += 1
        count
      }
      def countDownRight(down: Block, right: Block, downright: Block) = {
        var count = 0
        if (last(size - 1 - 1, size - 1 - 1)) count += 1
        if (last(size - 1 - 0, size - 1 - 1)) count += 1
        if (right(g - 1, 0, size - 1 - 1)) count += 1
        if (last(size - 1 - 1, size - 1 - 0)) count += 1
        if (right(g - 1, 0, size - 1 - 0)) count += 1
        if (down(g - 1, size - 1 - 1, 0)) count += 1
        if (down(g - 1, size - 1 - 0, 0)) count += 1
        if (downright(g - 1, 0, 0)) count += 1
        count
      }
      def transition(live: Boolean, count: Int) =
        (live && count == 2) || count == 3
      var empty = true
      
      // update middle
      var x, y = 1
      while (y < size - 1) {
        while (x < size - 1) {
          val count = countMiddle(x, y)
          val newval = transition(last(x, y), count)
          curr(x, y, newval)
          if (newval) empty = false
          x += 1
        }
        x = 1
        y += 1
      }
      
      // fetch neighbours
      val upleft    = grid.getOrElse((xp - 1, yp - 1), emptyBlock)
      val up        = grid.getOrElse((xp - 0, yp - 1), emptyBlock)
      val upright   = grid.getOrElse((xp + 1, yp - 1), emptyBlock)
      val left      = grid.getOrElse((xp - 1, yp - 0), emptyBlock)
      val right     = grid.getOrElse((xp + 1, yp - 0), emptyBlock)
      val downleft  = grid.getOrElse((xp - 1, yp + 1), emptyBlock)
      val down      = grid.getOrElse((xp - 0, yp + 1), emptyBlock)
      val downright = grid.getOrElse((xp + 1, yp + 1), emptyBlock)
      
      // update top border
      x = 1
      while (x < size - 1) {
        val count = countUp(x, up)
        val newval = transition(last(x, 0), count)
        curr(x, 0, newval)
        if (newval) empty = false
        x += 1
      }
      
      // update bottom border
      x = 1
      while (x < size - 1) {
        val count = countDown(x, down)
        val newval = transition(last(x, size - 1), count)
        curr(x, size - 1, newval)
        if (newval) empty = false
        x += 1
      }
      
      // update left border
      y = 1
      while (y < size - 1) {
        val count = countLeft(y, left)
        val newval = transition(last(0, y), count)
        curr(0, y, newval)
        if (newval) empty = false
        y += 1
      }
      
      // update right border
      y = 1
      while (y < size - 1) {
        val count = countRight(y, right)
        val newval = transition(last(size - 1, y), count)
        curr(size - 1, y, newval)
        if (newval) empty = false
        y += 1
      }
      
      // update corners
      {
        val count = countUpLeft(up, left, upleft)
        val newval = transition(last(0, 0), count)
        curr(0, 0, newval)
        if (newval) empty = false
      }
      {
        val count = countUpRight(up, right, upright)
        val newval = transition(last(size - 1, 0), count)
        curr(size - 1, 0, newval)
        if (newval) empty = false
      }
      {
        val count = countDownLeft(down, left, downleft)
        val newval = transition(last(0, size - 1), count)
        curr(0, size - 1, newval)
        if (newval) empty = false
      }
      {
        val count = countDownRight(down, right, downright)
        val newval = transition(last(size - 1, size - 1), count)
        curr(size - 1, size - 1, newval)
        if (newval) empty = false
      }
      
      // set emptiness
      emptiness(g % 2) = empty
    }
  }
  
  val emptyBlock = new Block(-1, -1, blockSize)
  
  /* some custom configurations */
  
  def initBars(sidelength: Int) {
    grid.clear()
    for (y <- 0 until sidelength) {
      for (x <- 0 until sidelength by 2) this(1, x, y) = true
    }
  }
  
}


object GameOfLifeDemo extends GameOfLifeUtils {
  /* config */
  
  lazy val blockSize = sys.props.getOrElse("blocksize", "10").toInt
  
  val sidelength = sys.props.getOrElse("sidelength", "800").toInt
  
  val parlevel = sys.props.getOrElse("parlevel", "4").toInt
  
  val refreshrate = sys.props.getOrElse("refreshrate", "10").toInt
  
  val tasksupport = new parallel.ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parlevel))
  
  val width = 1440
  
  val height = 960
  
  var buffer = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
  
  val display = new Display
  
  class Display extends Component {
    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
      g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, 
                         java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
      
      buffer.synchronized {
        g.drawImage(buffer, 0, 0, width, height, 0, 0, width, height, null, null)
      }
    }
  }
  
  val frame = new Frame {
    title = "Game of Life"
    contents = display
    display.requestFocus()
  }
  
  def main(args: Array[String]) {
    initBars(sidelength)
    frame.size = new Dimension(width, height)
    frame.peer.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE)
    frame.open()
    simulator.start()
  }
  
  val simulator = new Thread {
    val raster = new Array[Int](width * height)
    var lasttick = System.nanoTime()
    
    setDaemon(true)
    
    override def run() {
      var g = 1
      val grid = GameOfLifeDemo.this.grid.par
      grid.tasksupport = tasksupport
      while (true) {
        if ((g - 1) % refreshrate == 0) refresh(g, grid)
        for ((pos, block) <- grid) block.simulate(g)
        g += 1
      }
    }
    
    def refresh(gen: Int, grid: parallel.mutable.ParTrieMap[(Int, Int), Block]) = {
      var x, y = 0
      while (y < height) {
        while (x < width) {
          raster(y * width + x) = 0xffffffff
          x += 1
        }
        x = 0
        y += 1
      }
      
      for ((pos, block) <- grid) {
        val xoff = pos._1 * blockSize
        val yoff = pos._2 * blockSize
        val arr = block.cells(gen % 2)
        var i, j = 0
        while (j < blockSize) {
          while (i < blockSize) {
            val col = if (arr(j * blockSize + i)) 0xff005599 else 0xffcccccc
            val absx = xoff + i
            val absy = yoff + j
            if (absx >= 0 && absx < width && absy >= 0 && absy < height) raster(absy * width + absx) = col
            i += 1
          }
          i = 0
          j += 1
        }
      }
      
      buffer.setRGB(0, 0, width, height, raster, 0, width)
      val g = buffer.getGraphics.asInstanceOf[java.awt.Graphics2D]
      g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
      g.setColor(java.awt.Color.BLACK)
      g.drawString("Gen: " + gen.toString, 1250, 80)
      g.drawString("FPS: " + fps().toString, 1250, 95)
      
      display.repaint()
    }
    
    def fps() = {
      val tick = System.nanoTime()
      val diff = tick - lasttick
      lasttick = tick
      (1000000000.0 / diff * refreshrate).toInt
    }
  }
  
}
