import org.otfried.cs109.UI._

import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.geom._

class Board {
	val a = Array.ofDim[Int](4,4)

	def empty(): List[(Int,Int)] = {
		var empty = List[(Int,Int)]()
		for (i <- 0 to 3) {
			for (j <- 0 to 3) {
				if (a(i)(j) == 0) {
					empty = (i,j) :: empty
				}
			}
		}
		empty
	}

	def isFull(): Boolean = {
		var bool = false
		for (i <- 0 to 3) {
			for (j <- 0 to 3) {
				bool = bool || (a(i)(j) == 0)
			}
		}
		!bool
	}

	def insert() {
		val b = empty
		val n = (math.random*(b.length)).toInt
		val (x,y) = b(n)
		val m = math.random
		a(x)(y) = if (m >= 0.9) 4 else 2
	}

	override def toString: String = {
		var s = ""
		var i = 0
		var j = 0
		while (i <= 16) {
			while (j <= 20) {
				if (i % 4 == 0 && j % 5 == 0) {
					s = s + "o"
					j = j + 1
				} else if (j % 5 == 0) {
					s = s + "|"
					j = j + 1 
				} else if (i % 4 == 0) {
					s = s + "-"
					j = j + 1
				} else if (i % 4 == 1 || i % 4 == 3) {
					s = s + " "
					j = j + 1
				} else {
					if (a(i/4)(j/5) >= 1000) {
						s = s + a(i/4)(j/5).toString
						j = j + 4
					} else if (a(i/4)(j/5) >= 100) {
						s = s + " " + a(i/4)(j/5).toString
						j = j + 4
					} else if (a(i/4)(j/5) >= 10) {
						s = s + " " + a(i/4)(j/5).toString + " "
						j = j + 4
					} else if (a(i/4)(j/5) != 0) {
						s = s + " " + " " + a(i/4)(j/5).toString + " "
						j = j + 4
					} else {
						s = s + "    "
						j = j + 4
					}
				}
			}
			s = s + "\n"
			i = i + 1
			j = 0
		}
		s
	}

	def pushLeft(): Int = {
		var points = 0
		for (j <- 0 to 3) {
			val r1 = a(j) filter (_ != 0)
			var r = Array(0,0,0,0)
			for (i <- 0 until r1.length) {
				r(i) = r1(i) 
			}
			if (r(0) == r(1) && r(0) != 0) {
				points = points + r(0)*2
				r(0) = r(0)*2
				r(1) = 0
				if (r(2) == r(3) && r(2) != 0) {
					points = points + r(2)*2
					r(2) = r(2)*2
					r(3) = 0
				}
				val r2 = r filter (_ != 0)
				r = Array(0,0,0,0)
				for (i <- 0 until r2.length) {
					r(i) = r2(i) 
				}	
			} else if (r(1) == r(2) && r(1) != 0) {
				points = points + r(1)*2
				r(1) = r(1)*2
				r(2) = 0
				val r3 = r filter (_ != 0)
				r = Array(0,0,0,0)
				for (i <- 0 until r3.length) {
					r(i) = r3(i) 
				}
			} else if (r(2) == r(3) && r(2) != 0) {
				points = points + r(2)*2
				r(2) = r(2)*2
				r(3) = 0
				val r4 = r filter (_ != 0)
				r = Array(0,0,0,0)
				for (i <- 0 until r4.length) {
					r(i) = r4(i) 
				}
			}
			a(j) = r
		}
		points
	}

	def turn() {
		val b = Array.ofDim[Int](4,4)
		for (i <- 0 to 3) {
			for (j <- 0 to 3) {
				b(i)(j) = a(j)(3-i)
			}
		}
		 for (i <- 0 to 3) {
		 	for (j <- 0 to 3) {
		 		a(i)(j) = b(i)(j)
		 	}
		}
	}

	def pushUp(): Int = {
		turn()
		val points = pushLeft()
		turn()
		turn()
		turn()
		points
	}

	def pushRight(): Int = {
		turn()
		turn()
		val points = pushLeft()
		turn()
		turn()
		points
	}

	def pushDown(): Int = {
		turn()
		turn()
		turn()
		val points = pushLeft()
		turn()
		points
	}

	def push(ch: Char): Int = {
		var a = 0
		if (ch == 'l') {
			a = pushLeft
		} else if (ch == 'r') {
			a = pushRight
		} else if (ch == 'u') {
			a = pushUp
		} else if (ch == 'd') {
			a = pushDown
		}
		a
	}

	
	// color for the background (shows in the gaps between tiles)
	val backgroundColor = new Color(0xbbada0)

	// colors of the tiles
	val tileColors = Map(0 -> new Color(205, 192, 180),
			    		 2 -> new Color(0xeee4da),
					     4 -> new Color(0xede0c8),
					     8 -> new Color(0xf2b179),
					     16 -> new Color(0xf59563),
					     32 -> new Color(0xf67c5f),
					     64 -> new Color(0xf65e3b), 
					     128 -> new Color(0xedcf72),
					     256 -> new Color(0xedcc61),
					     512 -> new Color(0xedc850),
					     1024 -> new Color(0xedc53f),
					     2048 -> new Color(0xedc22e))
	// color for other tiles (4096 etc.)
	val otherTileColor = new Color(0x3c3a32)

	val lightTextColor = new Color(119, 110, 101)
	val darkTextColor = new Color(0xf9f6f2)

	// returns color for the number in the tile, depending on tile value
	def textColor(tileValue: Int) = 
  		if (tileValue <= 4) lightTextColor else darkTextColor

	// returns font size for the number in the tile, depending on tile value
	def textSize(tileValue: Int) = 
  		if (tileValue <= 64) 55
  		else if (tileValue <= 512) 45
  		else if (tileValue <= 2048) 35
  		else 30
		
	def draw(canvas: BufferedImage) {
		val g = canvas.createGraphics()

		g.setColor(backgroundColor)
		g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
		
		g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)

		for (i <- 0 to 3) {
			for (j <- 0 to 3) {
				g.setColor(tileColors(a(j)(i)))
				g.fill(new Rectangle2D.Double(15.0 + i*122.0, 15.0 + j*122.0, 107.0, 107.0))
				g.setColor(textColor(a(j)(i)))
				g.setFont(new Font("sans-serif", Font.PLAIN, textSize(a(j)(i))))
				val w = g.getFontMetrics().stringWidth((a(j)(i)).toString)
				if (a(j)(i) > 0) {
					g.drawString((a(j)(i)).toString, 15 + (107-w)/2 + i*122, 15 + 71 + j*122)
				}

			}
		}
		g.dispose()
	}
}
object play2048 {
	def main(args: Array[String]) {
		val b = new Board
		var points = 0
		b.insert()
  		b.insert()

  		setTitle("2048 Game")
  		val canvas = new BufferedImage(500, 500, BufferedImage.TYPE_INT_RGB)

  		while (true) {
    		b.draw(canvas)
    		show(canvas)
    		val ch = waitKey()
    		if ("lrud" contains ch) {
    	  		points += b.push(ch)
    	  		if (b.isFull) {
    	  			showMessage("Game over. You achieved " + points.toString + " points.")
    	  			sys.exit()
    	  		}
    	  		b.insert()
    	  		setTitle("Your points: " + points.toString)
    		}
  		}
	}
}
