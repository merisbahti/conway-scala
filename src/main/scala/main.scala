object main {
	def main(args: Array[String]) = {
		val coords =  Array(Array(false,true,false),
												Array(false,true,false),
												Array(false,true,false))
		val board: Board = new Board(coords)
		println(board)
		println("----------------------")
		println(board.iterate)
	}
}

class Board(coords: Array[Array[Boolean]]) {
	// Plz make sure your board is x*x in length. Shit will go crazy otherwise.
	def iterate(): Board = {
		new Board(
			(for {
					y <- (0 to coords.length-1)
				}	yield {
					(for {
						x <- (0 to coords.length-1)
					} yield countNeighbours(x,y) >= 3).toArray
			}).toArray
		)
	}

	def countNeighbours(x:Int,y:Int): Int = {
		(for 
		 { 
			 a <- (-1 to 1).toList // all combos
			 b <- (-1 to 1).toList
			 if x+a >= 0 && y+b >= 0 && // Check so that x+a and y+b aren't out of reach
				 y+b < coords.length && x+a < coords.length  
		 } yield coords(y+b)(x+a)).
		filter(_==true).length  // count which ones are alive
	}

	override def toString(): String = {
		coords.map(_.map(if (_) "X" else "0")).map(_.mkString(" ")).mkString("\n")
	}
}
