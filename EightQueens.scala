

//8-queens problem

def size = 1 to 8

def board = for {
	r <- size
	c <- size
}yield((r, c))

def doesNotAttack(queen1:(Int, Int), queen2:(Int, Int)): Boolean =
	(queen1._1 != queen2._1) & (queen1._2 != queen2._2) & ((queen2._1 - queen1._1).abs != (queen2._2 - queen1._2).abs) & (queen1._1 < queen2._1)

def doesNotAttackQueens(queens: Seq[(Int,Int)])(queen: (Int, Int)): Boolean = queens match {
	case firstQueen :: restQueens => doesNotAttack(firstQueen, queen) & doesNotAttackQueens(restQueens)(queen)
	case Nil => true
}

//Using straightforward seq monad
/*********************************/
def chooseQueensV1(board: Seq[(Int, Int)]) = for {
	queen1 <- board
	queen2 <- board if (doesNotAttackQueens(Seq(queen1))(queen2))
	queen3 <- board if (doesNotAttackQueens(Seq(queen1, queen2))(queen3))
	queen4 <- board if (doesNotAttackQueens(Seq(queen1, queen2, queen3))(queen4))
	queen5 <- board if (doesNotAttackQueens(Seq(queen1, queen2, queen3, queen4))(queen5))
	queen6 <- board if (doesNotAttackQueens(Seq(queen1, queen2, queen3, queen4, queen5))(queen6))
	queen7 <- board if (doesNotAttackQueens(Seq(queen1, queen2, queen3, queen4, queen5, queen6))(queen7))
	queen8 <- board if (doesNotAttackQueens(Seq(queen1, queen2, queen3, queen4, queen5, queen6, queen7))(queen8))
}yield(Seq(queen1, queen2, queen3, queen4, queen5, queen6, queen7, queen8))


//Using seq monad recursively
/*****************************/
def chooseQueensV2(queens: Seq[(Int, Int)], board: Seq[(Int, Int)], size: Int): Seq[(Int, Int)]
	= queens.length match {
		case `size` => queens
		case _ => for {
			queen <- board if (doesNotAttackQueens(queens)(queen))
			res <- chooseQueensV2(queens ++ Seq(queen), board, size)
		} yield (res)
	}

def showQueensV1 = (chooseQueensV1(board).map(queens => queens.map(queen => queen._2))).foreach(str => print(str + "\n"))
def showQueensV2 = (chooseQueensV2(Nil, board, size.length).grouped(8).toList.map(queens => queens.map(queen => queen._2))).foreach(str => print(str + "\n"))
