import cmpsci220.hw.sudoku._

object Solution extends SudokuLike {
  type T = Board
  
  
  def makeEmptyMap(y: Int, x: Int): Map[(Int,Int), List[Int]] = (y,x) match{
    case (8,8) => Map(((y,x), List(1,2,3,4,5,6,7,8,9)))
    case (_,8) => Map(((y,x), List(1,2,3,4,5,6,7,8,9))) ++ makeEmptyMap(y+1, 0)
    case _ => Map(((y,x), List(1,2,3,4,5,6,7,8,9))) ++ makeEmptyMap(y, x+1)

  }
  def parseHelper(str: String, x: Int, y: Int, board: Board): Board = str.length match {
    case 0 => board
    case _ => if(str.head != '.')  
                if(x < 8)
                  parseHelper(str.tail, x+1, y, board.place(y,x,(str.head)-48))
                else 
                  parseHelper(str.tail, 0, y+1, board.place(y,x,(str.head)-48))
              else
                if(x < 8)
                  parseHelper(str.tail, x+1, y, board)
                else 
                  parseHelper(str.tail, 0, y+1, board)
  }
  def parse(str: String): Board = {
    val emptyBoard = new Board(makeEmptyMap(0,0))
    parseHelper(str, 0, 0, emptyBoard)
   

  }
  def rowPeersHelper(row: Int, col: Int):Set[(Int,Int)] = col match{
    case 8 => Set((row, 8))
    case _ => Set((row, col)) ++ rowPeersHelper(row, col+1)                    
  }
  def colPeersHelper(row: Int, col:Int):Set[(Int, Int)] = row match{  
    case 8 => Set((8, col))
    case _ => Set((row, col)) ++ colPeersHelper(row+1, col)
  }
  def blockPeersHelper(row: Int, col: Int): Set[(Int,Int)] ={
    val xDist = col%3
    val yDist = row%3
    xDist match{
      case 0 => if(yDist == 0)
                  Set((row+2, col+2),(row+1, col+1),(row+1,col+2),(row+2,col+1))
                else if(yDist == 1)
                  Set((row+1, col+1),(row+1, col+2),(row-1,col+1),(row-1,col+2))
                else
                  Set((row-1, col+1),(row-1, col+2),(row-2,col+1),(row-2,col+2))                  
      case 1 => if(yDist == 0)
                  Set((row+2, col-1),(row+1, col-1),(row+1,col+1),(row+2,col+1))
                else if(yDist == 1)
                  Set((row-1, col-1),(row+1, col-1),(row-1,col+1),(row+1,col+1))
                else
                  Set((row-1, col-1),(row-1, col+1),(row-2,col+1),(row-2,col-1))
      case 2 => if(yDist == 0)
                  Set((row+1, col-2),(row+1, col-1),(row+2,col-2),(row+2,col-1))
                else if(yDist == 1)
                  Set((row-1, col-1),(row+1, col-1),(row-1,col-2),(row+1,col-2))
                else
                  Set((row-2, col-1),(row-1, col-1),(row-1,col-2),(row-2,col-2))
    }

  }
  def peers(row: Int, col: Int): List[(Int, Int)] = {
    val peers = rowPeersHelper(row, 0) ++ colPeersHelper(0,col) ++ blockPeersHelper(row,col) -- Set((row,col))
    peers.toList
  }
}

// Top-left corner is (0,0). Bottom-right corner is (8,8).
// You don't have to have a field called available. Feel free to change it.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {
  def getAvailableMap(): Map[(Int, Int), List[Int]] = {
    available
  }
  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    if(availableValuesAt(row,col).length == 1)
      Some(availableValuesAt(row,col).head)
    else 
      None
  }
  def isSolvedHelper(x: Int, y: Int): Boolean = (y,x) match {
    case (8,8) => if(availableValuesAt(y,x).length == 1)
                    true
                  else
                    false
    case (_, 8) => if(availableValuesAt(y,x).length == 1)
                      isSolvedHelper(0, y+1)
                   else
                      false
    case _ => if(availableValuesAt(y,x).length == 1)
                isSolvedHelper(x+1, y)
              else
                false
  }
  def isSolved(): Boolean = {
    isSolvedHelper(0,0)
  }
  def isUnsolvableHelper(x: Int, y: Int): Boolean = (y,x) match {
    case (8,8) => if(availableValuesAt(y,x).isEmpty)
                    true
                  else
                    false
    case (_, 8) => if(availableValuesAt(y,x).isEmpty)
                      true
                   else
                      isUnsolvableHelper(0, y+1)
    case _ => if(availableValuesAt(y,x).isEmpty)
                true
              else
                isUnsolvableHelper(x+1, y)
  }
  def isUnsolvable(): Boolean = {
    isUnsolvableHelper(0,0)
  }

  def availableUpdate(lst: List[(Int,Int)], value: Int, map:Map[(Int,Int), List[Int]]): Map[(Int,Int), List[Int]] ={
   lst.length match{
      case 0 => map

      case _ => val board = new Board(map)
                val avail = board.availableValuesAt(lst.head._1, lst.head._2)
                val newAvail = avail.diff(List(value))
                if(avail.length > 1 && newAvail.length == 1){
                  val prs = Solution.peers(lst.head._1,lst.head._2)
                  val newMap: Map[(Int,Int), List[Int]] = map.updated((lst.head._1,lst.head._2), newAvail)
                  val newestMap = availableUpdate(prs, newAvail.head, newMap)
                  availableUpdate(lst.tail,value,newestMap.updated((lst.head), newAvail))
                }
                else 
                  availableUpdate(lst.tail, value, map.updated((lst.head), newAvail))
    } 
  }
  def place(row: Int, col: Int, value: Int): Board = {
      val prs = Solution.peers(row,col)
      val newMap = availableUpdate(prs,value, available.updated((row,col),List(value)))
      new Board(newMap)
  }
  
  def nextStatesHelper(x: Int, y: Int): List[Board] = {
    var lst: List[Board] = List()
    (y,x) match{
    case (8,8) => for(i <- 1 to 9){
                    if(availableValuesAt(y,x).contains(i)){
                      val board = place(y, x, i)
                      lst = List(board)
                    }
                    else 
                      lst = List()
                 }
                 lst
    case (_,8) => for(i <- 1 to 9){
                    if(availableValuesAt(y,x).contains(i)){
                      val board = place(y, x, i)
                      lst = List(board)
                    }
                  }
                  lst ++ nextStatesHelper(0,y+1)
    case _ =>     for(i <- 1 to 9){
                    if(availableValuesAt(y,x).contains(i)){
                      val board = place(y, x, i)
                      lst = List(board) 
                    }
                  }
                  lst ++ nextStatesHelper(x+1,y)
    }
  }
  def getNumberOfSolvedCells(board: Board, x: Int, y: Int): Int = (y,x) match{
    case (8,8) => if(board.availableValuesAt(y,x).length == 1)
                    1
                  else
                    0
    case (_,8) => if(board.availableValuesAt(y,x).length == 1)
                    1 + getNumberOfSolvedCells(board, 0, y+1)
                  else
                    getNumberOfSolvedCells(board, 0, y+1)
    case (_,_) => if(board.availableValuesAt(y,x).length == 1)
                    1 + getNumberOfSolvedCells(board, x+1, y)
                  else
                    getNumberOfSolvedCells(board, x+1, y)
  }
  def compareTo(board: Board): Int ={
    if(this.isUnsolvable && !board.isUnsolvable)
      -1
    else if(!this.isUnsolvable && board.isUnsolvable)
      1
    else if(getNumberOfSolvedCells(this, 0, 0) > getNumberOfSolvedCells(board, 0, 0))
      1
    else if(getNumberOfSolvedCells(this, 0, 0) < getNumberOfSolvedCells(board, 0, 0))
      -1
    else
      0
  }
  // You can return any Iterable (e.g., Stream)
  def nextStates(): List[Board] = {
    if(isUnsolvable()) {
      return List()
    }
    val lst = nextStatesHelper(0,0)
    val returnLst = lst.sortWith(_.compareTo(_) > 0)
    returnLst
  }

  def solve(): Option[Board] = {
    if(this.isSolved){
      return Some(this)
    }
    else if(this.isUnsolvable){
      return None
    }
    else{
      this.nextStates.foreach{curr => 
                              val solved = curr.solve
                              if(!solved.isEmpty)
                                  return solved
                              }
      None
    }                                                                     
  }
}












//------CODE GRAVEYARD-----------------//
/*def findValues(lst: List[(Int,Int)], board: Board): List[Int] = lst.length match{
    case 1 => if((board.valueAt(lst.head._1, lst.head._2) != None))
                List(board.valueAt(lst.head._1, lst.head._2).get)
              else
                List.empty
    case _ => if(board.valueAt(lst.head._1, lst.head._2) != None)
                List(board.valueAt(lst.head._1, lst.head._2).get) ++ findValues(lst.tail, board)
              else
                findValues(lst.tail, board)
  }
  def parseAvailible(map: Map[(Int,Int), List[Int]], x: Int, y: Int, board: Board): Map[(Int,Int), List[Int]] = (y,x) match{
    case (8,8) => val prs = findValues(peers(y,x), board).toSet
                  val avail = board.availableValuesAt(y,x).toSet
                  map.updated((y,x), (avail -- prs).toList)

    case _ => val prs = findValues(peers(y,x), board).toSet
              val avail = board.availableValuesAt(y,x).toSet
              if(x < 8)
                 parseAvailible(map.updated((y,x), (avail -- prs).toList), x+1, y, board)
              else 
                 parseAvailible(map.updated((y,x), (avail -- prs).toList), 0, y+1, board)

  }
  def parsePopulator(str: String, x: Int, y: Int):Map[(Int,Int), List[Int]] = str.length match{
    case 0 => val map:Map[(Int,Int), List[Int]] = Map.empty
              map
    case _ => if(str.head != '.'){
                val map: Map[(Int,Int), List[Int]] = Map( ((y,x), List((str.head.toInt)-48)) ) //minus 48 because .toInt gives ascii-- 0 being DEC48
                if(x < 8) 
                  map ++ parsePopulator(str.tail, x+1, y)
                else
                  map ++ parsePopulator(str.tail, 0, y+1)
              }
              else{
                if(x < 8) 
                  parsePopulator(str.tail, x+1, y)
                else
                  parsePopulator(str.tail, 0, y+1)
              }

  }
  */
 /*val popMap = parsePopulator(str,0,0)
    val availMap = parseAvailible(popMap, 0, 0, new Board(popMap))
    new Board(availMap)*/