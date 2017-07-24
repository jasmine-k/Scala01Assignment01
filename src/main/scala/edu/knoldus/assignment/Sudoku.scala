package edu.knoldus.assignment

class Sudoku {

  def sudokuSolver(grid: Vector[Vector[Int]]): Boolean = {

    def solver(grid: Vector[Vector[Int]], row:Int, col:Int, prevVal:Int = 0): Boolean = {
      if (row == 9) {
        print(grid.map(_.mkString(" ")).mkString("\n")+"\n\n" )
        true
      }
      else if (col == 9) {solver(grid, row + 1, 0)}
      else {
        val (row,col) = findUnassignedLocation(grid)
        if(row == -1 && col == -1)  {
          true
        }
        else {
          val num = valueToAssign(grid, row, col,prevVal)
          if (num == -1) {
            false
          }
          else {
            val updatedGrid = grid.updated(row, grid(row).updated(col, num))
            if (solver(updatedGrid, row, col + 1)) true else {solver(grid, row, col, updatedGrid(row)(col))}
          }
        }
      }
    }
    solver(grid, 0,0)

  }

  private def valueToAssign(grid: Vector[Vector[Int]], row: Int, col: Int, prevVal:Int): Int = {

    val num = List.range(prevVal + 1,10)
    def assign(grid: Vector[Vector[Int]], num: List[Int], row: Int,col: Int): Int = {

      if(num.isEmpty){
       -1
      }
      else if(canAssign(grid, row, col, num.head)) {
        num.head
      }
      else {
        assign(grid, num.tail, row, col)
      }
    }
    assign(grid, num, row,col)

  }

  private def findUnassignedLocation(grid: Vector[Vector[Int]]): (Int,Int) = {

    def find(grid: Vector[Vector[Int]], row: Int, col: Int): (Int, Int) = {

      if(grid(row)(col) == 0) {
        (row, col)
      }
      else
      {
        if(row ==8 && col == 8) {
          (-1,-1)
        }
        else if(col == 8) {
          find(grid, row + 1, 0)
        }
        else {
          find(grid, row, col + 1)
        }
      }
    }
    find(grid,0,0)
  }

  private def canAssign(grid: Vector[Vector[Int]], row: Int, col: Int,num:Int): Boolean = {

    if(collisionInRow(grid,row,num) || collisionInCol(grid,col,num) || collisionInBox(grid,row-row%3,col-col%3,num)) {
      false
    }
    else {
      true
    }
  }

  private def collisionInRow(grid: Vector[Vector[Int]], row:Int, num:Int): Boolean = {

    if(grid(row).contains(num)) true else false
  }

  private def collisionInCol(grid: Vector[Vector[Int]], col:Int, num:Int): Boolean = {

    val colVector = grid.map(_(col))
    if(colVector.contains(num)) true else false
  }

  private def collisionInBox(grid: Vector[Vector[Int]], row:Int, col:Int, num:Int): Boolean = {

    def collision(grid: Vector[Vector[Int]], row:Int, col:Int, num:Int, i:Int, j:Int): Boolean = {

      if(j==3) {
        false
      }
      else if (i==3) {
        collision(grid, row, col, num, 0, j + 1)
      }
      else{
        if(grid(row + i)(col + j) == num) {
          true
        }
        else {
          collision(grid,row,col,num,i + 1,j)
        }
      }
    }
    collision(grid, row, col, num, 0, 0)

  }

}

object sudoku extends App {

  val sudokuObj = new Sudoku

  val grid = Vector(Vector(0,0,4,8,0,0,0,1,7), Vector(6,7,0,9,0,0,0,0,0), Vector(5,0,8,0,3,0,0,0,4),
    Vector(3,0,0,7,4,0,1,0,0), Vector(0,6,9,0,0,0,7,8,0), Vector(0,0,1,0,6,9,0,0,5),
    Vector(1,0,0,0,8,0,3,0,6), Vector(0,0,0,0,0,6,0,9,1), Vector(2,4,0,0,0,1,5,0,0))

  val grid1 = Vector(Vector(0,7,5,0,9,0,0,0,6), Vector(0,2,3,0,8,0,0,4,0), Vector(8,0,0,0,0,3,0,0,1),
    Vector(5,0,0,7,0,2,0,0,0), Vector(0,4,0,8,0,6,0,2,0), Vector(0,0,0,9,0,1,0,0,3),
    Vector(9,0,0,4,0,0,0,0,7), Vector(0,6,0,0,7,0,5,8,0), Vector(7,0,0,0,1,0,3,9,0))

  val success = sudokuObj.sudokuSolver(grid)
  if(!success) {
    print("No Solution Exists\n")
  }
}