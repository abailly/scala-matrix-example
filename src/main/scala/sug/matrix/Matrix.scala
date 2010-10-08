package sug.matrix

import sug.matrix.java.MatrixDouble
import Math.sqrt
import scala.actors.Actor
import Actor._
import scala.concurrent.MailBox

class Line[T](val cells : T*) {
  def !(that : T) : Line[T] = new Line((cells.toList :+ that):_*)
}

trait Lines {
  implicit def toLine[A](d : A) : Line[A] = new Line(d)
}

trait Matrices[A,M <: Matrix[A]] {
  def apply(data	 : Array[Array[A]]) : M
  def apply(lines	 : Line[A]*	  ) : M
}

trait RealMatrices {
  implicit def apply(data  : Array[Array[Double]]	) : RealMatrix = new RealMatrix(data)
  implicit def apply(lines : Line[Double]*	) : RealMatrix = apply(lines.map(_.cells.toArray).toArray)
}

trait BooleanMatrices  {
  implicit def apply(data  : Array[Array[Boolean]]) : BooleanMatrix = new BooleanMatrix(data)
  implicit def apply(lines : Line[Boolean]*	) : BooleanMatrix = apply(lines.map(_.cells.toArray).toArray)
}

trait MatricesOfMatrices  {
  implicit def apply(lines : Line[BooleanMatrix]*) : MatrixOfBooleanMatrices = new MatrixOfBooleanMatrices(lines.flatMap(_.cells).toArray)
  implicit def apply(lines : Line[RealMatrix]*) : MatrixOfRealMatrices = new MatrixOfRealMatrices(lines.flatMap(_.cells).toArray)
}

trait MValue[T] { 
  val unit : T
  def + (that : T) : T
  def * (that : T) : T
}

object MValues {

  implicit def toMValue(b : Boolean) : MValue[Boolean] = new MValue[Boolean] {
    type U = Boolean
    type S = Boolean 
    val unit = false
    def + (that : Boolean) : Boolean = b | that
    def * (that : Boolean) : Boolean = b & that
  }

  implicit def toMValue(d : Double) : MValue[Double] = new MValue[Double]{
    type U = Double
    val unit = 0.
    def + (that : Double) : Double = d + that
    def * (that : Double) : Double = d * that
  }

  implicit def toMValue(matrix : BooleanMatrix) : MValue[BooleanMatrix] = new  MValue[BooleanMatrix] {
    val unit = matrix.zero
    def + (that : BooleanMatrix) : BooleanMatrix = matrix + that
    def * (that : BooleanMatrix) : BooleanMatrix = matrix * that    
  }

  implicit def toMValue(matrix : RealMatrix) : MValue[RealMatrix] = new  MValue[RealMatrix] {
    val unit = matrix.zero
    def + (that : RealMatrix) : RealMatrix = matrix + that
    def * (that : RealMatrix) : RealMatrix = matrix * that    
  }

  implicit def toMValue[A](matrix : Matrix[A]) : MValue[Matrix[A]] = new  MValue[Matrix[A]] {
    val unit = matrix.zero
    def + (that : Matrix[A]) : Matrix[A] = matrix + that
    def * (that : Matrix[A]) : Matrix[A] = matrix * that    
  }

  def rng(ub : Int) = List.range(0,ub)

}

abstract class Matrix[A <% MValue[A]](val data : Array[A], val size : Int) {
  type M <: Matrix[A]

  val unit : A 
  def zero : M
  def factory(values : Array[A]) : M

  def + (that : M) : M =
    factory(for((a,b) <- data.zip(that.data)) yield a + b)

  def * (that : M) : M = {
    val res = zero
    for(i <- List.range(0,size); j <- List.range(0,size)) {
      var cell = unit
      for(k <- List.range(0,size))
        cell += data(i * size +k) * that.data(k * size +j)
      res.data(i * size +j) = cell
    }
    res
  }
  
  def * (scalar : A ) : M = map(( _ * scalar))

  def map(f : A => A) : M =  factory(data map f)

  override def equals(that : Any) : Boolean = data.deepEquals(that.asInstanceOf[M].data)
  
  override def toString : String = data.deepToString
}

import MValues._

class BooleanMatrix(dat : Array[Boolean], siz : Int) extends Matrix[Boolean](dat,siz) with BooleanMatrices  {
  type M = BooleanMatrix

  val unit = false
   
  def this(size : Int) = this(new Array(size * size), size)

  def this(matrix : Array[Array[Boolean]]) = {
    this(new Array(matrix.length * matrix.length), matrix.length) 
    for(i <- List.range(0,size); j <- List.range(0,size))
      data(i * size +j) = matrix(i)(j)
  }

  def zero : BooleanMatrix = new BooleanMatrix(new Array(size * size),size)

  def factory(values : Array[Boolean]) : BooleanMatrix = new BooleanMatrix(values,sqrt(values.length).asInstanceOf[Int])

}

class RealMatrix(dat : Array[Double], siz : Int) extends Matrix[Double](dat, siz) with RealMatrices {
  type M = RealMatrix

  val unit = 0.

  def this(size : Int) = this(new Array(size * size), size)

  def this(matrix : Array[Array[Double]]) = {
    this(new Array(matrix.length * matrix.length), matrix.length) 
    for(i <- List.range(0,size); j <- List.range(0,size))
      data(i * size +j) = matrix(i)(j)
  }

  def zero : RealMatrix = new RealMatrix(new Array(size * size), size)

  def factory(values : Array[Double]) : RealMatrix = new RealMatrix(values, sqrt(values.length).asInstanceOf[Int])

  def splitBy(cellSize : Int) : MatrixOfRealMatrices = {
    require(size % cellSize == 0)
    val numberOfBlocksPerDimension = size / cellSize
    val res : MatrixOfRealMatrices = new MatrixOfRealMatrices(numberOfBlocksPerDimension, cellSize)
    for (i <- 0 to numberOfBlocksPerDimension-1; 
	 l <- 0 to numberOfBlocksPerDimension-1) {
      val block = new RealMatrix(cellSize)
      for (j <- 0 to cellSize-1; k <- 0 to cellSize-1)
          block.data(j * cellSize + k) = data((i * cellSize + j) * size + (l * cellSize + k))
      res.data(i * numberOfBlocksPerDimension + l) = block
    }
    res
  }
}

class MatrixOfBooleanMatrices(dat : Array[BooleanMatrix], siz : Int, cellSize : Int) extends Matrix[BooleanMatrix](dat, siz) {
  require (dat.length > 0)
  type M = MatrixOfBooleanMatrices

  val unit = new BooleanMatrix(cellSize)

  def this(matrix : Array[BooleanMatrix]) = {
    this(matrix, sqrt(matrix.length).asInstanceOf[Int], matrix(0).size)
  }

  def zero : MatrixOfBooleanMatrices = new MatrixOfBooleanMatrices(dat.map(_ => unit), size, cellSize)

  def factory(values : Array[BooleanMatrix]) : MatrixOfBooleanMatrices = new MatrixOfBooleanMatrices(values, sqrt(values.length).asInstanceOf[Int], cellSize)
}


class MatrixOfRealMatrices(dat : Array[RealMatrix], siz : Int, cellSize : Int) extends Matrix[RealMatrix](dat, siz) {
  require (dat.length > 0)
  type M = MatrixOfRealMatrices

  val unit = new RealMatrix(cellSize)

  def this(size : Int, cellSize : Int) = {
    this(new Array(size * size),size, cellSize)
  }

  def this(matrix : Array[RealMatrix]) = {
    this(matrix, sqrt(matrix.length).asInstanceOf[Int], matrix(0).size)
  }

  def zero : MatrixOfRealMatrices = new MatrixOfRealMatrices(dat.map(_ => unit), size, cellSize)

  def factory(values : Array[RealMatrix]) : MatrixOfRealMatrices = new MatrixOfRealMatrices(values, sqrt(values.length).asInstanceOf[Int], cellSize)

  def |*| (that : MatrixOfRealMatrices) : MatrixOfRealMatrices = {
    case object done
    val result = new MailBox 

    val collector = new Actor { 
      val res = new MatrixOfRealMatrices(size, cellSize)
      val complete = new BooleanMatrix(size)
      def act() {
	react {
	  case (m : RealMatrix, x : Int, y : Int) => {
	    res.data(x * size + y) = m
	    complete.data(x * size + y) = true
	    if(complete.data.foldLeft(true)(_ && _)) 
	      result send res 
	    else 
	      act()
	  }
	}
      }
    }
    collector.start
    for (i <- 0 to size-1; j <- 0 to size-1) {
      val summer = new Actor {
	var init = new RealMatrix(that.data(0).size)
	def act() {
	  react {
	    case (a : RealMatrix,b: RealMatrix) => init += a * b; act()
 	    case done                           => collector ! (init,i,j)
	  }
	}
      }
      summer.start
      for(k <- 0 to size-1) {
	summer ! (this.data(i * size + k), that.data(k * size + j))
      }
      summer ! done
    }
    result receive {
      case m : MatrixOfRealMatrices => m
    }
  }

  def flatten = {
    val ln = size * cellSize
    val res = new RealMatrix(ln)
    for (i <- List.range(0,ln); j  <- List.range(0,ln)) { 
        val ib = i / cellSize
        val jb = j / cellSize
        val ri = i % cellSize
        val rj = j % cellSize
        res.data(i * ln + j) = data(ib * size + jb).data(ri * cellSize + rj);
    }
    res
  }
}

