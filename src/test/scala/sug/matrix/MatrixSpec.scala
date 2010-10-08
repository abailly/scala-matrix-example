package sug.matrix

import org.specs.runner.JUnit4
import org.specs._
import org.specs.mock.Mockito
import org.mockito.Matchers._

object MatrixSpec extends Specification with SystemContexts with Mockito {

  object Matrix extends Lines with RealMatrices with BooleanMatrices with MatricesOfMatrices
  import Matrix._

  val m	= Matrix(Array(Array(2.,3.),Array(0.,1.)))
  val zero	= Matrix(Array(Array(0.,0.),Array(0.,0.)))
  val one	= Matrix(Array(Array(1.,0.),Array(0.,1.)))
  val mplusm2 = Matrix(Array(Array(3.,3.),Array(0.,2.)))
  val mplus1  = Matrix(Array(Array(3.,4.),Array(1.,2.)))
  val mpower  = Matrix(Array(Array(8.,4.),Array(2.,1.)))
  val mlog    = Matrix(Array(Array(2.0794415416798357,1.3862943611198906),
			     Array(0.6931471805599453,0.0)))
  "A scala matrix" should {

    
    "provide operator for addition" in {
      (m + zero) must be_==(m)
      (m + one) must be_==(mplusm2)
    }

    "provide operator '*'" in {
      (m * one) must be_==(m)      
    }

    "can be multiplied by a scalar" in {
      val deuxm = Matrix(Array(Array(4.,6.),Array(0.,2.)))
      (m * 2) must be_==(deuxm)
    }

    "can map functions on the matrix" in {
      val plus1 = { d : Double => d + 1 }
      m.map(plus1) must be_==(mplus1)
      mpower.map(Math.log) must be_==(mlog)      
    }

    "be constructed with 'nice' syntax" in {
       val m = Matrix( 2. ! 3., 0. ! 1.)
       (m + m) must be_==(m * 2)
    }

     "allow for matrices of booleans" in {
        val m = Matrix( true ! false , false ! true)
        (m + m * true) must be_==(m * true)      
     }
  }
  
  "A matrix of matrices" should {

    val m5 = Matrix(m ! zero , mplus1 ! mplusm2)
    
    "allow matrices of matrices of Booleans" in {
      val m1 = Matrix( true ! false , false ! true)
      val m2 = Matrix( false ! false , false ! false)
      val m3 = Matrix( false ! false , false ! false)
      val m4 = Matrix( true ! false , false ! true)
      val m5 = Matrix(m1 ! m2 , m3 ! m4)
      (m5 * m5) must be_==(m5)
    }

    "allow matrices of matrices of Doubles" in {
      val m6 = Matrix(Matrix(4.0 ! 9.0, 0.0 ! 1.0) ! zero,
		      Matrix(18.0! 31.0, 4.0! 9.0) ! Matrix(9.0 ! 15.0, 0.0 ! 4.0))
      (m5 * m5) must be_==(m6)
    }

    "be flattened" in {
      val m7 = Matrix(2. ! 3. ! 0. ! 0.,
		      0. ! 1. ! 0. ! 0.,
		      3. ! 4. ! 3. ! 3.,
                      1. ! 2. ! 0. ! 2.)
      (m5 * m5).flatten must be_==(m7 * m7)
    }

    "be constructed from splitting a matrix" in {
      val m7 = Matrix(2. ! 3. ! 0. ! 0.,
		      0. ! 1. ! 0. ! 0.,
		      3. ! 4. ! 3. ! 3.,
                      1. ! 2. ! 0. ! 2.)
      m7 splitBy 2  must be_==(m5)
    }

    "allow computing multiplication in parallel" in {
      val m7 = Matrix(2. ! 3. ! 0. ! 0.,
		      0. ! 1. ! 0. ! 0.,
		      3. ! 4. ! 3. ! 3.,
                      1. ! 2. ! 0. ! 2.)
      val m8 = Matrix(2. ! 3. ! 0. ! 4.,
		      0. ! 1. ! 1. ! 0.,
		      2. ! 4. ! 3. ! 3.,
                      1. ! 2. ! 1. ! 0.)
      
      ((m7 splitBy 2) |*| (m8 splitBy 2)).flatten must be_==(m7 * m8)
    }
  }

  "A MValue" should {

    "support implicit conversions" in {
      import MValues._
      (true + false) must be_==(true)
      (true * false) must be_==(false)
    }
    
    "be a matrix of booleans" in {
      import MValues._
      val m = Matrix( true ! false , false ! true)
      (m + m * true) must be_==(m * true)      
    }
  }

}


class MatrixTest extends JUnit4(MatrixSpec)

