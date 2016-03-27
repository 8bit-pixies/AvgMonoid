/**
 * Created by Chapman on 5/04/2015.
 */

import scalaz._
import Scalaz._

object Avg {
  def main(args: Array[String]){

    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }

    object avgMonoid extends Monoid[(Double, Int)] {
      /* The Set is a tuple where first arg is the average and the 2nd argument 
         is the number of elements averaged.
         The identity is (0,0)
      */
      def mappend(a1: (Double, Int), a2: (Double, Int)): (Double, Int) = (((a1._1*a1._2)+(a2._1*a2._2))/(a1._2+a2._2), a1._2+a2._2)
      def mzero : (Double, Int) = (0.0, 0)
    }
    
    val x: (Double, Int) = avgMonoid.mappend((0,0), (10,2))
    
    println(x)

    val xs: List[(Double, Int)] = List((10,2), (5,3), (5,3), (4.2,1))
    val xy = xs.fold(avgMonoid.mzero)(avgMonoid.mappend)
    
    println(xy)
    
    val l: List[Double] = List(1,2,3,4,5,6)
    val xx: List[(Double, Int)] = l.map(x => (x, 1))
    println(xx.fold(avgMonoid.mzero)(avgMonoid.mappend))
    


  }
}