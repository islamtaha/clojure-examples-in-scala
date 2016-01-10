package functional

import Closure._
/**
 * @author eslam
 */
object Closure2 extends App{
   
  type Triple[T] = Pair[Pair[T]]
  
  def triple[A, B, C](first:A , second:B, third:C) = {
    singlePair(first, singlePair(second, third))
  }     
  
  def first[T](triple: Triple[T]) = {
    head(triple.asInstanceOf[Pair[T]])
  }   
  
  def second[T](triple: Triple[T]) = {
    head(tail(triple))
  }
  
  def third[T](triple: Triple[T]) = {
    tail(tail(triple))
  } 
  
  def printTriple[T](x: Triple[T]): Unit = {
     val output = "(" + first(x).toString() + ", " + second(x).toString() + ", " + third(x).toString() + ")"
       println(output)
  }
   
}