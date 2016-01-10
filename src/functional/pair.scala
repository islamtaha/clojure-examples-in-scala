package functional

/**
 * @author eslam
 */
object Closure extends App{
  
  type Pair[+T] = Int => T
  
  def singlePair[H, T, A](a: H, b: T): Pair[A] = {
   ( (i: Int) => if(i == 0) a.asInstanceOf[A]  else b.asInstanceOf[A] )
  }
  
  def head[T](p: Pair[T]): T = p(0)
  
  def tail[T](p: Pair[T]): T = p(1)
  
  def containsPair[T](p: Pair[T], elem: T): Boolean = if(p(0) == elem || p(1) == elem) true else false
  
  def printPair[T](x: Pair[T]): Unit = {
     val output = "(" + x(0).toString() + ", " + x(1).toString() + ")"
       println(output)
  }
  
  
}