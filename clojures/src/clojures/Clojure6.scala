package clojures
import Clojure._
import Clojure3._

/**
 * @author eslam
 */
object Clojure6 extends App {
  type Set[T] = Pair[T]
  
  val emptySet = nil
  val isEmptySet = isEmptyList _
  
  def printSet = printRange _
  
  def addToSet[T](set: Set[T], elem: T) = {
    if (contains(set, elem)) set
    else prepend(elem, set)
  }
  
  def union[T, E, R](set1: Set[T], set2: Set[E]): Set[R] = {
    if(isEmptySet(set2)) set1.asInstanceOf[Set[R]] 
    else union(addToSet(set1, head(set2)), tail(set2).asInstanceOf[Set[E]])
  }
  
 
  def intersection[T, E, R](set1: Set[T], set2: Set[E]): Set[R]  = {
    if (isEmptySet(set1) || isEmptySet(set2))
      emptySet
    else if (contains(set2, head(set1)))
      prepend(head(set1), intersection(tail(set1).asInstanceOf[Set[T]], set2)).asInstanceOf[Set[R]]
    else
      intersection(tail(set1).asInstanceOf[Set[T]], set2)
  }
  

}