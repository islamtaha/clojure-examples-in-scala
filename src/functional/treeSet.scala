package functional
import Closure4._

/**
 * @author eslam
 */
object Closure5 extends App{
  type Set = Node[Int]
  
  val emptySet = emptyTree
  val isEmptySet = isEmptyTree _
  def printSet[Int] = printTree[Int] _
  
  
  def contains(set: Set, elem: Int): Boolean = {
    if (isEmptyTree(set)) false
    else if (elem == value(set)) true
    else if (elem < value(set))
      contains(left(set).asInstanceOf[Set], elem)
    else 
      contains(right(set).asInstanceOf[Set], elem)
  }
  
  def addToSet(set: Set, elem: Int): Set = {
    if (isEmptySet(set)) leaf(elem)
    else if (elem == value(set)) set
    else if (elem < value(set))
      node(value(set), addToSet(left(set).asInstanceOf[Set], elem), right(set));
    else 
      node(value(set), left(set), addToSet(right(set).asInstanceOf[Set], elem));
  }
  
  def union(set1: Set, set2: Set): Set = {
    if(isEmptySet(set2)) set1
    else 
      union(union(addToSet(set1, value(set2)), left(set2).asInstanceOf[Set]), right(set2).asInstanceOf[Set])
  }  
  
  def intersection(set1: Set, set2: Set): Set = {
    if (isEmptySet(set1) || isEmptySet(set2)) emptySet
    
    val elem = value(set1)
    val rest = union(intersection(left(set1).asInstanceOf[Set], set2), intersection(right(set1).asInstanceOf[Set], set2))
    if(contains(set2, value(set1))){
      addToSet(rest, elem) 
    }else {
      rest
    }
  }
  
  
  
}