package clojures

import Clojure._
import Clojure2._
/**
 * @author eslam
 */
object Clojure4 extends App {
  type Node[T] = Triple[T]
  
  
  def node[A, B, C] = triple[A, B, C] _
  val emptyTree = null
  
  def value[T] = first[T] _
  def left[T] = second[T] _
  def right[T] = third[T] _
  
  def leaf[T](value: T): Node[T] = {
    node(value, emptyTree, emptyTree)
  }
  
  def isEmptyTree(tree: Any): Boolean = {
    tree == null
  }
  
  def isLeaf[T](node: Node[T]): Boolean = {
    isEmptyTree(left(node)) && isEmptyTree(right(node))
  }
  
  def lenght[T](tree: Any): Int = {
    val t = tree.asInstanceOf[Node[T]]
    if(isEmptyTree(t)) 0
    else 1 + lenght(left(t)) + lenght(right(t))
  }
  
  def printTree[E](tree: Node[E]) = {
    def treeStr[T](tr: Any): String = tr match {
        case t: Node[E] => "{" + treeStr(left(t)) +"<-" + treeStr(value(t)) +"->" +treeStr(right(t)) + "}"
        case x if isEmptyTree(x)  => "."
        case u => u.toString() 
      }
    println(treeStr(tree))
  }
  
  
}