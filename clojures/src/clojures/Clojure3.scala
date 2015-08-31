package clojures

import Clojure._

/**
 * @author eslam
 */
object Clojure3 extends App{
  
  def nil() = { null }
  
   
  def isEmptyList(l: Any): Boolean = {
    l == nil
  }
  
  def range[T](a: Int, b: Int): Any = a match {
    case h if h == b => singlePair(h, nil) 
    case _           => singlePair(a, range(a+1, b))
  }                                                 

  def last[T](list: Any): T = {
    var l = list.asInstanceOf[Pair[T]]
    if (isEmptyList(tail(l))) head(l) else last(tail(l).asInstanceOf[Pair[T]])
  }
  
  def prepend[T](a: T, b: Pair[T]): Pair[Pair[T]] = b match {
    case y: Pair[T] => singlePair(a, y)
    case _ => throw new Error()
  }
  
  
  def append[T](list: Any, elem: T): Any = {
    val l = list.asInstanceOf[Pair[T]]
    if(isEmptyList(l)) singlePair(elem, l)
    else singlePair(head(l), append(tail(l).asInstanceOf[Pair[T]], elem))
  }
    
 
  def reverse[T](list: Any): Any = {
    val l = list.asInstanceOf[Pair[T]]
    if(isEmptyList(l)) nil
    else append(reverse(tail(l)), head(l))
  }
    
     
  def get[T](list: Any, i: Int): T = {
    val l = list.asInstanceOf[Pair[T]]
    def getIter(l: Pair[T], count: Int): T = {
      if(i == count) head(l)
      else getIter(tail(l).asInstanceOf[Pair[T]], count+1)
    }
    getIter(l, 0)
  }
    
  
  def lenght[T](list: Any): Int = {
    val l = list.asInstanceOf[Pair[T]]
    def lenIter(li: Pair[T], count: Int): Int = {
      if(isEmptyList(li)) count
      else lenIter(tail(li).asInstanceOf[Pair[T]], count+1)
    }
    lenIter(l, 0)
  }
  
  def contains[T](list: => Any, elem: T): Boolean =  {
    val l = list.asInstanceOf[Pair[T]]
    if (isEmptyList(l)) false
    else if (head(l) == elem) true
    else contains(tail(l), elem)
 }
  
  
  def merge[T, E, R](list1: Any, list2: Any): Pair[R] = {
    val l1 = list1.asInstanceOf[Pair[T]]
    val l2 = list2.asInstanceOf[Pair[E]]
    if (isEmptyList(l2)) l1.asInstanceOf[Pair[R]]
    else merge(append(l1,head(l2)),tail(l2))
  }
  
  
  def map[T, R](list: Any, fun: Function[T, R]): Pair[R] = {
    val l = list.asInstanceOf[Pair[T]]
    if (isEmptyList(l)) nil.asInstanceOf[Pair[R]]
    else singlePair(fun(head(l)), map(tail(l),fun))
  }

  
  def filter[T, R](list: Any, pred: Function[T, Boolean]): Pair[R]={
    val l = list.asInstanceOf[Pair[T]]
    if (isEmptyList(l)) nil.asInstanceOf[Pair[R]]
    else
        if(pred(head(l))) singlePair(head(l), filter(tail(l), pred))
        else filter(tail(l), pred)
  }
  
  
  def reduce[T, R](initial: Any, list: Any, fun: Function2[Pair[R], T ,Pair[R]]): Pair[R] = {
    val l = list.asInstanceOf[Pair[T]]
     if(isEmptyList(l)) initial.asInstanceOf[Pair[R]] 
     else fun(reduce(initial, tail(l), fun), head(l))
  }

  def map2[T, R](list: Any, fun: Function[T, R]): Pair[R] = {
    val l = list.asInstanceOf[Pair[T]]
    reduce(nil, l, (initial: Pair[R], elem: T) => singlePair(fun(elem),initial))
  }

 
  def printList[E](l: Pair[E]) =  {      
      def listStr(list: Any): String = list match {
        case li: Pair[E] =>   listStr(head(li)) + ", " + listStr(tail(li))
        case u => u.toString() 
      }
      var output = "(" + listStr(l) + ")"
      println(output)
  }
 
  
  def printRange(ra: Any) = {
    def rangeStr[T](range: Any): String =  {
      var r = range.asInstanceOf[Pair[T]]
      if(isEmptyList(r)) "Nil"
      else head(r).toString() + ", " + rangeStr(tail(r))
    }
    var output = "(" + rangeStr(ra) + ")"
    println(output)
  }
  
 
}