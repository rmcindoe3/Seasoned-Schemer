package Chap13

import Chap11.Chap11._
import Chap12.Chap12._

object Chap13 {
  
  def main(args: Array[String]) {
    
    //Tests the results of our functions

    println(member(1, List(1,2,3,4,5)))
    println(member(1, List(2,3,4,5)))
    
    println(intersect(List(1,2,3,4,5), List(1,3,5)))
    println(intersect(List(1,2,4,5), List(1,3,5,7,8)))
    
    println(intersectAll(List(List(1,2,3,4,5),List(1,3,5),List(1,2,4,5), List(1,3,5,7,8))))
    
    println(rember(1, List(2,3,1,5,4)))
    println(remberBeyondFirst(1, List(2,3,1,5,4)))
    println(remberUpToLast(1, List(2,3,1,5,4)))
    println(remberUpToLast(1, List(2,3,1,5,1,4)))

  }
  
  //New version on member with ints
  def member(a: Int, lat: List[Int]) = {

    def iter(lat: List[Int]): Boolean = {
      if(lat.isEmpty) false
      else (lat.head == a) || iter(lat.tail)
    }

    iter(lat)
  }
  
  //Returns the intersection of the two given sets
  def intersect(set1: List[Int], set2: List[Int]) = {

    def iter(ret: List[Int], set1: List[Int]): List[Int] = {
      if(set1.isEmpty) ret.reverse
      else if(member(set1.head, set2)) iter(set1.head :: ret, set1.tail)
      else iter(ret, set1.tail)
    }

    //Optimization for when set2 is empty
    if(set2.isEmpty) Nil
    else iter(Nil, set1)
  }
  
  //Takes in a list of sets and returns the intersection of all sets
  def intersectAll(lset: List[List[Int]]) = {
    
    def iter(lset: List[List[Int]]): List[Int] = {
      //If any list in the list of sets is empty, then the intersection is Nil
      if(lset.head.isEmpty) Nil
      else if(lset.tail.isEmpty) lset.head
      else intersect(lset.head, iter(lset.tail))
    }
    
    if(lset.isEmpty) Nil
    else iter(lset)
  }
  
  //returns 'lat' without the first occurance of  'a'
  def rember(a: Int, lat: List[Int]) = {
    
    //Tail recursive version
    def iter(ret: List[Int], lat: List[Int]): List[Int] = {
      if(lat.isEmpty) ret.reverse
      else if(lat.head == a) ret.reverse ++ lat.tail
      else iter(lat.head :: ret, lat.tail)
    }
    
    iter(Nil, lat)
  }
  
  //Returns 'lat' with everything after the first occurance of 'a' removed
  def remberBeyondFirst(a: Int, lat: List[Int]) = {
    
    //Tail recursive version
    def iter(ret: List[Int], lat: List[Int]): List[Int] = {
      if(lat.isEmpty || lat.head == a) ret.reverse
      else iter(lat.head :: ret, lat.tail)
    }
    
    iter(Nil, lat)
  }
  
  //returns 'lat' with everything before the last occurance of 'a' removed
  def remberUpToLast(a: Int, lat: List[Int]) = {
    
    //Tail recursive version
    def iter(ret: List[Int], lat: List[Int]): List[Int] = {
      if(lat.isEmpty) ret.reverse
      else if(lat.head == a) iter(Nil, lat.tail)
      else iter(lat.head :: ret, lat.tail)
    }
    
    iter(Nil, lat)
  }

}