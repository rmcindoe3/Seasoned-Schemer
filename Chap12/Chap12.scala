package Chap12

import Chap11.Chap11._

object Chap12 {

  def main(args: Array[String]) {
    
    //Tests output of the methods

    println(multirember(1, List(1,2,1,3,1,5,1,7)))
    println(multirember(3, List(1,2,1,3,1,5,1,7)))
    println(multirember(8, List(1,2,1,3,1,5,1,7)))
    
    println(newMember(1, List(0,1,2,3,4,5)))
    println(newMember(6, List(0,1,2,3,4,5)))

    println(union(List(1,8,3,2,5,7), List(2,4,6,8,9)).sorted)

  }
  
  //Removes all occurances of 'a' from 'lat' and returns the result
  def multirember(a: Int, lat: List[Int]) = {
    
    //Tail recursive implementation of algorithm from book
    def mr(acc: List[Int], lat: List[Int]): List[Int] = {
      if(lat.isEmpty) acc.reverse
      else if(lat.head == a) mr(acc, lat.tail)
      else mr(lat.head :: acc, lat.tail)
    }
    
    mr(Nil, lat);
  }
  
  //New implementation of member? function which determines if 
  // 'a' is a member of the list 'lat'
  def newMember(a: Int, lat: List[Int]) = {
    
    def letrec(lat: List[Int]): Boolean = {
      if(lat.isEmpty) false
      else (lat.head == a) || letrec(lat.tail)
    }
    
    letrec(lat)
  }
  
  //Performs a union of the two given lists.  We're under the assumption
  // that the List's given are actually sets.
  def union(set1: List[Int], set2: List[Int]): List[Int] = {
    
    def letrec(set1: List[Int]): List[Int] = {
      if(set1.isEmpty) set2
      else if(newMember(set1.head, set2)) letrec(set1.tail)
      else set1.head :: letrec(set1.tail)
    }

    letrec(set1)
  }
  
  /* The remaining functions described in chapter 12 are 
   * the same as the functions I wrote for chapter 11.  I had
   * already hidden the functions inside outer functions in my
   * original definitions.
   */

}