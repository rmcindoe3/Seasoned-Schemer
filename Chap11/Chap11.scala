package Chap11

object Chap11 {

  def main(args: Array[String]) {

    //Tests output of the methods

    println(member("sardines", List("Italian", "sardines", "spaghetti", "parsley")))
    println(member("sardines", List("Italian", "rigatoni", "spaghetti", "parsley")))
    
    println(twoInARow(List("Italian", "sardines", "sardines", "parsley")))
    println(twoInARow(List("Italian", "spaghetti", "sardines", "parsley")))
    
    println(sumOfPrefixes(List(2, 1, 9, 17, 0)))
    println(sumOfPrefixes(List(1, 1, 1, 1, 1)))
    
    println(pick(2, List(0,1,2,3,4,5)))
    println(pick(5, List(0,1,2,3,4,5)))
    
    println(scramble(List(0,1,2,3,4,5,6,7,8,9)))
    println(scramble(List(0,0,0,2,3,1,0,0,8,1)))

  }
  
  //Determines whether 'a' is a member of 'lat'
  def member(a: String, lat: List[String]): Boolean = {
    if(lat.isEmpty) false
    else (a == lat.head) || member(a, lat.tail)
  }
  
  //Determines whether a string occurs twice in a row in the 'lat'
  def twoInARow(lat: List[String]) = {
    
    def twoInARowB(preceding: String, lat: List[String]): Boolean = {
      if(lat.isEmpty) false
      else (preceding == lat.head) || twoInARowB(lat.head, lat.tail) 
    }
    
    twoInARowB(null, lat)
    
    /* My Original Solution:
    if(lat.isEmpty || lat.tail.isEmpty) false
    else (lat.head == lat.tail.head) || twoInARow(lat.tail) */
  }
  
  //Creates a list of integers that is the sum of the prefixes of the given list
  def sumOfPrefixes(lat: List[Int]) = {
    
    def sumOfPrefixesB(sum: Int, lat: List[Int]): List[Int] = {
      if(lat.isEmpty) Nil
      else (sum + lat.head) :: sumOfPrefixesB(sum + lat.head, lat.tail)
    }
    
    sumOfPrefixesB(0, lat)
  }

  //Returns the 'n'th element of 'lat'
  def pick(n: Int, lat: List[Int]): Int = {
    if(n == 0) lat.head
    else pick(n - 1, lat.tail)
  }
  
  //Takes a non empty list of integers where each number is no greater
  // than it's index in the list and returns a list of integers where
  // each integer in the argument is treated as the backwards index
  // from its own position in the list.
  def scramble(lat: List[Int]) = {
    
    def scrambleB(prev: List[Int], lat: List[Int]): List[Int] = {
      if(lat.isEmpty) Nil
      else pick(lat.head, lat.head :: prev) :: scrambleB(lat.head :: prev, lat.tail)
    }
    
    scrambleB(Nil, lat)
  }

}