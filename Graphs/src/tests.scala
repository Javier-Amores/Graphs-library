object test extends App {

var d = Map(1->Set(2),2->Set(1,3),3->Set(2,3,5))
  d = d + (3->Set())
  println(d(1))
  println(d.keySet)
  println(d.values.toList)


/*
      var myMatrix = Array.ofDim[Boolean](3,3)

      // build a matrix
      for (i <- 0 to 2) {
        for ( j <- 0 to 2) {
         if (i==j) myMatrix(i)(j) = true else myMatrix(i)(j) = false
        }
      }

      // Print two dimensional array
      for (i <- 0 to 2) {
        for ( j <- 0 to 2) {
          print(" " + myMatrix(i)(j));
        }
        println();
      }
*/

var s = Set(1,2,3)
  println(s)
  s = s+4
  println(s)
  s = s-2
  println(s)


}