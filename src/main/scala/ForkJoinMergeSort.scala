import scala.util.Random

object ForkJoinMergeSort extends App {
  val length = 100
  val randomList = (for (i <- 1 to length) yield Random.nextInt(100)).toList
  println(randomList)


  val sortedList = ???
  println(sortedList)
}