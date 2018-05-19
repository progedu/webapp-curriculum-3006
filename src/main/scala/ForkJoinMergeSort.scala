import java.util.concurrent.{ForkJoinPool, RecursiveTask}

import scala.util.Random

object ForkJoinMergeSort extends App {
  val length = 100
  val randomList = (for (i <- 1 to length) yield Random.nextInt(100)).toList
  println(randomList)

  val pool = new ForkJoinPool()

  class MergesortTask(list: List[Int]) extends RecursiveTask[List[Int]] {
    override def compute(): List[Int] = {
      val n = list.length / 2
      if (n == 0) {
        list
      } else {
        val (left, right) = list.splitAt(n)
        val leftTask = new MergesortTask(left)
        val rightTask = new MergesortTask(right)
        leftTask.fork()
        rightTask.fork()
        merge(leftTask.join(), rightTask.join())
      }
    }
  }

  private[this] def merge(left: List[Int], right: List[Int]): List[Int] = {
    if (left.isEmpty) right
    else if (right.isEmpty) left
    else if (left.head < right.head) left.head :: merge(left.tail, right)
    else right.head :: merge(left, right.tail)
  }

  val sortedList = pool.invoke(new MergesortTask(randomList))
  println(sortedList)
}