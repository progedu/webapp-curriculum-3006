import java.util.concurrent.{ForkJoinPool, RecursiveTask}

import scala.util.Random

object ForkJoinMergeSort extends App {

  val length = 100
  val randomList = (for (i <- 1 to length) yield Random.nextInt(100)).toList
  println(randomList)

  val pool = new ForkJoinPool()

  class MergeSortTask(list: List[Int]) extends RecursiveTask[List[Int]] {
    override def compute(): List[Int] = {
      val n = list.length / 2
      if (n == 0) list
      else {
        val (left, right) = list.splitAt(n)
        val leftTask = new MergeSortTask(left)
        val rightTask = new MergeSortTask(right)
        leftTask.fork()
        rightTask.fork()

        val mergeTask = new MargeTask(leftTask.join(), rightTask.join())
        mergeTask.fork()
        mergeTask.join()
      }
    }
  }

  class MargeTask(left: List[Int], right: List[Int]) extends RecursiveTask[List[Int]] {

    override def compute(): List[Int] = {
      (left, right) match {
        case (left, List()) => left
        case (List(), right) => right
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (leftHead < rightHead) {
            val mergeTask = new MargeTask(leftTail, right)
            mergeTask.fork()
            leftHead :: mergeTask.join()
          } else {
            val mergeTask = new MargeTask(left, rightTail)
            mergeTask.fork()
            rightHead :: mergeTask.join()
          }
      }
    }

  }

  val sortedList = pool.invoke(new MergeSortTask(randomList))
  println(sortedList)
}