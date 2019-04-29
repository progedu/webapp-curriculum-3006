import java.util.concurrent.{ForkJoinPool, RecursiveTask}

import scala.annotation.tailrec
import scala.util.Random

object ForkJoinMergeSort extends App {
  val length = 100
  val randomList = (for (i <- 1 to length) yield Random.nextInt(100)).toList
  println(randomList)

  val pool = new ForkJoinPool()

  class AggregateTask(list: List[Int]) extends RecursiveTask[List[Int]] {

    override def compute(): List[Int] = {
      val n = list.length / 2
      if (n == 0) {
        list
      } else {
        val (left, right) = list.splitAt(n)
        val leftTask = new AggregateTask(left)
        val rightTask = new AggregateTask(right)
        leftTask.fork()
        rightTask.fork()
        merge(leftTask.join(), rightTask.join())
      }
    }

    private[this] def merge(left: List[Int], right: List[Int]): List[Int] = {
      @tailrec
      def mergeRec(left: List[Int], right: List[Int], merged: List[Int]): List[Int] = {
        (left.isEmpty, right.isEmpty) match {
          case (true, true) => merged
          case (true, false) => mergeRec(left, right.tail, right.head :: merged)
          case (false, true) => mergeRec(left.tail, right, left.head :: merged)
          case _ => (left.head, right.head) match {
            case (l, r) if l < r => mergeRec(left.tail, right, l :: merged)
            case (_, r) => mergeRec(left, right.tail, r :: merged)
          }
        }
      }

      mergeRec(left, right, List()).reverse
    }

  }

  val sortedList = pool.invoke(new AggregateTask(randomList))
  println(sortedList)
}
