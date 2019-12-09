import scala.util.Random
import java.util.concurrent.{ForkJoinPool,RecursiveTask}

object ForkJoinMergeSort extends App {
  val length = 100
  val randomList = (for (i <- 1 to length) yield Random.nextInt(100)).toList
  println(randomList)
  val pool = new ForkJoinPool()

  class AggregateSortTask(list:List[Int]) extends RecursiveTask[List[Int]] {
    override def compute(): List[Int] = {
      val n = list.length / 2
      if (n == 0) list
      else {
        val (left, right) = list.splitAt(n)
        val leftTask = new AggregateSortTask(left)
        val rightTask = new AggregateSortTask(right)
        leftTask.fork()
        rightTask.fork()
        val task = new MergeTask(leftTask.join(), rightTask.join())
        task.fork()
        task.join()
      }
    }
  }
  
  class MergeTask(left:List[Int], right:List[Int]) extends RecursiveTask[List[Int]] {
    override def compute(): List[Int] = {
      (left, right) match {
        case (left, List()) => left
        case (List(), right) => right
        case (lHead :: lTail, rHead :: rTail) => {
          if (lHead < rHead) {
            val tail = new MergeTask(lTail, right)
            tail.fork
            lHead :: tail.join()
          } else{
            val tail = new MergeTask(left, rTail)
            tail.fork
            rHead :: tail.join()
          }
        }
      }
    }
  }

  val sortedList = pool.invoke(new AggregateSortTask(randomList))
  println(sortedList)
}