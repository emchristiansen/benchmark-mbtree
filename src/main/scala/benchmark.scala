package benchmark_mbtree

import mbtree._
import mbtree.extra._
import mbtree.Util._

import BenchmarkUtil._

// TODO: I think this can be refactored to pass abstract factories to Benchmark,
// but I don't know Scala well enough to get it to work. So I gave up and did
// things this way.

object RunAccuracy extends App {
  val data_filename = "data/letter-recognition.data"
  val hash_dimension = 64
  val num_permutations = 16

  type TestMetric = L2Vector

  def MakeTestMetric = (data: Seq[Double]) => new TestMetric(data: _*)
  def MakeTestNNFinder = 
      (data: IndexedSeq[TestMetric]) => new MetricLSHNN(data, hash_dimension, num_permutations)

  val data = { 
    val data = LoadDoubleData(data_filename).take(10000)
    random.shuffle(data.toIndexedSeq)
  }

  val accuracy = Accuracy(      
       MakeTestNNFinder,
       MakeTestMetric,
       data,
       3)

  println("accuracy: %.8f".format(accuracy))
}

object RunBenchmark extends App {
  val data_filename = "data/iris.data"
  type TestMetric = L2Vector
  // TODO: hide DistanceCounter
  type TestNNFinder = MBTreeNN[DistanceCounter[TestMetric]]

  def MakeTestMetric = (data: Seq[Double]) => new TestMetric(data: _*)
  def MakeTestNNFinder = 
      (data: IndexedSeq[DistanceCounter[TestMetric]]) => new TestNNFinder(data)

  val data = { 
    val data = LoadDoubleData(data_filename)
    random.shuffle(data.toIndexedSeq)
  }

  val (total_time, num_metric_evals) = Benchmark(      
      MakeTestNNFinder,
      MakeTestMetric,
      data, 
      3)

  println("total time, num metric evals: %.8f, %d".format(total_time, num_metric_evals))
}


