package benchmark_mbtree

import scalax.io._

import mbtree._
import mbtree.Util._

object BenchmarkUtil { 
  final case class DistanceCounter[T <: Metric[T]](val metric: T) extends Metric[DistanceCounter[T]] { 
    var num_metric_evaluations = 0

    def Distance(that: DistanceCounter[T]): Double = { 
      num_metric_evaluations += 1
      metric.Distance(that.metric)
    }
  }

  def LoadDoubleData(path: String): List[List[Double]] = { 
    // Ignore empty lines.
    val lines = Resource.fromFile(path).lines().filter(_.size > 0).toList

    val Pattern = """[0-9\.]+""".r
    for (line <- lines) yield {
      for (number <- Pattern.findAllIn(line).toList) yield number.toDouble
    }
  }

  def BenchmarkSingle[T <: Metric[T]](
      make_finder: (IndexedSeq[DistanceCounter[T]]) => NNFinder[DistanceCounter[T]],
      make_metric: (Seq[Double]) => T,
      test_data: IndexedSeq[Seq[Double]],
      train_data: IndexedSeq[Seq[Double]]): (Double, Int) = {
    val test: IndexedSeq[DistanceCounter[T]] = test_data.map(make_metric).map(x => DistanceCounter(x))
    val train: IndexedSeq[DistanceCounter[T]] = train_data.map(make_metric).map(x => DistanceCounter(x))
    
    println("training")
    val finder = make_finder(train)
    // We don't want to measure metric evals that occur during training.
    train.foreach(_.num_metric_evaluations = 0)

    println("testing")
    val start = System.nanoTime
    test.foreach(t => finder.FindNearest(t))
    val seconds = (System.nanoTime - start).toDouble / 1000000000
    (seconds, test.map(_.num_metric_evaluations).sum + train.map(_.num_metric_evaluations).sum)
  }

  def Benchmark[T <: Metric[T]](
      make_finder: (IndexedSeq[DistanceCounter[T]]) => NNFinder[DistanceCounter[T]],
      make_metric: (Seq[Double]) => T,
      data: IndexedSeq[Seq[Double]],
      fold_depth: Int): (Double, Int) = { 
    val folds = BreakIntoFolds(data, fold_depth)

    val scores = folds.map(f => BenchmarkSingle(make_finder, make_metric, f._1, f._2))
    (scores.map(_._1).sum, scores.map(_._2).sum)
  }
}
