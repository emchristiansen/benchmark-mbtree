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

  def LoadDoubleData(path: String): List[IndexedSeq[Double]] = { 
    // Ignore empty lines.
    val lines = Resource.fromFile(path).lines().filter(_.size > 0).toList

    val Pattern = """(^|[ ,\t\n]+)([0-9\.]+)($|[ ,\t\n]+)""".r
    val data = for (line <- lines) yield {
      val list = for (number <- Pattern.findAllIn(line)) yield number.toDouble
      list.toIndexedSeq
    }

    // All data must have the same dimension.
    for (d <- data) { 
      assert(data(0).size == d.size)
    }
    
    data
  }

  def AccuracySingle[T <: Metric[T]](
      make_finder: (IndexedSeq[T]) => NNFinder[T],
      make_metric: (Seq[Double]) => T,
      test_data: IndexedSeq[Seq[Double]],
      train_data: IndexedSeq[Seq[Double]]): Double = {
    val test: IndexedSeq[T] = test_data.map(make_metric)
    val train: IndexedSeq[T] = train_data.map(make_metric)
    
    println("training")
    val finder = make_finder(train)
    println("testing")
    val brute = new BruteNN(train)

    val scores = for (t <- test) yield { 
      val guess = train(finder.FindNearest(t)._1).Distance(t)
      val truth = train(brute.FindNearest(t)._1).Distance(t)
      if (guess == truth) 1.0 else 0.0
    }
    
    scores.sum / scores.size.toDouble
  }

  def Accuracy[T <: Metric[T]](
      make_finder: (IndexedSeq[T]) => NNFinder[T],
      make_metric: (Seq[Double]) => T,
      data: IndexedSeq[Seq[Double]],
      fold_depth: Int): Double = { 
    val folds = BreakIntoFolds(data, fold_depth)

    val scores = folds.map(f => AccuracySingle(make_finder, make_metric, f._1, f._2))
    scores.sum / scores.size.toDouble
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
