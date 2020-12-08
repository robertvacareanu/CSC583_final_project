package edu.arizona.cs

object ResultsStatsGenerator {

  def mrr(r: Seq[Int]): Double = r.map { 1/_.toDouble }.mean()

  def topOneScore(r: Seq[Int]): Double = r.count(_==1)/r.size.toDouble

  def topKScore(r: Seq[Int], k: Int): Double = r.count(_<=k)/r.size.toDouble

}
