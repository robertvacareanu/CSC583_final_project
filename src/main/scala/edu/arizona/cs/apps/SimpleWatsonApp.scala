package edu.arizona.cs.apps

import java.io.File
import java.util.Calendar

import com.typesafe.config.ConfigFactory
import edu.arizona.cs.indexing.{ProcessingFields, SmartTextPreprocessing, TextPreprocessing}
import edu.arizona.cs.model.Question
import org.apache.lucene.index.{DirectoryReader, IndexWriterConfig, Term}
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.{BooleanQuery, IndexSearcher, Query, TermQuery}
import org.clulab.processors.clu.CluProcessor
import edu.arizona.cs.{EnhancedNumericCollection, EnhancedResource, EnhancedType, ResultsStatsGenerator, using}
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.search.similarities.ClassicSimilarity
import org.apache.lucene.store.FSDirectory
import org.clulab.dynet.Utils

import scala.io.Source

object SimpleWatsonApp extends App {
  Utils.initializeDyNet()
  val c = ConfigFactory.load()
  private lazy val proc = new CluProcessor(c)
  //  println(proc.mtl.model.parameters.parametersList().map(it => it.values.toSeq().take(5)))
  //  System.exit(1)
  val tp: TextPreprocessing = new SmartTextPreprocessing(proc)

  def buildQuery(q: Question): Query = {
    val text = new BooleanQuery.Builder().let { it =>
      tp(q.question, ProcessingFields.Body).foreach { s =>
        it.add(new TermQuery(new Term("text", s)), Occur.SHOULD)
      }
      it.build()
    }
    text
  }

  def readQuestions(path: String): Seq[Question] = {
    using(Source.fromFile(path)) { it =>
      it.getLines().sliding(4, 4).map { sliding =>
        Question(sliding.head, sliding(1), sliding(2))
      }.toList
    }
  }

  val indexPath: String = c.getString("csc583.data.indexSimple")
  val analyzer = new StandardAnalyzer
  val index = FSDirectory.open(new File(indexPath).toPath)
  val config = new IndexWriterConfig(analyzer)
  val reader = DirectoryReader.open(index)
  val is = new IndexSearcher(reader)

  def query(question: Question): Int = {
    val result = is.search(buildQuery(question), Int.MaxValue).scoreDocs
    val answer = (if(question.answer.contains("""|""")) question.answer.split("""|""").toSeq else Seq(question.answer)).map(_.toLowerCase())
    val position = result.map { it => (is.doc(it.doc).get("name"), it.score) }.map { it => answer.contains(it._1.toLowerCase) }.indexOf(true)
    if(position == -1) {
      Int.MaxValue
    } else {
      position + 1
    }
  }


  println(f"${Calendar.getInstance().getTime}")

  readQuestions("questions.txt")
      .par
      .map(query)
      .toList
//      .zipWithIndex
      .let { it =>
        println(it);
        println(f"\tmrr - ${ResultsStatsGenerator.mrr(it)}")
        println(f"\tP@1 - ${ResultsStatsGenerator.topOneScore(it)}")
        println(f"\tP@3 - ${ResultsStatsGenerator.topKScore(it, 3)}")
        println(f"\tP@5 - ${ResultsStatsGenerator.topKScore(it, 5)}")
        println(f"\tP@10- ${ResultsStatsGenerator.topKScore(it, 10)}")
        println(f"\tP@25- ${ResultsStatsGenerator.topKScore(it, 25)}")
        println(f"\tP@50- ${ResultsStatsGenerator.topKScore(it, 50)}")
        println("\n\n\n")
      }
  println(f"${Calendar.getInstance().getTime}")

  analyzer.close()
  index.close()
  reader.close()
}
