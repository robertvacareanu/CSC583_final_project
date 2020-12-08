package edu.arizona.cs.apps

import java.io.File
import java.util.Calendar

import com.typesafe.config.ConfigFactory
import edu.arizona.cs.apps.SimpleWatsonApp.{proc, query}
import edu.arizona.cs.apps.SmartIndexWikiApp.proc
import org.apache.lucene.search.PhraseQuery
import org.apache.lucene.search.similarities.ClassicSimilarity
import org.clulab.dynet.Utils
//import edu.arizona.cs.apps.WatsonApp.{analyzer, extractChunks, extractEntities, index, indexPath, is, readQuestions, reader, tp}
import edu.arizona.cs.indexing.{ProcessingFields, SmartTextPreprocessing, TextPreprocessing}
import edu.arizona.cs.model.Question
import org.apache.lucene.index.{DirectoryReader, IndexWriterConfig, Term}
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.{BooleanQuery, BoostQuery, IndexSearcher, Query, TermQuery}
import org.clulab.processors.clu.CluProcessor
import edu.arizona.cs.{EnhancedNumericCollection, EnhancedResource, EnhancedType, ResultsStatsGenerator, using}
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.store.FSDirectory
import org.clulab.processors.Document

import scala.collection.mutable
import scala.io.Source

object SmarterWatsonApp extends App {
  Utils.initializeDyNet()

  val c = ConfigFactory.load()
  private lazy val proc = new CluProcessor(c)

  val tp: TextPreprocessing = new SmartTextPreprocessing(proc)
  val vocabulary = {
    val v = mutable.Map.empty[String, Int]
    using(Source.fromFile("vocabulary.txt")) { it =>
      it.getLines().foreach { it =>
        val split = it.split("\t")
        v.update(split(0), split(1).toInt)
      }
    }
    v.toMap
  }

  def extractChunks(ad: Document): Array[String] = {
    val chunkProcessed = mutable.ArrayBuffer.empty[mutable.ListBuffer[String]]
    val chunksWords = ad.sentences.flatMap { it => it.chunks }.flatten.zip(ad.sentences.flatMap(_.words))
    chunksWords.foreach { it =>
      if(it._1.startsWith("B-")) {
        chunkProcessed.append(mutable.ListBuffer())
      }
      if(chunkProcessed.nonEmpty) {
        chunkProcessed.last.append(it._2)
      }
    }
    chunkProcessed.filter(_.size > 1).map(_.filter(it => it != "," && it != ".").mkString(" ").toLowerCase()).toArray
  }

  def extractEntities(ad: Document): Array[String] = {
    val entitiesProcessed = mutable.ArrayBuffer.empty[mutable.ListBuffer[String]]
    val entitiesWords = ad.sentences.flatMap { it => it.entities }.flatten.zip(ad.sentences.flatMap(_.words))

    entitiesWords.filter(_._1!="O").foreach { it =>
      if(it._1.startsWith("B-")) {
        entitiesProcessed.append(mutable.ListBuffer())
      }
      if(entitiesProcessed.nonEmpty) {
        entitiesProcessed.last.append(it._2)
      }
    }

    entitiesProcessed.map(_.filter(it => it != "," && it != ".").mkString(" ").toLowerCase()).toArray

  }

  def extractYears(ad: Document): Array[String] = {
    ad.sentences
      .flatMap { it => it.tags }
      .flatten
      .zip(ad.sentences.flatMap(_.words))
      .filter { it => it._1 == "CD" && it._2.length > 2 }
      .map(_._2)

  }

  def buildQuery(q: Question, d: Document): Query = {
    // Remove things between parenthesis
    // e.g. STATE OF THE ART MUSEUM (Alex: We'll give you the museum. You give us the state.)
    val categoryProcessed = {
      if(q.category.contains("(") && q.category.contains(")")) {
        val betweenParenthesis = q.category.indexOf(")") - q.category.lastIndexOf("(") + 1
        q.category.dropRight(betweenParenthesis).stripPrefix(" ").stripSuffix(" ")
      } else {
        q.category
      }
    }

    // Check if there are years. If there are, add a boolean query that must match
    val years = extractYears(d)
    val yearsQuery = new BooleanQuery.Builder().let { bqb =>
      years.foreach { it => bqb.add(new TermQuery(new Term("text", it)), Occur.MUST) }
      bqb.build()
    }

    // Build phrase queries using the chunks and the entities from the question
    val chunks   = extractChunks(d)
    val entities = extractEntities(d)

    val entitiesQuery = entities.map { it =>
      val pqb = new PhraseQuery.Builder()
      it.split(" ").foreach { it => pqb.add(new Term("text", it)) }
      pqb.build()
    }.let { it =>
      val bqb = new BooleanQuery.Builder()
      it.foreach { it => bqb.add(it, Occur.SHOULD) }
      bqb.build()
    }.let { it => new BoostQuery(it, 2f) }

    // Shouldn't appear in the title, as this means that the answer was already given
    val entitiesQueryTitle = entities.map { it =>
      val pqb = new PhraseQuery.Builder()
      it.split(" ").foreach { it => pqb.add(new Term("title", it)) }
      pqb.build()
    }.let { it =>
      val bqb = new BooleanQuery.Builder()
      it.foreach { it => bqb.add(it, Occur.SHOULD) }
      bqb.build()
    }

    val chunksQuery = chunks.map { it =>
      val pqb = new PhraseQuery.Builder()
      it.split(" ").foreach { it => pqb.add(new Term("text", it)) }
      pqb.build()
    }.let { it =>
      val bqb = new BooleanQuery.Builder()
      it.foreach { it => bqb.add(it, Occur.SHOULD) }
      bqb.build()
    }.let { it => new BoostQuery(it, 2f) }

    val chunksQueryBoolean = chunks.map { it =>
      val pqb = new BooleanQuery.Builder()
      it.split(" ").foreach { it => pqb.add(new TermQuery(new Term("text", it)), Occur.SHOULD) }
      pqb.build()
    }.let { it =>
      val bqb = new BooleanQuery.Builder()
      it.foreach { it => bqb.add(it, Occur.SHOULD) }
      bqb.build()
    }

    val categoryQuery = tp(categoryProcessed, ProcessingFields.Category).let { it =>
      val bqb = new BooleanQuery.Builder()
      it.foreach { it => bqb.add(new TermQuery(new Term("categories", it)), Occur.SHOULD) }
      bqb.build()
    }.let { it => new BoostQuery(it, 4f) }

    val categoryTextQuery = tp(categoryProcessed, ProcessingFields.Category).let { it =>
      val bqb = new BooleanQuery.Builder()
      it.foreach { it => bqb.add(new TermQuery(new Term("text", it)), Occur.SHOULD) }
      bqb.build()
    }

    // The base query, using the words from the question
    val query = new BooleanQuery.Builder().let { it =>
      tp(q.question, ProcessingFields.Body).foreach { s =>
        it.add(new TermQuery(new Term("text", s)), Occur.SHOULD)
      }
      it.build()
    }.let { it => new BoostQuery(it, 2f) }

    val queryFiltered = new BooleanQuery.Builder().let { it =>
      tp(q.question, ProcessingFields.Body).foreach { s =>
        if (vocabulary.contains(s)) {
          it.add(new TermQuery(new Term("text", s)), Occur.SHOULD)
          s
        }
      }
      it.build()
    }.let { it => new BoostQuery(it, 0.5f) }

    // If it contains quotes in the question, search them as a phrase query
    if(q.question.contains("\"")) {

      val pattern = """.*"(.*)".*""".r
      val pattern(betweenQuotes) = q.question


      val phraseQueryBQ = tp(betweenQuotes, ProcessingFields.Body).let { it =>
        val pqb = new PhraseQuery.Builder()
        it.foreach { it => pqb.add(new Term("text", it)) }
        pqb.build()
      }

      // Phrase and category query
      // Used for filtering
      val pcq = new BooleanQuery.Builder()
        .let { it => if(years.nonEmpty) { it.add(yearsQuery, Occur.SHOULD) }; it }
        .add(phraseQueryBQ, Occur.SHOULD)
        .add(categoryQuery, Occur.SHOULD)
        .build()

      new BooleanQuery.Builder()
        .add(pcq, Occur.FILTER)
        .add(phraseQueryBQ, Occur.SHOULD)
        .add(categoryQuery, Occur.SHOULD)
        .add(entitiesQuery, Occur.SHOULD)
        .add(chunksQuery, Occur.SHOULD)
        .add(chunksQueryBoolean, Occur.SHOULD)
        .add(query, Occur.SHOULD)
        .add(queryFiltered, Occur.SHOULD)
        .build()

    } else {
      new BooleanQuery.Builder()
        .let { it => if(years.nonEmpty) { it.add(yearsQuery, Occur.FILTER) }; it }
        .add(categoryQuery, Occur.SHOULD)
        .add(entitiesQuery, Occur.SHOULD)
        .add(chunksQuery, Occur.SHOULD)
        .add(chunksQueryBoolean, Occur.SHOULD)
        .add(query, Occur.SHOULD)
        .add(queryFiltered, Occur.SHOULD)
        .build()
    }
  }

  def readQuestions(path: String): Seq[Question] = {
    using(Source.fromFile(path)) { it =>
      it.getLines().sliding(4, 4).map { sliding =>
        Question(sliding.head, sliding(1), sliding(2))
      }.toList
    }
  }

  val indexPath: String = c.getString("csc583.data.indexSmart")
  val analyzer = new StandardAnalyzer
  val index = FSDirectory.open(new File(indexPath).toPath)
  val config = new IndexWriterConfig(analyzer)
  val reader = DirectoryReader.open(index)
  val is = new IndexSearcher(reader)

  def query(question: Question, d: Document): Int = {
    val q = buildQuery(question, d)
    val result = is.search(q, Int.MaxValue).scoreDocs
    val answer = (if(question.answer.contains("""|""")) question.answer.split("""|""").toSeq else Seq(question.answer)).map(_.toLowerCase())
    val position = result.map { it => (is.doc(it.doc).get("name"), it.score, it.doc) }
      .map { it => answer.contains(it._1.toLowerCase) }.indexOf(true)
    if(position == -1) {
      Int.MaxValue
    } else {
      position + 1
    }
  }


  println(f"${Calendar.getInstance().getTime}")

  readQuestions("questions.txt")
    .map { it => val d = proc.mkDocument(it.question); proc.annotate(d); (it, d) }
    .par
    .map { it => query(it._1, it._2) }
    .toList
    .let { it =>
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

