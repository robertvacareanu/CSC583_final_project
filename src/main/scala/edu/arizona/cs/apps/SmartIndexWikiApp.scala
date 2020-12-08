package edu.arizona.cs.apps

import java.io.File
import java.util.Calendar

import com.typesafe.config.ConfigFactory
import edu.arizona.cs.indexing.{FileRepresentation, SmartTextPreprocessing, TextPreprocessing}
import edu.arizona.cs.model.{Section, WikipediaDocument}
import edu.arizona.cs.using
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.document.{Document, Field, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.FSDirectory
import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor

import scala.io.Source

object SmartIndexWikiApp extends App {
  Utils.initializeDyNet()

  private lazy val proc = new CluProcessor()
  val c = ConfigFactory.load()
  val indexPath: String = c.getString("csc583.data.indexSimple")
  val nltkStopWords = Set("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now")
  val tp: TextPreprocessing = new SmartTextPreprocessing(proc)

  def flattenHeader(s: Section): List[String] = {
    s.title.strings.mkString(" ") :: s.subsections.flatMap(flattenHeader).toList
  }

  def flattenBody(s: Section): List[String] = {
    s.body.flatMap(_.strings).mkString(" ") :: s.subsections.flatMap(flattenBody).toList
  }

  def flattenAllTextSections(s: Section): List[String] = {
    s.title.strings.mkString(" ") :: s.body.flatMap(_.strings).mkString(" ") :: s.subsections.flatMap(flattenAllTextSections).toList
  }

  def flattenAllText(wikipediaDocument: WikipediaDocument): List[String] = {
    wikipediaDocument.title.strings.mkString(" ") ::
      wikipediaDocument.body.map { it => it.map { it => it.strings.mkString(" ") }.mkString(" ") }.getOrElse(" ") ::
      wikipediaDocument.content.flatMap(flattenAllTextSections).toList
  }


  /**
   * Add only name of the document and the body
   * Assumes that if there is any relevant information, it is in the body of the article
   *
   * @param w
   * @param wdoc
   */
  def addDoc(w: IndexWriter, wdoc: WikipediaDocument): Unit = {
    val doc = new Document()
    doc.add(new TextField("name", wdoc.title.strings.mkString(" "), Field.Store.YES))
    doc.add(new TextField("categories", wdoc.categories.map { it => it.mkString(" ") }.getOrElse(""), Field.Store.YES))
    doc.add(new TextField("content", wdoc.body.map { it => it.map { it => it.strings.mkString(" ") }.mkString(" ") }.getOrElse(""), Field.Store.YES))
    doc.add(new TextField("header", wdoc.content.flatMap(flattenHeader).mkString(" "), Field.Store.YES))
    doc.add(new TextField("body", wdoc.content.flatMap(flattenBody).mkString(" "), Field.Store.YES))
    doc.add(new TextField("text", flattenAllText(wdoc).mkString(" "), Field.Store.YES))
    w.addDocument(doc)
  }

  def buildIndex(): Unit = {

    lazy val analyzer = new WhitespaceAnalyzer
    lazy val index = FSDirectory.open(new File(indexPath).toPath)
    val config = new IndexWriterConfig(analyzer)
    val iw = new IndexWriter(index, config)
    println(f"${Calendar.getInstance().getTime}")
    new File(c.getString("csc583.data.wikipedia"))
      .listFiles()
      .filter { it => !it.isHidden }
      .foreach { it =>
        println(it.getPath)
        using(Source.fromFile(it)) { file =>
          val fr = FileRepresentation(file.getLines().toList, tp)
          fr.foreach { it =>
            val doc = it.toDocument()
            addDoc(iw, doc)
          }
        }
      }
    println(f"${Calendar.getInstance().getTime}")
    iw.close()
    index.close()
    analyzer.close()
  }

  buildIndex()


}
