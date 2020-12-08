package edu.arizona.cs.apps

import java.io.File
import java.util.Calendar

import com.typesafe.config.ConfigFactory
import edu.arizona.cs.indexing.{FileRepresentation, SmartTextPreprocessing, TextPreprocessing}
import edu.arizona.cs.model.SimpleWikipediaDocument
import edu.arizona.cs.using
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.document.{Document, Field, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.FSDirectory
import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor

import scala.io.Source

object SimpleIndexWikiApp extends App {
  Utils.initializeDyNet()
  private lazy val proc = new CluProcessor()
  val c = ConfigFactory.load()
  val indexPath: String = "data_simplewd/"
  val tp: TextPreprocessing = new SmartTextPreprocessing(proc)

  def buildIndex(): Unit = {
    lazy val analyzer = new WhitespaceAnalyzer
    lazy val index = FSDirectory.open(new File(indexPath).toPath)
    val config = new IndexWriterConfig(analyzer)
    val iw = new IndexWriter(index, config)
    println(f"${Calendar.getInstance().getTime}")
    new File("/mnt/8A70445970444E5F/University/PhD, University of Arizona/2020 Fall/CSC 583/Assignments/Project/wiki-subset-20140602/")
      .listFiles()
      .filter { it => !it.isHidden }
      //      .filter(_.getPath=="/mnt/8A70445970444E5F/University/PhD, University of Arizona/2020 Fall/CSC 583/Assignments/Project/wiki-subset-20140602/enwiki-20140602-pages-articles.xml-0067.txt")
      .foreach { it =>
        println(it.getPath)
        using(Source.fromFile(it)) { file =>
          val fr = FileRepresentation(file.getLines().toList, tp)
          fr.foreach { it =>
            val doc = it.toSimpleDocument()
            addDoc(iw, doc)
          }
        }
      }
    println(f"${Calendar.getInstance().getTime}")
    iw.close()
    index.close()
    analyzer.close()
  }

  /**
   *
   * @param w
   * @param swdoc
   */
  def addDoc(w: IndexWriter, swdoc: SimpleWikipediaDocument): Unit = {
    val doc = new Document()
    doc.add(new TextField("name", swdoc.title, Field.Store.YES))
    doc.add(new TextField("text", swdoc.text.mkString(" "), Field.Store.YES))
    w.addDocument(doc)
  }

  buildIndex()

}
