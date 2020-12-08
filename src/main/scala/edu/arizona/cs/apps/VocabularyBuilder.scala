package edu.arizona.cs.apps

import java.io.{File, PrintWriter}

import edu.arizona.cs.indexing.{FileRepresentation, SmartTextPreprocessing, TextPreprocessing}
import edu.arizona.cs.using
import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor

import scala.collection.mutable
import scala.io.Source

object VocabularyBuilder extends App {
  Utils.initializeDyNet()

  private lazy val proc = new CluProcessor()
  val map = mutable.Map.empty[String, Int].withDefaultValue(0)
  val tp: TextPreprocessing = new SmartTextPreprocessing(proc)
  new File("wiki-subset-20140602/")
    .listFiles()
    .filter { it => !it.isHidden }
    .foreach { it =>
      println(it.getPath)
      using(Source.fromFile(it)) { file =>
        val fr = FileRepresentation(file.getLines().toList, tp)
        fr.foreach { it =>
          for (w <- it.toSimpleDocument().text.flatMap(_.split(" "))) {
            map(w) += 1
          }
        }
      }
    }
  using(new PrintWriter(new File("vocabulary.txt"))) { pw =>
    val data = map.toList.sortBy(-_._2)
    println(data.size)
    println(data.take(100))
    val rightDrop = data.count(_._2 < 10)
    data.drop(250).dropRight(rightDrop).foreach { it =>
      pw.println(f"${it._1}\t${it._2}")
    }
  }
}
