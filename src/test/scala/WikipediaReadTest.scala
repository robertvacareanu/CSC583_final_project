import com.typesafe.config.ConfigFactory
import edu.arizona.cs.indexing.{FileRepresentation, SimpleTextPreprocessing, TextPreprocessing}
import edu.arizona.cs.model.Section
import edu.arizona.cs.using
import org.clulab.processors.clu.CluProcessor
import org.scalatest.FlatSpec

import scala.io.Source

class WikipediaReadTest extends FlatSpec {
  val lines = using(Source.fromResource("wiki-example.txt")) { _.getLines().toList }
  val sectionTitles = lines.filter(it => it.startsWith("==") && it.endsWith("=="))
  val c = ConfigFactory.load()
  private lazy val proc = new CluProcessor()
  val indexPath: String = "data/"
  val tp: TextPreprocessing = new SimpleTextPreprocessing(proc)
  val fileRepresentation = FileRepresentation(lines, tp)

  "The reading" should "have the same number of titles" in {
    def flatten(s: Section): Int = {
      if(s.subsections.isEmpty) {
        1
      } else {
        1 + s.subsections.map(flatten).sum
      }
    }

    val titlesCount = fileRepresentation.map { it => it.toDocument().content.map(flatten).sum }.sum
    val goldTitlesCount = sectionTitles.size
    assert(titlesCount == goldTitlesCount)

  }

  "The titles" should "be in the same order" in {
    def flatten(s: Section): List[String] = {
      val ss = if(s.subsections.isEmpty) {
        Nil
      } else {
        s.subsections.flatMap(flatten).toList
      }
      s.title.strings.mkString(" ")::ss
    }
    val goldTitles = sectionTitles.map { it => it.dropWhile(_=='=').reverse.dropWhile(_=='=').reverse }
    val titles = fileRepresentation.flatMap { it => it.toDocument().content.flatMap(flatten) }

    assert(titles == goldTitles)
  }
}
