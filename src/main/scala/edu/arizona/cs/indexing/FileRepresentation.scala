package edu.arizona.cs.indexing

import scala.collection.mutable

case class FileRepresentation(text: Seq[String], tp: TextPreprocessing) extends Iterable[DocumentRepresentation] { parent =>

  override def iterator: Iterator[DocumentRepresentation] = new Iterator[DocumentRepresentation] {
    override def hasNext: Boolean = parent.hasNext

    override def next(): DocumentRepresentation = parent.nextWikipediaDocument(tp)
  }

  lazy val lines: BufferedIterator[String] = text.filter { it =>
    if (it.length == 0 || (it.count(_ == ' ') == it.length)) { // Skip empty lines or lines that have only spaces
                                                               // We parse the document using "[[" and "==*" as clues
      false
    } else {
      true
    }
  }.iterator.buffered

  private val buffer = mutable.ListBuffer.empty[String]

  /**
   * Assumes that the first line is always the start of a new document (if there is a next)
   * @return
   */
  def nextWikipediaDocument(tp: TextPreprocessing): DocumentRepresentation = {
    buffer.clear()
    // This tells if there is a next document to be read
    while(lines.hasNext) {
      // Add the next line, which is actually a [[<text>]]
      buffer.append(lines.next())

      // When the next line starts with "[[" and ends with "]]" this signals a new document to be read
      while(lines.hasNext && !(lines.head.startsWith("[[") && lines.head.endsWith("]]"))) {
        buffer.append(lines.next())
      }

      return DocumentRepresentation(buffer.toList, tp)
    }

    throw new IllegalArgumentException("The format is not respected. A document should end with two empty lines")
  }

  def hasNext: Boolean = lines.hasNext

}
