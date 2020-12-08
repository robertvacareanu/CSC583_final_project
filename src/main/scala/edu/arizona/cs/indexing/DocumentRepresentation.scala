package edu.arizona.cs.indexing

import edu.arizona.cs.model.{Section, Sentence, SimpleWikipediaDocument, WikipediaDocument}

import scala.collection.mutable

case class DocumentRepresentation(lines: Seq[String], tp: TextPreprocessing) {

  def toSimpleDocument(): SimpleWikipediaDocument = {
    val li = lines.iterator.buffered
    val title = if(li.hasNext) {
      val firstLine = li.next()
      assert(firstLine.startsWith("[[") && firstLine.endsWith("]]"))
      tp(firstLine.drop(2).dropRight(2), ProcessingFields.Title).mkString(" ")
    } else {
      throw new IllegalArgumentException("The first line should be the title")
    }

    val restOfLines = mutable.ListBuffer.empty[String]

    while(li.hasNext) {
      if(li.head.startsWith("==")) {
        val length = li.head.takeWhile(_ == '=').length
        val header = li.head.drop(length).reverse.drop(length).reverse
        restOfLines.append(tp(header, ProcessingFields.Header).mkString(" "))
        li.next()
      } else if (li.head.isEmpty || li.head.count(_ == ' ') == li.head.length) {
        li.next()
      } else {
        restOfLines.append(tp(li.next(), ProcessingFields.Body).mkString(" "))
      }
    }

    SimpleWikipediaDocument(title, restOfLines.toSeq)
  }

  def toDocument(): WikipediaDocument = {
    // the title is always on the first line
    val li = lines.iterator.buffered
    val title = if(li.hasNext) {
      val firstLine = li.next()
      assert(firstLine.startsWith("[[") && firstLine.endsWith("]]"))
      Sentence(tp(firstLine.drop(2).dropRight(2), ProcessingFields.Title))
    } else {
      throw new IllegalArgumentException("The first line should be the title")
    }

    if (li.isEmpty) {
      return WikipediaDocument(title, None, None, Seq.empty)
    }

    val categories = if (li.head.startsWith("CATEGORIES: ")) {
      Some(tp(li.next().drop("CATEGORIES: ".length), ProcessingFields.Category))
    } else {
      None
    }

    if(li.hasNext) {
      if (li.head.startsWith("==") && li.head.endsWith("==")) {
        return WikipediaDocument(title, None, categories, buildSections(li))
      } else {
        val body = readUntilSectionStart(li)
        return WikipediaDocument(title, Some(body), categories, buildSections(li))
      }
    } else {
      return WikipediaDocument(title, None, categories, Seq.empty)
    }
  }

  /**
   *
   * @param li buffered iterator containing the lines of the document (some of them consumed)
   * @return
   */
  private def readUntilSectionStart(li: BufferedIterator[String]): Seq[Sentence] = {
    if(li.isEmpty || (li.head.startsWith("==") && li.head.endsWith("=="))) {
      return Seq.empty
    }
    val body = mutable.ListBuffer.empty[Sentence]
    body.append(Sentence(tp(li.next(), ProcessingFields.Body)))
    while(li.hasNext) {
      val nextLine = li.head
      if(nextLine.startsWith("==") && nextLine.endsWith("==")) {
        return body.toList
      } else {
        body.append(Sentence(tp(li.next(), ProcessingFields.Body)))
      }
    }

    body.toList
  }

  def buildSections(li: BufferedIterator[String]): Seq[Section] = {
    if(li.hasNext) {
      // The first line is the title, always
      val t = li.next()
      val title = Sentence(tp(t.dropWhile(_=='=').reverse.dropWhile(_=='=').reverse, ProcessingFields.Body))
      val level = t.takeWhile(_=='=').length

      // Sanity check
      assert(t.startsWith("==") && t.endsWith("=="))

      val body = readUntilSectionStart(li)

      // Check if there are lines left. If there are, it means that there is a section to be read. If not, this section
      // has only title and body
      if(li.hasNext) {
        // the start of a new section
        if(li.head.startsWith("==") && li.head.endsWith("==")) {
          val sectionLevel = li.head.takeWhile(_ == '=').length // Calculate level
          if(level < sectionLevel) { // if level < sectionLevel then the next section is a child of this section
            // Calculate the sections
            val sections = buildSections(li)

            // Check if it has next, and if it does call recursively
            // Also, if it has the same level as the current one, append it to the result
            if(li.hasNext) {
              val nextSectionLevel = li.head.takeWhile(_ == '=').length
              if (level < nextSectionLevel) { // happens when the next section is 2 levels deeper, and the following one is only 1 (e.g. {2, 4, 3})
                return Seq(Section(title, body, sections++buildSections(li).toList))
              } else if(level == nextSectionLevel) { // If they are on the same level, return concatenation
                return Section(title, body, sections)::buildSections(li).toList
              } else { // This happens only when level > nextSectionLevel. In this case return current only,
                       // as we will take care of the next section as we get back from the recursive calls
                return Seq(Section(title, body, sections))
              }
            } else {
              // We finished the document
              return Seq(Section(title, body, sections))
            }
          } else if (level == sectionLevel) { //
            return Section(title, body) :: buildSections(li).toList
          } else {
            return Seq(Section(title, body))
          }
        } else {
          throw new IllegalArgumentException("Weird format")
        }
      } else { // This section has only title and body
        return Seq(Section(title, body))
      }
    } else {
      return Seq.empty
    }

  }
}
