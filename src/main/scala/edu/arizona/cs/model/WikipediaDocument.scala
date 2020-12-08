package edu.arizona.cs.model

/**
 * A more complex representation, which separetes between the categories, the body, and the headers

 * @param title the title of the wikipedia document
 * @param body the text between the title and the start of the first header
 * @param categories the categories of the document
 * @param content the sections
 */
case class WikipediaDocument(title: Sentence, body: Option[Seq[Sentence]], categories: Option[Seq[String]], content: Seq[Section])
