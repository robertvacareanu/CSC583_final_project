package edu.arizona.cs.model

/**
 * A simple wikipedia representation contains only the title of the document and the whole content
 */
case class SimpleWikipediaDocument(title: String, text: Seq[String])
