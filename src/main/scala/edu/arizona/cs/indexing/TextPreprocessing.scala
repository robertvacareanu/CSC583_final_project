package edu.arizona.cs.indexing

import org.clulab.processors.clu.CluProcessor

trait TextPreprocessing {
  def preprocess(text: String, field: ProcessingFields.TextProcessingField): Seq[String]

  def apply(text: String, field: ProcessingFields.TextProcessingField): Seq[String] = preprocess(text, field)
}

class SimpleTextPreprocessing(val proc: CluProcessor) extends TextPreprocessing {
  override def preprocess(text: String, field: ProcessingFields.TextProcessingField): Seq[String] = field match {
    case ProcessingFields.Title => Seq(text.toLowerCase())
    case ProcessingFields.Header => proc.tokenizer.tokenize(text.toLowerCase()).flatMap(_.words).map(it => proc.lemmatizer.lemmatizeWord(it))
    case ProcessingFields.Category => text.toLowerCase().split(",").toSeq
    case ProcessingFields.Body => proc.tokenizer.tokenize(text.toLowerCase()).flatMap(_.words).map(it => proc.lemmatizer.lemmatizeWord(it))
  }
}

class SmartTextPreprocessing(val proc: CluProcessor) extends TextPreprocessing {
  override def preprocess(text: String, field: ProcessingFields.TextProcessingField): Seq[String] = {
    lazy val lower = text.toLowerCase()
    lazy val procText = proc.tokenizer.tokenize(lower).flatMap(_.words) //.filterNot { it => nltkStopWords.contains(it) }
    field match {
      case ProcessingFields.Title => Seq(lower)
      case ProcessingFields.Header => procText.map(it => proc.lemmatizer.lemmatizeWord(it))
      case ProcessingFields.Category => procText.map(it => proc.lemmatizer.lemmatizeWord(it))
      case ProcessingFields.Body => procText.map(it => proc.lemmatizer.lemmatizeWord(it))
      case ProcessingFields.PosTag => ??? // proc.tagPartsOfSpeech()
    }
  }
}

object ProcessingFields {

  sealed trait TextProcessingField

  case object Title extends TextProcessingField

  case object Header extends TextProcessingField

  case object Category extends TextProcessingField

  case object Body extends TextProcessingField

  case object PosTag extends TextProcessingField

}