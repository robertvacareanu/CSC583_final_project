package edu.arizona.cs.model

/**
 *
 * @param title the header
 * @param body the content
 * @param subsections the headers which are contained by this header
 */
case class Section(title: Sentence, body: Seq[Sentence], subsections: Seq[Section])
object Section {
  def apply(title: Sentence, body: Seq[Sentence]): Section = Section(title, body, Seq.empty)
}
