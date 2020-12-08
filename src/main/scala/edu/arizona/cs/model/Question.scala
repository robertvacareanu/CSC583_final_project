package edu.arizona.cs.model

/**
 * Example of a question
 *      NEWSPAPERS
 *      The dominant paper in our nation's capital, it's among the top 10 U.S. papers in circulation
 *      The Washington Post
 *
 * The category: NEWSPAPER
 * The question: The dominant paper in our nation's capital, it's among the top 10 U.S. papers in circulation
 * The answer: The Washington Post
 *
 */
case class Question(category: String, question: String, answer: String)
