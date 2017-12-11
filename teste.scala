import java.time.Instant
import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations.{LemmaAnnotation, SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.umd.cloud9.collection.XMLInputFormat
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.io._
import org.apache.spark.mllib.linalg.{Matrix, SingularValueDecomposition}
import org.apache.spark.sql.Dataset

import scala.collection.Map
import scala.collection.mutable.ArrayBuffer

Instant.now.getEpochSecond
val path = "/aas/ch06-lsa/src/main/resources/small-dump.xml"
val stopwordsPath = "/aas/ch06-lsa/src/main/resources/stopwords-it.txt"

@transient val conf = new Configuration()
conf.set(XMLInputFormat.START_TAG_KEY, "<page>")
conf.set(XMLInputFormat.END_TAG_KEY, "</page>")
val kvs = spark.sparkContext.newAPIHadoopFile(path, classOf[XMLInputFormat], classOf[LongWritable], classOf[Text], conf)
val rawXmls = kvs.map(_._2.toString).toDS()

rawXmls.describe() //> org.apache.spark.sql.DataFrame = [summary: string, value: string]

import edu.umd.cloud9.collection.wikipedia.language._
import edu.umd.cloud9.collection.wikipedia._

def wikiXmlToPlainText(pageXml: String): Option[(String, String)] = {
  val hackedPageXml = pageXml.replaceFirst(
    "<text xml:space=\"preserve\" bytes=\"\\d+\">",
    "<text xml:space=\"preserve\">")
  val page = new EnglishWikipediaPage()
  WikipediaPage.readPage(page, hackedPageXml)
  if (page.isEmpty) None // Better: if (page.isEmpty || !page.isArticle || page.isRedirect || page.getTitle.contains("(disambiguation)"))
  else Some((page.getTitle, page.getContent))
}
val docTexts = rawXmls.filter(_ != null).flatMap(wikiXmlToPlainText)


import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.ling.CoreAnnotations._
import java.util.Properties

def createNLPPipeline(): StanfordCoreNLP = {
  val props = new Properties()
  props.put("annotators", "tokenize, ssplit, pos, lemma")
  new StanfordCoreNLP(props)
}

def isOnlyLetters(str: String): Boolean = {
  str.forall(c => Character.isLetter(c))
}

def plainTextToLemmas(text: String, stopWords: Set[String], pipeline: StanfordCoreNLP)
: Seq[String] = {
  val doc = new Annotation(text)
  pipeline.annotate(doc)
  val lemmas = new ArrayBuffer[String]()
  val sentences = doc.get(classOf[SentencesAnnotation])
  for (sentence <- sentences.asScala;
       token <- sentence.get(classOf[TokensAnnotation]).asScala) {
    val lemma = token.get(classOf[LemmaAnnotation])
    if (lemma.length > 2 && !stopWords.contains(lemma) && isOnlyLetters(lemma)) {
      lemmas += lemma.toLowerCase
    }
  }
  lemmas
}

val stopWords = scala.io.Source.fromFile(stopwordsPath).getLines.toSet // toSet !
val bStopWords = spark.sparkContext.broadcast(stopWords)

val terms: Dataset[(String, Seq[String])] = docTexts.mapPartitions { iter =>
  val pipeline = createNLPPipeline()
  iter.map { case (title, contents) => (title, plainTextToLemmas(contents, bStopWords.value, pipeline)) }
}

val termsDF = terms.toDF("title", "terms")
val filtered = termsDF.where(size($"terms") > 1)

import org.apache.spark.ml.feature.CountVectorizer

val numTerms = 20000 // number of terms to keep (the most frequent ones)
val countVectorizer = new CountVectorizer().setInputCol("terms").setOutputCol("termFreqs").setVocabSize(numTerms)
val vocabModel = countVectorizer.fit(filtered) // extract the vocabulary from our DF and construct the model
val docTermFreqs = vocabModel.transform(filtered) // transform our DF / apply the model

docTermFreqs.cache()

import org.apache.spark.ml.feature.IDF
val idf = new IDF().setInputCol("termFreqs").setOutputCol("tfidfVec")
val idfModel = idf.fit(docTermFreqs)
val docTermMatrix = idfModel.transform(docTermFreqs) //.select("title", "tfidfVec")

vocabModel.vocabulary.take(8).zip {
  docTermMatrix.first.getAs[org.apache.spark.ml.linalg.SparseVector](2).toArray.zip {
    docTermMatrix.first.getAs[org.apache.spark.ml.linalg.SparseVector](3).toArray
  }
}

Instant.now.getEpochSecond


