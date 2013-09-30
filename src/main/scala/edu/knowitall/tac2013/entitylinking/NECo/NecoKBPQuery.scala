package edu.knowitall.tac2013.entitylinking.NECo
import scala.xml.XML
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.utils.WikiMappingHelper
import edu.knowitall.tac2013.entitylinking.utils.FileUtils
import java.io.File
import edu.knowitall.tac2013.entitylinking.utils.StanfordAnnotatorHelperMethods
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tac2013.entitylinking.coref.CorefHelperMethods

class NecoKBPQuery (val id: String, val name: String, val doc: String,
  val begOffset: Int, val endOffset: Int, val baseDir: String, val necoDoc: NECoDocument) {

  var entityString = name
  var sportsSense: Option[Boolean] = None
  var highestLinkClassifierScore = 0.0


  private def getSourceContext(): String = {
    NEcoCorefHelperMethods.getContextFromDocument(necoDoc.getRawDocument, begOffset, name)
  }

  private def getWideContext(): String = {
    NEcoCorefHelperMethods.getWideContextFromDocument(necoDoc.getRawDocument, begOffset, name)
  }
  
  private def getHeadLineContext(): List[String] = {
    NEcoCorefHelperMethods.getHeadLineContextFromDocument(necoDoc.getRawDocument)
  }

  private def getContextOfAllMentions(): List[String] = {
    var contextualSentences = List[String]()
    val corefMentions = necoDoc.corefOffsetsMap.get(id).get
    for (cmi <- corefMentions) {
      val contextSentence = NEcoCorefHelperMethods.getContextFromDocument(necoDoc.getRawDocument, begOffset, name)
      contextualSentences = contextualSentences :+ contextSentence
    }
    var totalContext = List[String]()
    val corefContext = ((contextualSentences.toList ::: List(getSourceContext())).toSet).toList
    totalContext = corefContext
    val headLineContext = getHeadLineContext()
    for(hlc <- headLineContext){
      var addToContext = true
      for(cc <- corefContext){
        if(cc.contains(hlc)){
          addToContext = false
        }
      }
      if(addToContext){
          totalContext = hlc :: totalContext        
      }
    }
    totalContext.toList
  }

  val sourceContext = getSourceContext()
  val sourceWideContext = getWideContext()

  val corefSourceContext = getContextOfAllMentions()
  //val stanfordNERType = CorefHelperMethods.get(year).getStanfordNERType(id, year)

  def trimSourceContext(): String = {
    val stringOffsetOfEntity = sourceContext.indexOf(name)

    stringOffsetOfEntity match {
      case -1 => sourceContext.slice((sourceContext.length() / 2) - 40, ((sourceContext.length() / 2) + 40))
      case _ => sourceContext.slice(stringOffsetOfEntity - 40, stringOffsetOfEntity + 40 + name.length())
    }

  }

  //debug output on construction
  //System.err.println("KBPQuery for entity: " + name +" has context sentence of: " + sourceContext)
}

object NecoKBPQuery {

  private val helperCache = new scala.collection.mutable.HashMap[String, NecoKBPQueryHelper]

  def getHelper(baseDir: String) = helperCache.getOrElseUpdate(baseDir, NecoKBPQueryHelper(baseDir))
}

case class NecoKBPQueryHelper(val baseDir: String) {

 
  val mapFile = baseDir + "/wikimap.txt"
  val wikiMap = using(io.Source.fromFile(mapFile, "UTF8")) { source =>
    WikiMappingHelper.loadNameToNodeIdMap(source.getLines)
  }
  
  val kbContextMapFile = baseDir + "/kbIdToFirstParagraph.txt"
  //val kbContextMap = using(io.Source.fromFile(kbContextMapFile, "UTF8")) { source =>
   // WikiMappingHelper.loadKbIdToContextMap(source.getLines)
 // }

  val kbIdToWikiStructuredTypeFile = getClass.getResource("kbIdToWikiStructuredTypeMap.txt").getPath()
  val kbIdToWikiStructuredTypeMap = using(io.Source.fromFile(kbIdToWikiStructuredTypeFile, "UTF8")) { source =>
    WikiMappingHelper.loadKbIdTowikiStructuredTypeMap(source.getLines)
  }

  val kbIdTextToMapFile = baseDir + "/kbIdToTextMap.txt"
  val kbIdTextMap = using(io.Source.fromFile(kbIdTextToMapFile, "UTF8")) { source =>
    WikiMappingHelper.loadIdToIntroTextMap(source.getLines)
  }
  val kbToTitleMapFile = baseDir + "/wikimap.txt"
  val kbIdToTitleMap = using(io.Source.fromFile(kbToTitleMapFile, "UTF8")) { source =>
    WikiMappingHelper.loadIdToTitleMap(source.getLines)
  }

  val kbTitleToIdMapFile = getClass.getResource("kbIdToTitleMap.txt").getPath()
  val kbTitleToIdMap = using(io.Source.fromFile(kbTitleToIdMapFile, "UTF8")) { source =>
    WikiMappingHelper.loadKbTitleToIdMap(source.getLines)
  }
  val kbIdToWikiTypeFile = getClass.getResource("kbIdToWikiTypeMap.txt").getPath()
  val kbIdToWikiTypeMap = using(io.Source.fromFile(kbIdToWikiTypeFile, "UTF8")) { source =>
    WikiMappingHelper.loadKbIdToWikiTypeMap(source.getLines)
  }
}
