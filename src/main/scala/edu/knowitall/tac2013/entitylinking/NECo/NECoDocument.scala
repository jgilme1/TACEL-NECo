package edu.knowitall.tac2013.entitylinking.NECo

import java.io.File
import edu.knowitall.collection.immutable.Interval
import org.apache.commons.io.FileUtils

class NECoDocument(val name: String, val file: File, val annotationMap : Map[String,TextSpan]){
  
  val namedEntityCollection = NamedEntityCollection(
		  						scala.collection.JavaConversions.asScalaIterable(NEcoCorefHelperMethods.annotationHelper.getNamedEntitiesByType(
		  						name,"ORGANIZATION",FileUtils.readFileToString(file))).toList,
		  						scala.collection.JavaConversions.asScalaIterable(NEcoCorefHelperMethods.annotationHelper.getNamedEntitiesByType(
		  						name,"LOCATION",FileUtils.readFileToString(file))).toList,
			  					scala.collection.JavaConversions.asScalaIterable(NEcoCorefHelperMethods.annotationHelper.getNamedEntitiesByType(
		  						name,"PERSON",FileUtils.readFileToString(file))).toList)
		  						
  val nerTypeMap = for(am <- annotationMap) yield {
    val nerType = NEcoCorefHelperMethods.annotationHelper.getMatchingNamedEntities(name,getRawDocument,am._2.offsets.start,am._2.offsets.end)
    if(nerType.isEmpty()){
      (am._1,"None")
    }
    else{
     (am._1,nerType.get(0))
    }
  }
  
  
  val corefOffsetsMap = for (am <- annotationMap) yield {
    val corefIntervals = scala.collection.JavaConversions.asScalaIterable(NEcoCorefHelperMethods.annotationHelper.getCorefIntervals(name,getRawDocument, am._2.offsets.start, am._2.offsets.end)).toList
    (am._1,corefIntervals)
  }
  

  
		  						
  def getRawDocument() = {
    FileUtils.readFileToString(file)
  }
}

case class TextSpan(
    val offsets: Interval,
    val spanningString: String,
    val annotation : Option[Annotation]
    )

case class Annotation(
  val dbKey : String,
  val dbEntryName: String
)

case class NamedEntityCollection(
      val organizations: List[String],
      val locations: List[String],
      val people: List[String])


object NECoDocument{
  
  
  def createNECoDocument(textFile: File, annotationFile: File): NECoDocument ={
    val annotationFileString = FileUtils.readFileToString(annotationFile)
    val relevantLines = annotationFileString.split("\n").filter(p => p.startsWith("N") || p.startsWith("T"))
    val tLines = relevantLines.filter(p => p.startsWith("T"))
    val nLines = relevantLines.filter(p => p.startsWith("N"))
    
    var annotationMap = Map[String,TextSpan]()
    for(tLine <- tLines){
      val values = tLine.split("\t")
      val tIdString = values(0)
      val column2values = values(1).split(" ")
      val tOffsets = Interval.closed(column2values(1).toInt,column2values(2).toInt)
      val spanningString = values(2)
      var annotation :Option[Annotation] = None
      for(nLine <- nLines){
        val values = nLine.split("\t")
        val column2Values = values(1).split(" ")
        val nTId = column2Values(1)
        if(tIdString == nTId){
          val dbKey = column2Values(2)
          val dbEntryName = values(2)
          annotation = Some(new Annotation(dbKey,dbEntryName))
        }
      }
      val textSpan = new TextSpan(tOffsets,spanningString,annotation)
      annotationMap = annotationMap + ((tIdString,textSpan))
    }
    val newDoc = new NECoDocument(textFile.getName().dropRight(4),textFile,annotationMap)
    newDoc
  }

}