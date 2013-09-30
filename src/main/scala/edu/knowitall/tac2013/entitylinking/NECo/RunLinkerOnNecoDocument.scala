package edu.knowitall.tac2013.entitylinking.NECo

import edu.knowitall.tac2013.entitylinking.classifier.LinkClassifier
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.tac2013.entitylinking.FormattedOutput
import edu.knowitall.tac2013.entitylinking.utils.GeneralHelperMethods
import edu.knowitall.browser.entity.EntityLink
import edu.knowitall.tac2013.entitylinking.utils.ConfidenceHelper
import java.io.PrintWriter
import java.io.File
import edu.knowitall.tac2013.entitylinking.KBPQuery

object RunLinkerOnNecoDocument {
  
  val linkThreshold = .93
  
  def runLinkerOnNecoDocument(nDoc : NECoDocument, baseDir: String, linker: EntityLinker, linkClassifier: LinkClassifier){
    
    //create set of queries from NECoDocument
    val tIds = nDoc.annotationMap.keys
    val queries = for(tId <- tIds) yield {
      val map = nDoc.annotationMap.get(tId).get
      val name = map.spanningString
      val offsets = map.offsets
      new NecoKBPQuery(tId,name,nDoc.name,offsets.start,offsets.end,baseDir,nDoc)
    }
    var answers = List[NECoFormattedOutput]()

    for(q <- queries){
        val query = new KBPQuery(q.id,q.name,q.doc,q.begOffset,q.endOffset,q.baseDir,"2013")
	    val entityString = NEcoCorefHelperMethods.identifyBestEntityStringByRules(q)
	    q.entityString = entityString
	    query.entityString = entityString
	    val linkOpt = linker.getBestEntity(entityString, q.corefSourceContext)
	    q.highestLinkClassifierScore = linkOpt match{
	      case None => 0.0
	      //case Some(x) => linkClassifier.score(q, x)
	      case Some(x) => linkClassifier.score(query, x)
	    }
        query.highestLinkClassifierScore = q.highestLinkClassifierScore
	    val answer = linkOpt.filter(l => linkClassifier.score(query,l) > linkThreshold) match {
	      case None => {
	        //if link is null and there is a better entity string
	        //than the one given in KBP check KB for
	        var answer: Option[NECoFormattedOutput] = None

	        /*****-Not applicable to NecoCode **********/
//	        if (q.entityString != q.name) {
//	          val kbIdOption = KBPQuery.getHelper(baseDir, year).kbTitleToIdMap.get(q.entityString)
//	          if (kbIdOption.isDefined) {
//	            answer = Some(new FormattedOutput(q.id, kbIdOption.get, .8))
//	          }
//	        }
	        
	        // if no answer has been found and the entity String is longer than the 
	        // original name by two words, try trimming the entity string and running the linker
	        // again
	        if(answer.isEmpty && ((q.entityString.split(" ").length > (q.name.split(" ").length +1)) && (!q.entityString.contains(",")) && (!q.entityString.contains(".")))){
	          val backOffStrings = GeneralHelperMethods.findBackOffStrings(q.name,q.entityString)
	          var maxScore = 0.0
	          var maxLink :Option[EntityLink] = None
	          var maxString = q.entityString
	          for(backOffString <- backOffStrings){
	            val link = linker.getBestEntity(backOffString, q.corefSourceContext)
	            if(link.isDefined){
	              val score = linkClassifier.score(query,link.get)
	              println("original name = " + q.name + " backOffstring = " + backOffString + " score = " + score)
	              if(score > maxScore){
	            	 maxScore = score
	            	 maxLink = Some(link.get)
	            	 maxString = backOffString
	              }
	            }
	          }
	          if(maxLink.isDefined){
	            q.entityString = maxString
	            query.entityString = maxString
	            q.highestLinkClassifierScore = maxScore
	            query.highestLinkClassifierScore = maxScore
	            if(maxScore > 0.0 || q.entityString == maxLink.get.entity.name){
	        	  val nodeId = Some(maxLink.get.entity.fbid)
	              answer = Some(new NECoFormattedOutput(q.id,maxLink.get, 0.6))
	            }
	          }
	        }
	        if (answer.isDefined) {
	          answer.get
	        } else {
	          new NECoFormattedOutput(q.id, null, 0.55)
	        }
	      }
	      case Some(link) => {
	
	        var nodeId :Option[String] = None
	        nodeId = Some(link.entity.fbid)
	        
	        //if the KBNode doesn't exist, then make sure to change the alternative entity string	
	        //to the name of the Freebase link for better nil clustering.
	        if(nodeId.isEmpty){
	          q.entityString = link.entity.name.replaceAll("\\([^\\(]+\\)", "").trim()
	          new NECoFormattedOutput(q.id, link, .75)
	        }
	
	        new NECoFormattedOutput(q.id, link, .5)
	      }
	    }
	    answers = answer :: answers
	    
	    
	    println(q.id + "\t" + q.name + "\t" + q.entityString + "\t" + q.highestLinkClassifierScore)
    }
    
    
    val pw = new PrintWriter(new File(nDoc.name + "out"))
    
    var totalSpans =0.0
    var correctPredictions = 0.0
    var totalAnnotated = 0.0
    var correctAnnotationPredictions = 0.0
    val mappingHelper = NecoKBPQuery.getHelper(baseDir)
    for(am <- nDoc.annotationMap){
      pw.write(am._1 + "\t")
      val goldResult = am._2.annotation.isDefined match {
        case true => {
        			  totalAnnotated += 1
                      am._2.annotation.get.dbEntryName
        }
        case false => "NIL"
      }
      
      pw.write(goldResult)
      
      val goldDBKey = am._2.annotation.isDefined match {
        case true => {
        			am._2.annotation.get.dbKey
        }
        case false => "NIL"
      }
      pw.write("\t"+goldDBKey)
      pw.write("\n")
      
      
      //output system result
      var systemAnswer = "NIL"
      var systemDbKey = "NIL"
      var result = "INCORRECT"
      for(answer <- answers){
        if(answer.queryId == am._1){
          if(answer.link != null){
            systemAnswer = answer.link.entity.name
            systemDbKey = answer.link.entity.fbid
          }
        }
      }
      if(systemDbKey == goldDBKey.split(":")(1)){
        correctPredictions += 1
        result = "CORRECT"
        if(am._2.annotation.isDefined){
          correctAnnotationPredictions += 1
        }
      }
      
      pw.write(am._1 + "\t")
      pw.write(systemAnswer+"\t"+systemDbKey+"\n")
      pw.write("--------------------------------\n"+result+"\n---------------------------------------\n")
      
      
      totalSpans += 1
    }
    
    val precision = correctPredictions/totalSpans
    val recall = correctAnnotationPredictions/totalAnnotated
    pw.write("PRECISION:   "+precision+"\n")
    pw.write("RECALL:      "+recall)
    pw.close()
    
  }

}