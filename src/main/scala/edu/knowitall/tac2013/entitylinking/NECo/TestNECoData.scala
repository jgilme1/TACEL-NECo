package edu.knowitall.tac2013.entitylinking.NECo

import org.apache.commons.io.FileUtils
import java.io.File
import RunLinkerOnNecoDocument.runLinkerOnNecoDocument
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.browser.entity.batch_match
import edu.knowitall.browser.entity.CrosswikisCandidateFinder
import edu.knowitall.browser.entity.EntityTyper
import edu.knowitall.tac2013.entitylinking.classifier.LinkClassifier
import edu.knowitall.tac2013.entitylinking.classifier.LinkTrainingData

object TestNECoData {
  
  
  def main(args: Array[String]){
    
    val baseDir = args(0)
    val fileListUrl = getClass().getResource("fileList.txt")
    val pathName = fileListUrl.getPath().split("/").dropRight(1).mkString("/")+"/"
    val listOfFiles = FileUtils.readFileToString(new File(fileListUrl.toURI())).split("\n")
    
    
    val annotationTextPairs =listOfFiles.groupBy(f => {f.reverse.dropWhile(p => p!= '.')}).map(f => {(f._2(0),f._2(1))})

    val necoDocuments  = for(ap <- annotationTextPairs) yield NECoDocument.createNECoDocument(new File(pathName+ap._2), new File(pathName+ap._1))
    

    val linkClassifier = new LinkClassifier(new LinkTrainingData(baseDir,"2011") ++ new LinkTrainingData(baseDir,"2012"))

    val linkerSupportPath = new java.io.File(baseDir)
    val linker = new EntityLinker(
    		new batch_match(linkerSupportPath),
    		new CrosswikisCandidateFinder(linkerSupportPath, 0.00, 1),
    		new EntityTyper(linkerSupportPath)
  		)
    
    
    necoDocuments.foreach(f => runLinkerOnNecoDocument(f,baseDir,linker,linkClassifier))
  }
  
  

}