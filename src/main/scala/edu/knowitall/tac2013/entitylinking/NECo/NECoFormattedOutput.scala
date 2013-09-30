package edu.knowitall.tac2013.entitylinking.NECo

import edu.knowitall.browser.entity.EntityLink


case class NECoFormattedOutput(val queryId: String, val link: EntityLink, val confidence: Double) {
  
  override def toString(): String = {
    Iterator(queryId,link,confidence) mkString "\t"
  }
}

object FormattedOutput{
  
 
  def readFormattedOutput(line: String): NECoFormattedOutput = {
    val vals = line.split("\t")
    val queryId = vals(0)
    val kbLink = vals(1)
    val confidence = vals(2).toDouble
    new NECoFormattedOutput(queryId,null,confidence)
  }
  
  
  
  
}