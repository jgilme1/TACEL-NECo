package edu.knowitall.tac2013.entitylinking.NECo

import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.tac2013.entitylinking.SolrHelper
import edu.knowitall.tac2013.entitylinking.utils.TipsterData.expandStateAbbreviation
import edu.knowitall.tac2013.entitylinking.utils.TipsterData
import java.io.File
import scala.util.matching.Regex
import scala.collection.mutable
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tac2013.entitylinking.utils.WikiMappingHelper
import edu.knowitall.browser.entity.EntityLink
import scala.util.control.Breaks._
import edu.knowitall.tac2013.entitylinking.utils.StanfordAnnotatorHelperMethods
import edu.knowitall.tool.sentence.OpenNlpSentencer

object NEcoCorefHelperMethods {
  
  val annotationHelper = new NECoStanfordAnnotatorHelperMethods(true);
  private val stateAbbreviationPattern = """(\w+),\s([A-Za-z])\.?([A-Za-z])\.?$""".r
  private val stopWords = {
    val url = getClass.getResource("/edu/knowitall/tac2013/entitylinking/classifier/stopwords.txt")
    require(url != null, "Could not find stopwords.txt")
    io.Source.fromURL(url, "UTF8").getLines.flatMap(_.split(",")).map(_.toLowerCase).toSet
  }
  private val xmlTagPattern = new Regex("</?[^<]+>")
  private val sentencer = new OpenNlpSentencer()
  
  def getStanfordNERType(q : NecoKBPQuery) = {
	  	annotationHelper.getMatchingNamedEntities(q.necoDoc.name,q.necoDoc.getRawDocument, q.begOffset, q.endOffset)
  }
  
  
  private def getIndices(searchString: String, targetString: String): List[Interval] = {
    var intervalList = List[Interval]()
    var nextIndex = searchString.indexOf(targetString)
    while(nextIndex != -1){
      val thisInterval = Interval.closed(nextIndex,nextIndex+targetString.length()-1)
      intervalList = thisInterval :: intervalList
      nextIndex = searchString.indexOf(targetString,nextIndex+1)
    }
    intervalList.toList
  }
  
  private def searchCoreferences(necoKbpQuery: NecoKBPQuery, entityType: String, namedEntityCollection: NamedEntityCollection): String = {
    val originalName = necoKbpQuery.name
    if(entityType == "ORGANIZATION" || entityType == "LOCATION"){
      val rawDoc = necoKbpQuery.necoDoc.getRawDocument
      val namedEntities = namedEntityCollection.locations ::: namedEntityCollection.organizations
      val corefOffsets = necoKbpQuery.necoDoc.corefOffsetsMap.get(necoKbpQuery.id).getOrElse(List[Interval]())
      var candidateNamedEntities = List[String]()
      for (namedEntity <- namedEntities){
        val intervals = getIndices(rawDoc.toLowerCase(),namedEntity.toLowerCase())
        for(interval <- intervals){
          for(offsets <- corefOffsets){
            if(offsets.size < 50){
	            if(offsets.contains(interval.start) && offsets.contains(interval.end)){
	              candidateNamedEntities = namedEntity.replace(" in ", ", ") :: candidateNamedEntities
	            }
            }
          }
        }
      }
      val candidate = (candidateNamedEntities.filter(p => {p.length() > originalName.length()}).filter(p => !p.contains(",")).sortBy(f => f.length()).headOption)
      if(candidate.isDefined){
          return candidate.get
      }
    }
    originalName
  }
  
  
  def identifyBestEntityStringByRules(q: NecoKBPQuery): String = {
    //check sports classifier to override location possibilities..
    val sportsSenseOption = q.sportsSense
    val couldBeLocation = sportsSenseOption match{
      case Some(true) => {false}
      case Some(false) => {true}
      case None => {true}
    }
    var alternateName = q.name
    val namedEntityCollection = q.necoDoc.namedEntityCollection
    val entityType = q.necoDoc.nerTypeMap.get(q.id).get
    if(entityType != "None"){
      alternateName =
      entityType match{
        case "ORGANIZATION" => { findBestOrganizationString(q,namedEntityCollection.organizations)}
        case "LOCATION" => {
        	if(couldBeLocation){
        	  findBestLocationString(q,namedEntityCollection.locations)
        	}
        	else{
        	  q.name
        	}
        	}
        case "PERSON" => {findBestPersonString(q,namedEntityCollection.people,true)}
      }
    }
    alternateName match{
      case q.name => {
        alternateName = findBestOrganizationString(q,namedEntityCollection.organizations)
        if(alternateName == q.name){
          if(couldBeLocation){
            alternateName = findBestLocationString(q,namedEntityCollection.locations)
          }
        }
        if(alternateName == q.name){
          alternateName = findBestPersonString(q,namedEntityCollection.people,false)
        }
      }
      case _ => {}
    }
    if(alternateName == q.name){
      val corefString = searchCoreferences(q,entityType,namedEntityCollection)
      if(corefString.toLowerCase().contains(q.name.toLowerCase())){
        alternateName = corefString
      }
    }
    alternateName
  }
  
  private def sortCandidateStringsByProximity(necoKbpQuery: NecoKBPQuery, candidateStrings: List[String]): List[String] =  {
    val rawDoc = necoKbpQuery.necoDoc.getRawDocument
    val entityPosition = necoKbpQuery.begOffset
    val uniqueCandidateMap = candidateStrings.groupBy[String](f=> f)
    val candidateDistanceTuples = for(uniqueCandidate <- uniqueCandidateMap.keys) yield {
      var nextIndex = rawDoc.indexOf(uniqueCandidate)
      var minDistance = rawDoc.length()
      while(nextIndex != -1){
        val proximity = entityPosition - nextIndex
        if( proximity > 0){
          minDistance = math.min(minDistance, proximity)
        }
        nextIndex = rawDoc.indexOf(uniqueCandidate,nextIndex+1)
      }
      (uniqueCandidate,minDistance)
    }
    candidateDistanceTuples.toList.sortBy(f => f._2).map(x => x._1)
  }
  
  private def findBestOrganizationString(necoKbpQuery: NecoKBPQuery, candidateStrings: List[String]) :String = {
    val originalString = necoKbpQuery.name.trim()
    val sortedCandidateStrings = sortCandidateStringsByProximity(necoKbpQuery,candidateStrings)
    val rawDoc = necoKbpQuery.necoDoc.getRawDocument


    try{
    val accronymRegex = new Regex("\\([^\\)\\(]{0,15}"+originalString+"[^\\)\\(]{0,15}\\)")
    //if the organization is an acronym
    if(originalString.forall(p => p.isUpper) || accronymRegex.findFirstIn(rawDoc).isDefined ){
            
      for(cs <- sortedCandidateStrings){
        val words = cs.split(" ").filter(p => {p(0).isUpper}).takeRight(originalString.length())
        var goodCandidate = true
        var index = 0
        if(words.length >= originalString.length()){
	        for(word <- words){
	          if(word(0) != originalString(index)){
	            goodCandidate = false
	          }
	          index += 1
	        }
	        if(goodCandidate){
	          val candidateWords = cs.split(" ")
	          var index = 0
	          for(cw <- candidateWords){
	            if(cw == words.head){
	              return candidateWords.slice(index,candidateWords.length) mkString " "
	            }
	            index +=1
	          }
	        }
        }
      }
      
      // if in parentheses and nothing was found...
      //val parenthesisRegexPattern = new Regex("([A-Z]\\w+ (\\w+ )*[A-Z]\\w+)[\\.\\s]*\\([^\\)\\(]{0,5}"+originalString+"[^\\)\\(]{0,5}\\)")
      val accRegexPattern = new Regex("(["+originalString(0).toUpper+originalString(originalString.length()-1).toUpper+"][\\S]+ ([\\S]+ ){0,2}[A-Z][\\S]+).{0,15}"+originalString)
      val accronymMatch = accRegexPattern.findFirstMatchIn(rawDoc)
      if(accronymMatch.isDefined){
        var expandedString = accronymMatch.get.group(1)
        if(stopWords.contains(expandedString.split(" ")(0).toLowerCase())){
          expandedString = expandedString.split(" ").drop(1).mkString(" ")
        }
        return expandedString
      }
      
    }
    }
    catch{
      case e: Exception => {
        
      }
    }
    
    //non caps organization, check if there is a longer string than the original
    //name with the original name as the rightmost word
	var probablyOrganization = true  
    var originalStringIsLocation = false
    val namedEntityCollection = necoKbpQuery.necoDoc.namedEntityCollection
    val locations = namedEntityCollection.locations
	  
    for(loc <- locations){
      if(loc.contains(originalString)){
        originalStringIsLocation = true
      }
    }
	  
    if(originalStringIsLocation){
      probablyOrganization = false
      if(necoKbpQuery.sportsSense.getOrElse(false)){
        probablyOrganization = true
      }
    }
	
	  
	  
    if(probablyOrganization){
        //do this if original String is not refferring to a location
        for(cs <- candidateStrings){
          val words = cs.split(" ")
          val originalWords = originalString.split(" ")
          if( (words.length > originalWords.length) &&
              ( (words.takeRight(originalWords.length).mkString(" ") == originalString) ||
                (words.take(originalWords.length).mkString(" ") == originalString)  )){
            return words mkString " "
          }
        }
    }
    
    //finally check if the original string if prefix of an organization
    for(cs <- sortedCandidateStrings){
      if(cs.toLowerCase().startsWith(originalString.toLowerCase()) && cs.length() > originalString.length() && cs.split(" ").length ==1){
        return cs
      }
    }
    
    originalString
    
  }
  
  def locationCasing(str: String) :String ={
    var words = List[String]()
    for(s <- str.split(" ")){
      var newS = s
      if(!s.contains(".")){
        newS = for(c <- s) yield {
          c.toLower
        }
        newS = newS(0).toUpper + newS.tail
      }
        words = words :+ newS
      }
    words mkString " "
  }
  
  private def expandLocation(containerLocation: String): List[String] = {
    
    val containerLocationPrefix = if(!containerLocation.last.isLetter){
      containerLocation.dropRight(1)
    }
    else{
      containerLocation
    }
    var possibleExpansions = List[String]()
    
    if(containerLocationPrefix.length() > 2){
      val stateOrProvinces = TipsterData.stateOrProvinces
      for(state <- stateOrProvinces){
        if(state.startsWith(containerLocationPrefix.toLowerCase())){
          possibleExpansions = locationCasing(state) :: possibleExpansions
        }
      }
    }
    possibleExpansions.toList    
  }
  
  private def expandAbbreviation(str:String) :String = {
    val stateAbbreviationMatch = stateAbbreviationPattern.findFirstMatchIn(str)
    if(stateAbbreviationMatch.isDefined){
      val abbreviation = stateAbbreviationMatch.get.group(2).toUpperCase() + 
    		  stateAbbreviationMatch.get.group(3).toUpperCase()
      val city = stateAbbreviationMatch.get.group(1)
      val expandedStateAbbreviation = expandStateAbbreviation(abbreviation,city)
      if(expandedStateAbbreviation.isDefined){
        expandedStateAbbreviation.get
      }
      else{
       str 
      }
    }
    else{
      //check for Mass. pattern and expand if valid
      val containedLocation = str.split(",")(0).trim()
      val containerLocation = str.split(",")(1).trim()
      val expandedLocations = expandLocation(containerLocation)
      for(expandedLocation <- expandedLocations){
        if(locationContainsLocation(expandedLocation,containedLocation)){
          return (containedLocation + ", " + expandedLocation) 
        }
      }
      str
    }
  }
  private def findBestLocationString(necoKbpQuery: NecoKBPQuery, candidateStrings: List[String]) :String = {
    val originalString = necoKbpQuery.name.trim()
    val sortedCandidateStrings = sortCandidateStringsByProximity(necoKbpQuery,candidateStrings)
    var candidates = List[String]()
    val originalWords = originalString.split(" ")
    for(cs <- sortedCandidateStrings){
      val size = cs.split(" ").length
      var index = 0
      while(index < (size-1)){
	      val words = cs.split(" ").drop(index)
	      if( (words.length > (originalWords.length +1)) &&
	          (words.take(originalWords.length).mkString(" ").toLowerCase() == originalString.toLowerCase()) &&
	          (words(originalWords.length) == "," || words(originalWords.length) == "in")){
	        candidates  = candidates :+ words.take(originalWords.length).mkString(" ") + ", " + words.drop(originalWords.length+1).mkString(" ") 
	      }
	      index += 1
      }
    }
    candidates = candidates.filter(p => (p.split(" ").length < 7))
    candidates = candidates.filter(p => (isValidLocation(p)))
    if(candidates.isEmpty){
      //check to see if state is mentioned somewhere, then build a new String with
      //that state or country
      val containerMap = scala.collection.mutable.Map[String,Int]()
      for(cs <- candidateStrings){
        if(locationContainsLocation(cs,originalString)){
          if(cs != originalString && cs != "United States"){
	          if(containerMap.contains(cs)){
	              containerMap += ((cs,containerMap.get(cs).get+1))
	          }
	          else{
	              containerMap += ((cs,1))           
	          }
          }
        }
      }
      if(containerMap.isEmpty){
        //try  regular string searching instead of relying on Stanford NER
        val containedPlace = originalString
        val origQuote = originalString.replaceAll("\\(|\\)", "")
        val locationRegex = new Regex("("+origQuote+"|"+origQuote.toLowerCase()+"|"+origQuote.toUpperCase()+"),\\s?([A-Z][\\S]+)[\\s\\.\\?!,]")
        val sourceText = necoKbpQuery.necoDoc.getRawDocument
        val candidates = scala.collection.mutable.Map[String,Int]()
        for( locationRegex(containedLoc,containerLoc) <- locationRegex.findAllMatchIn(sourceText); fullLocation = expandAbbreviation(locationCasing(containedLoc+", " +containerLoc)).split(",");
             if locationContainsLocation(fullLocation(1).trim(),fullLocation(0).trim())) {
          val containerLocation = fullLocation(1).trim()
          if(candidates.contains(containerLocation)){
            candidates += ((containerLocation, 1 + candidates.get(containerLocation).get))
          }
          else{
            candidates += ((containerLocation,1))
          }
        }
        val headTuple = candidates.toMap.toList.sortBy(f => f._2).headOption
        if(headTuple.isDefined){
          containedPlace + ", "+headTuple.get._1
        }
        else{
          originalString
        }
      }
      else{
        //sort by distance to original string
        val containerStrings = containerMap.keys
        val sortedContainerStrings = sortCandidateStringsByProximity(necoKbpQuery,containerStrings.toList)
        locationCasing(originalString + ", " + sortedContainerStrings.head)
//        var largestCount =0
//        var largestContainer = ""
//        for(containerCandidate <- containerMap){
//          val container = containerCandidate._1
//          val count = containerCandidate._2
//          if(count > largestCount){
//            largestCount = count
//            largestContainer = container
//          }
//        }
//        locationCasing(originalString +", " + largestContainer)
      }
    }
    else{
       val candidate = candidates.head
       expandAbbreviation(locationCasing(candidate))
      }
  }
  private def findBestPersonString(necoKbpQuery: NecoKBPQuery, candidateStrings: List[String], probablyPerson: Boolean) :String = {
      val originalString = necoKbpQuery.name.trim()
      for(cs <- sortCandidateStringsByProximity(necoKbpQuery,candidateStrings)){
        val words = cs.split(" ")
        val originalWords = originalString.split(" ")
        if( (words.length > originalWords.length) &&
        		( (words.takeRight(originalWords.length).mkString(" ") == originalString) ||
        		   (words.take(originalWords.length).mkString(" ") == originalString)) &&
        		   (words.length < 4)){
          return (words mkString " ") 
        }
      }

      if(probablyPerson){
      //try a conservative name regex if nothing from Stanford NER was found
	      val nameRegex = """(\.|(\s[a-z]+\s))([A-Z]\w+\s[A-Z]\w+)(\.|(\s[a-z]+\s))""".r
	      val rawDoc = necoKbpQuery.necoDoc.getRawDocument
	      val nameList = for(nameMatch <- nameRegex.findAllMatchIn(rawDoc); name = nameMatch.group(3); if name.contains(originalString)) yield name
	      val sortedNameList = sortCandidateStringsByProximity(necoKbpQuery,nameList.toList)
	      if(sortedNameList.headOption.isDefined){
	        return sortedNameList.head
	      }
      }
      
      originalString
  }
  
  private def isValidLocation(locationStr: String): Boolean = {
    val placeNames = locationStr.split(",").map(f => f.trim())
    if(placeNames.length == 2){
      return ((locationContainsLocation(placeNames(1),placeNames(0))) || (!sameLocationType(placeNames(1),placeNames(0))))
    }
    else{
      return false
    }
  }
  
  private def sameLocationType(location1: String, location2: String): Boolean = {
    val cities = TipsterData.cities
    val stateOrProvinces = TipsterData.stateOrProvinces
    val countries = TipsterData.countries
    
    if(cities.contains(location1.toLowerCase()) && cities.contains(location2.toLowerCase())){
      return true
    }
    if(stateOrProvinces.contains(location1.toLowerCase()) && stateOrProvinces.contains(location2.toLowerCase())){
      return true
    }
    if(countries.contains(location1.toLowerCase()) && countries.contains(location2.toLowerCase())){
      return true
    }
    return false
  }
  
  private def locationContainsLocation(container: String, contained: String): Boolean = {
    val cities = TipsterData.cities
    val stateOrProvinces = TipsterData.stateOrProvinces
    val countries = TipsterData.countries
    val stateCityMap = TipsterData.provinceCityMap
    val countryCityMap = TipsterData.countryCityMap
    
    if(cities.contains(contained.toLowerCase())){
      if(stateOrProvinces.contains(container.toLowerCase())){
        val citySet = stateCityMap.get(locationCasing(container))
        if(citySet.isDefined){
          if(citySet.get.contains(locationCasing(contained))){
        	  return true
          }
        }
      }
      if(countries.contains(container.toLowerCase())){
        val citySet = countryCityMap.get(locationCasing(container))
        if(citySet.isDefined){
          if(citySet.get.contains(locationCasing(contained))){
            return true
          }
        }
      }
    }
    return false
  }
  
    /***
   * Return the a string from the document giving the context based on the KBP 
   * Query offsets. If a sentence can reasonably be found return it, else return
   * a section of characters
   */
  def getContextFromDocument(rawDoc: String, offset: Integer, name: String): String = {

    //initlaize context variable
    var context = ""
      
    val sentence = getSentenceFromDocWithParagraphFormat(rawDoc:String, offset:Integer, name: String)
    
    //if sentence was not found from <P> </P> format fall back onto less robust
    //sentence finders and character context finder
    if(sentence.isEmpty){
      //try using the sentence on docs without <P> format
      val unformattedSentence = getUnformattedSentenceFromDoc(rawDoc,offset,name)
      if(unformattedSentence.isEmpty)
        //if no sentence was found just get the character context
        context = getCharacterContext(rawDoc, offset, name)
      else
        context = unformattedSentence.get
    }
    else
      context = sentence.get
      
    //make context a single line
    context.replaceAll("\\s+", " ")
  }
  
   /***
   * Find a sentence from the Document if it can be found by using the XML <P> </P> format
   */
  private def getSentenceFromDocWithParagraphFormat(rawDoc: String, offset:Integer, name: String) :Option[String] = {
    
      try{
          var nameOffset = offset
		  if(offset == -1){
		    nameOffset = rawDoc.indexOf(name)
		  }
		  val lines = rawDoc.split("\n")
		  
		  var charsRead = 0
		  var lastP = 0
		  var paragraphInterval :Option[Interval] = None
		  var index =0
		  
		  //iterate over <P> until the Paragraph envelops the offset of the entity
		  while(paragraphInterval.isEmpty){
		    val line = lines(index)
		    if(line == "<P>"){
		      lastP = charsRead
		    }
		    
		    if(line == "</P>" && charsRead > nameOffset){
		      paragraphInterval = Some(Interval.closed(lastP,charsRead))
		    }
		    
		    index += 1
		    charsRead += (line.size + 1)
		  }
		  
		  //if the paragraph is not found return None
		  if(paragraphInterval.isEmpty){
		    return None
		  }
		  
		  //if the paragraph is found use the OpenNlpSentencer
		  else{
		    val paragraph = rawDoc.slice(paragraphInterval.get.start, paragraphInterval.get.end)
		    //remove remaining xml from paragraph
	        val paragraphMinusAllXML = xmlTagPattern.replaceAllIn(paragraph,"")
	        
	        //use the OpenNlpSentence to find the first sentence that contains
	        //the entity name
	        val sentences = sentencer.segmentTexts(paragraphMinusAllXML)
		    val entitySentence = sentences.filter(p => p.contains(name)).headOption
		    if(entitySentence.isDefined){
		      return Some(entitySentence.get.trim())
		    }
		    //if no appropriate sentence found return None
		    else{
		    return None
		    }
		  }
      }
      catch {
        case e: Exception => {
          None
        }
      }
  }
  
  /**
   * Try finding a sentence with the OpenNlpSentence without the helpful
   * <P> and </P> XML tags
   */
  private def getUnformattedSentenceFromDoc(rawDoc:String, offset:Integer, name:String) : Option[String] = {
    var nameOffset = offset
    if(offset == -1){
      nameOffset = rawDoc.indexOf(name)
    }
    val leftcharSeq = rawDoc.slice(0, nameOffset)
    val rightcharSeq = rawDoc.slice(nameOffset,rawDoc.size)
    val afterXML = leftcharSeq.reverse.takeWhile(p => {p != '>'}).reverse
    val beforeXML = rightcharSeq.takeWhile(p => {p != '<'})
    val text = afterXML + beforeXML
    val sentences = sentencer.segmentTexts(text)
    for(s <- sentences){
      if(s.contains(name))
        return Some(s)
    }
    None
  }
  
   /**
   * Take up to 100 characters, take off first and last char sequences and 
   * turn the chars into a single line string
   */
  private def getCharacterContext(rawDoc:String, offset:Integer, name: String): String = {
    var nameOffset = offset
    if(offset == -1){
      nameOffset = rawDoc.indexOf(name)
    }
    val charContext = rawDoc.slice(nameOffset-50, nameOffset+50)
    
    val charContextArray = charContext.split("\\s")
    val tokenizedContext = charContextArray.slice(1, charContextArray.length-1).mkString(" ")
	xmlTagPattern.replaceAllIn(tokenizedContext,"")
    
  }
  
  def getWideContextFromDocument(rawDoc:String, offset: Integer, name:String): String = {
    var nameOffset = offset
    if(offset == -1){
      nameOffset = rawDoc.indexOf(name)
    }
    val leftcharSeq = rawDoc.slice(0, nameOffset)
    val rightcharSeq = rawDoc.slice(nameOffset,rawDoc.size)
    val afterXML = leftcharSeq.reverse.takeWhile(p => {p != '>'}).reverse
    val beforeXML = rightcharSeq.takeWhile(p => {p != '<'})
    val text = afterXML + beforeXML
    text
  }
  
   def getHeadLineContextFromDocument(rawDoc: String): List[String] = {
    var contextList = List[String]()
    val headLineRegex = """<HEADLINE>([^<]+)</HEADLINE>""".r
    val headlineMatch = headLineRegex.findFirstMatchIn(rawDoc)
    if(headlineMatch.isDefined){
       contextList = headlineMatch.get.group(1).replaceAll("\\s+", " ").replaceAll("-", " ").trim() :: contextList
    }
    
    val paragraphRegex = """<P>([^<]+)</P>""".r
    val firstParagraphMatch = paragraphRegex.findFirstMatchIn(rawDoc)
    if(firstParagraphMatch.isDefined){
       contextList = firstParagraphMatch.get.group(1).replaceAll("\\s+", " ").replaceAll("-"," ").trim() :: contextList
    }
    contextList.toList
  }
  

}