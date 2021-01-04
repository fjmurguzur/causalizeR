#POS labels are based on https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html


#' Active voice analyses
#'
#' Extracts the causal relation in active sentences
#' For internal and experimental use

#' @return The drivers and the effect
#' @param effect the keyword to use as the "effect" (e.g. increase, decrease, promote)
#' @param sentence the sentence to be analyzed in active voice, parsed by coreNLP

#' @export

activevoice<-function(effect=NULL,sentence=NULL){


  for(k in (which(sentence$lemma==effect)-1)[1]:1){

      if( sentence$POS[k]=="NN"||sentence$POS[k]=="NNS"){
        if(k==1){ driver[[length(driver)+1]]<<-sentence$lemma[k]}else
        if(sentence$POS[k-1]=="NN"||sentence$POS[k-1]=="NNS"||sentence$POS[k-1]=="JJ"){

        driver[[length(driver)+1]]<<-paste(sentence$lemma[k-1],sentence$lemma[k],collapse="_")} else{
          driver[[length(driver)+1]]<<-sentence$lemma[k]}
    } #Close the loop that finds a NOUN before the "effect"

      if( sentence$POS[k]=="NN"){break}


  } #Close the driver loop


  for(m in (which(sentence$lemma==effect)+1)[1]:length(sentence$lemma)){


      if( sentence$POS[m]=="NN"){
        if(m==length(sentence$lemma)){
          if(sentence$POS[m-1]=="JJ"){affected[[length(affected)+1]]<<-paste(sentence$lemma[m-1],sentence$lemma[m],collapse="_")} else {
          affected[[length(affected)+1]]<<-sentence$lemma[m]}} else
        if(sentence$POS[m+1]=="NN"||sentence$POS[m+1]=="NNS"){  # for "JJ" it is m-1 because adjectives always come before the sustantive
         affected[[length(affected)+1]]<<-paste(sentence$lemma[m],sentence$lemma[m+1],collapse="_")}

        else{
           affected[[length(affected)+1]]<<-sentence$lemma[m]
         }
                    if( sentence$POS[m]=="NN"){break}

        } #Close the loop that finds a NOUN after the "effect"

  } #Close the affected loop

} #Close the conditional that looks for nouns before and after the verb




#' Passive voice analyses
#'
#' Extracts the causal relation in passive sentences
#' For internal and experimental use
#' @return The drivers and the effect
#' @param effect the keyword to use as the "effect" (e.g. increase, decrease, promote)
#' @param sentence the sentence to be analyzed in active voice, parsed by coreNLP

#' @export

passivevoice<-function(effect=NULL,sentence=NULL){

  for(k in (which(sentence$lemma==effect)-1)[1]:1){

    if( sentence$POS[k]=="NN"||sentence$POS[k]=="NNS"){
      if(k==1){ affected[[length(affected)+1]]<<-sentence$lemma[k]}else
        if(sentence$POS[k-1]=="NN"||sentence$POS[k-1]=="NNS"||sentence$POS[k-1]=="JJ"){

          affected[[length(affected)+1]]<<-paste(sentence$lemma[k-1],sentence$lemma[k],collapse="_")} else{
            affected[[length(affected)+1]]<<-sentence$lemma[k]}
    } #Close the loop that finds a NOUN before the "effect"

    if( sentence$POS[k]=="NN"){break}


  } #Close the driver loop


  for(m in (which(sentence$lemma==effect)+1)[1]:length(sentence$lemma)){


    if( sentence$POS[m]=="NN"){
      if(m==length(sentence$lemma)){
        if(sentence$POS[m-1]=="JJ"){affected[[length(affected)+1]]<<-paste(sentence$lemma[m-1],sentence$lemma[m],collapse="_")} else {
          driver[[length(driver)+1]]<<-sentence$lemma[m]}} else
            if(sentence$POS[m+1]=="NN"||sentence$POS[m+1]=="NNS"){  # for "JJ" it is m-1 because adjectives always come before the sustantive
              driver[[length(driver)+1]]<<-paste(sentence$lemma[m],sentence$lemma[m+1],collapse="_")}

      else{
        driver[[length(driver)+1]]<<-sentence$lemma[m]
      }
      if( sentence$POS[m]=="NN"){break}

    } #Close the loop that finds a NOUN after the "effect"

  } #Close the affected loop

} #Close the conditional that looks for nouns before and after the verb








#' Causal link extraction from unstructured texts
#'
#' Extracts the causal relation in texts
#'
#' @param texts The data frame containing the documents to analyze.
#' @param effect a keyword (character string) to extract nouns before and after it
#' @param effect_num the weight of the effect and its direction (positive or negative)
#' @param object_name how should the causal relation dataset be named?
#'
#' @return A data.frame with the driver, affected parameter, effect, effect weight and the text number
#'
#' @example
#' example.data<-data.frame(AB="Predation decreases herbivore populations")
#' causalize(example.data,"decrease",(-1))
#'
#' @export
causalize<-function(texts=NULL,effect=NULL,effect_num=0){

  driver<-list()
  affected<-list()


for(w in 1:length(texts)){

  sent.num<-NULL

  for(i in 1:length(unique(x$sentence_id))){
    if(grepl(paste0(effect),x$sentence[x$sentence_id==i][1])==TRUE){
      sent.num<-c(sent.num,i)
    }

  }

  if(is.null(sent.num)){}else{ #Check that the sent.num is not empty

    for(j in sent.num){

      sent1<-x[x$sentence_id==j,]

      ##Ensure that there are nouns before and after the verb. Skip sentence if this requirement is not met
      if(which(sent1$lemma==effect)==1){}else{
        if("NOUN"%in%sent1$upos[(which(sent1$lemma==effect)-1):1]){ #If it is TRUE that there are no NOUNS before the verb, skip the sentence
          if("NOUN"%in%sent1$upos[(which(sent1$lemma==effect)+1):length(sent1$lemma)]){ #If it is TRUE that there are no NOUNS after the verb, skip the sentence
            if (length(grep("in response to|\\bby\\b|\\bwith\\b",sent1))>0){ #If there is a "in response to" or "by", then we have a passive voice
              #e.g. A increased in response to B ->A=affected//B=driver
              activevoice()
            }
            else{
              passivevoice()
            } #Close the conditional for active voice (the "else" command)
          } #Close sentence loop if sent.num is not null
        } #Close the loop if the effect (verb) is in the first position
      }
    }
  }
  if ( is.null(driver)){} else if(is.null(affected)){} else{
    res.mat[[length(res.mat)+1]]<-cbind.data.frame(driver=as.factor(driver),effect_name=effect,affected=as.factor(affected),effect_direction=effect_num,abs_number=w)
  }
} #Close full sentence loop, document scan finished

  causal_data<<-do.call(rbind.data.frame, res.mat)

}
