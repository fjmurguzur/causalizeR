


#' Passive voice analyses
#'
#' Extracts the causal relation in passive sentences
#' For internal and experimental use
#' @return The drivers and the effect
#' @param effect the keyword to use as the "effect" (e.g. increase, decrease, promote)
#' @param sentence the sentence to be analyzed in active voice, parsed by coreNLP

#' @export

passivevoice<-function(effect=NULL,sentence=NULL){

  driver<-NULL
  affected<-NULL
  for(k in (which(sentence$lemma==effect)-1)[1]:1){

    if( sentence$upos[k]=="NOUN"){
      if(k==1){ affected<-sentence$lemma[k]}else
        if(sentence$upos[k-1]=="NOUN"||sentence$upos[k-1]=="ADJ"){

          affected<-paste(sentence$lemma[k-1],sentence$lemma[k],collapse="_")}
      else{
            affected<-sentence$lemma[k]}
    } #Close the loop that finds a NOUN before the "effect"

    if( sentence$upos[k]=="NOUN"){break}


  } #Close the driver loop


  for(m in (which(sentence$lemma==effect)+1)[1]:length(sentence$lemma)){


    if( sentence$upos[m]=="NOUN"){
      if(m==length(sentence$lemma)){
        if(sentence$upos[m-1]=="ADJ"){driver<-paste(sentence$lemma[m-1],sentence$lemma[m],collapse="_")} else {
          driver<-sentence$lemma[m]}} else
            if(sentence$upos[m+1]=="NOUN"){  # for "JJ" it is m-1 because adjectives always come before the sustantive
              driver<-paste(sentence$lemma[m],sentence$lemma[m+1],collapse="_")}

      else{
        driver<-sentence$lemma[m]
      }
      if( sentence$upos[m]=="NOUN"){break}

    } #Close the loop that finds a NOUN after the "effect"

  } #Close the affected loop
  if(!is.null(driver)&&!is.null(affected)){
    return(cbind.data.frame(driver=driver,affected=affected,effect=effect))
  }
}



