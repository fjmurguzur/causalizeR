


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

    if( sentence$POS[k]=="NN"||sentence$POS[k]=="NNS"){
      if(k==1){ affected<-sentence$lemma[k]}else
        if(sentence$POS[k-1]=="NN"||sentence$POS[k-1]=="NNS"||sentence$POS[k-1]=="JJ"){

          affected<-paste(sentence$lemma[k-1],sentence$lemma[k],collapse="_")} else{
            affected<-sentence$lemma[k]}
    } #Close the loop that finds a NOUN before the "effect"

    if( sentence$POS[k]=="NN"){break}


  } #Close the driver loop


  for(m in (which(sentence$lemma==effect)+1)[1]:length(sentence$lemma)){


    if( sentence$POS[m]=="NN"){
      if(m==length(sentence$lemma)){
        if(sentence$POS[m-1]=="JJ"){affected<-paste(sentence$lemma[m-1],sentence$lemma[m],collapse="_")} else {
          driver<-sentence$lemma[m]}} else
            if(sentence$POS[m+1]=="NN"||sentence$POS[m+1]=="NNS"){  # for "JJ" it is m-1 because adjectives always come before the sustantive
              driver<-paste(sentence$lemma[m],sentence$lemma[m+1],collapse="_")}

      else{
        driver<-sentence$lemma[m]
      }
      if( sentence$POS[m]=="NN"){break}

    } #Close the loop that finds a NOUN after the "effect"

  } #Close the affected loop
  if(!is.null(driver)&&!is.null(affected)){
    return(cbind.data.frame(driver=driver,affected=affected,effect=effect))
  }
}



