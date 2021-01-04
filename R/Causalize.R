
#' Causal link extraction from unstructured texts
#'
#' Extracts the causal relation in texts
#'
#' @param texts The vector (or column of a data frame) containing the documents to analyze.
#' @param effect a keyword (character string) to extract nouns before and after it
#' @param effect_num the weight of the effect and its direction (positive or negative)
#'
#'
#' @return A data.frame with the driver, affected parameter, effect, effect weight and the text number
#'
#' @examples
#' library(coreNLP)
#' initCoreNLP()
#' example.data<-"Predation decreases herbivore populations"
#' causalize(example.data,"decrease",(-1))
#'
#' @export
causalize<-function(texts=NULL,effect=NULL,effect_num=0){

  driver<-list()
  affected<-list()
  res.mat<-list()


  for(w in 1:length(texts)){
    annotated_text<-as.data.frame(annotateString(texts[w])$token)

    sent.num<-NULL

    for(i in 1:length(unique(annotated_text$sentence))){
      if(effect%in%annotated_text$lemma[annotated_text$sentence==i]){
        sent.num<-c(sent.num,i)
      }

    }

    if(is.null(sent.num)){}else{ #Check that the sent.num is not empty

      for(j in sent.num){

        sent1<-annotated_text[annotated_text$sentence==j,]

        ##Ensure that there are nouns before and after the verb. Skip sentence if this requirement is not met
        if(which(sent1$lemma==effect)==1){}else{
          if("NN"%in%sent1$POS[(which(sent1$lemma==effect)-1):1]){ #If it is TRUE that there are no NOUNS before the verb, skip the sentence
            if("NN"%in%sent1$POS[(which(sent1$lemma==effect)+1):length(sent1$lemma)]){ #If it is TRUE that there are no NOUNS after the verb, skip the sentence
              if (length(grep("in response to|\\bby\\b|\\bwith\\b",sent1))>0){ #If there is a "in response to" or "by", then we have a passive voice
                #e.g. A increased in response to B ->A=affected//B=driver

                res.mat[[length(res.mat)+1]]<-cbind.data.frame(passivevoice(effect=effect,sentence=sent1),effect_num=effect_num,text_number=w)

              }
              else{
                res.mat[[length(res.mat)+1]]<-cbind.data.frame(activevoice(effect=effect,sentence=sent1),effect_num=effect_num,text_number=w)

              } #Close the conditional for active voice (the "else" command)
            } #Close sentence loop if sent.num is not null
          } #Close the loop if the effect (verb) is in the first position
        }
      }
    }

  } #Close full sentence loop, document scan finished

  return(do.call(rbind.data.frame, res.mat))

}

