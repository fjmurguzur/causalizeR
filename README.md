# causalizeR
A simple R algorithm to extract causal linkages between words in unstructured texts

Technical details and description can be found in the associated publication:

This package allows to extract the effects of drivers on the ecosystem from literature as a way to summarize the current knowledge about nature. 

To install the package, install directly from github:

>devtools::install_github("fjmurguzur/causalizeR")

_Example usage_

>library(causalizeR)
>library(udpipe)
>
>example.data<-"Predation decreases herbivore populations"
>causalize(example.data,"decrease",(-1))
