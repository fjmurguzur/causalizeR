This README file was generated on 2021-06-14 by Francisco Javier Ancin-Murguzur

---------------------
GENERAL INFORMATION
---------------------

1. Title of dataset: Replication Data for: causalizeR: A text mining algorithm to identify causal relationships in scientific literature
2. DOI: 
3. Contact information:
	Francisco Javier Ancin-Murguzur
	UiT - The Arctic University of Norway
	Email: francisco.j.murguzur@uit.no

---------------------------
DESCRIPTION
---------------------------
Complex interactions among multiple abiotic and biotic drivers result in rapid changes in ecosystems worldwide. Predicting how specific interactions can cause ripple effects potentially resulting in abrupt shifts in ecosystems is of high relevance to policymakers, but difficult to quantify using data from singular cases. We present causalizeR (https://github.com/fjmurguzur/causalizeR), a text-processing algorithm that extracts causal relations from literature based on simple grammatical rules that can be used to synthesize evidence in unstructured texts in a structured manner. The algorithm extracts causal links using the relative position of nouns relative to the keyword of choice to extract the cause and effects of interest. The resulting database can be combined with network analysis tools to estimate the direct and indirect effects of multiple drivers at the network level, which is useful for synthesizing available knowledge and for hypothesis creation and testing. We illustrate the use of the algorithm by detecting causal relationships in scientific literature relating to the tundra ecosystem.

The package uses the text annotation tools from the package "udpipe"

----Database creation and processing----

We searched the Elsevier Scopus (Scopus) database for relevant publications using the search string TITLE-ABS-KEY (tundra) on 26 November 2019. 

Then we used the causalizeR algorithm to demonstrate how causal relationships in the ecosytem can be extracted from unstructured texts

Updated versions of the package can be found in Zenodo (https://doi.org/10.5281/zenodo.4817639) and gitHub (https://github.com/fjmurguzur/causalizeR)
---------------------
DATA & FILE OVERVIEW
---------------------

The "CausalizeR.zip" file contains a backup of the package to ensure long-term storage
The "Data.zip" file contains the bibliographic dataset in .bib format
The "Script-1.R" file contains the script where the necessary steps are explained at every step.

--------------
USAGE EXAMPLE
--------------

#To install the package, install directly from github:

devtools::install_github("fjmurguzur/causalizeR")

library(udpipe)

example.data<-"Predation decreases herbivore populations"
causalize(example.data,"decrease",(-1))
