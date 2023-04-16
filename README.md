## Linking inventors to Swedish registry data

This repository contains the code to complete a linking exercise between Swedish 
inventors, using patent data, and Swedish identities, using Swedish registry data.

Note that for obivous confidentiality reasons, this repository does not conain any data.
It only contains the source code which should be applied to the data.

The source code is geared towards being user friendly for the operator and, as such, 
there is only 7 lines of code to be run to complete the whole exercise (provided the
source data has been properly set up).
The downside of this is that the source code is harder to read (especially for R beginners).

Details on how to replicate the matching exercise is provided in the document [FORTE_documentation.pdf](https://github.com/lrberge/linking_forte/blob/main/pdf/FORTE-Documentation.pdf).
Reading this document is highly recommended for setting up R properly.

### Structure of the code

The algorithm used in this project is described in the [proposal](https://github.com/lrberge/linking_forte/blob/main/pdf/Proposal_Berge.pdf). 
It consists in 4 main steps:
1. matching on names
1. creating bilateral variables 
1. applying an EM algorithm to tell the right matches from wrong matches
1. extra identification step using patent information

To these four steps, there are two more to add: the initial cleaning of the input data, and the creation of a 
final data set for information purposes.

You can find all the source for all the steps in the file [src_algo_steps.R](https://github.com/lrberge/linking_forte/blob/main/src_algo_steps.R).

### Acknowledgements

This work has been supported by the FORTE-project, grant number 2021-01552_3.
