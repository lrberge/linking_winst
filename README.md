## Linking inventors to Swedish registry data

This repository contains the code to complete a linking exercise between Swedish 
inventors, using patent data, and Swedish identities, using Swedish registry data.

Note that for obivous confidentiality reasons, this repository does not conain any data.
It only contains the source code which should be applied to the data.

The source code is geared towards being user friendly for the operator and, as such, 
there is only 5 lines of code to be run to complete the whole exercise (provided the
source data has been properly set up).

Details on how to replicate the matching exercise is provided in the document [FORTE_documentation.pdf](https://github.com/lrberge/linking_forte/blob/main/FORTE-Documentation.pdf).

### Structure of the code

The main algorithm consists in 4 main steps:
1. matching on names
1. creating bilateral variables 
1. applying an EM algorithm to tell the right matches from wrong matches
1. extra step identification using 

### Acknowledgements

This work has been supported by the FORTE-project, grant number 2021-01552_3.
