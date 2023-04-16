#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Thu Jan 12 00:04:41 2023
# ~: Main user-level document
#----------------------------------------------#

# INFO:
# - run the functions in the given order
# - the function step0_cleaning_se is slow because it is the importation
# of the large Swedish data files, further the non-UTF8 encoding 
# make reading the files extra slow (I need to rely on read.delim() which isn't fast)
# Anyway, it should be run just once. The function has been optimized for RAM requirements
# and not speed.
# - if the data importation works fine, there should be no bug in these functions. 
# If there is, please contact me at 
#   laurent.berge@u-bordeaux.fr
# => we may need a visio to debug quickly
#

# - the functions don't return anything!
# - they create files on the hard drive, in the _DATA folder
# (some in _DATA/_RAW for the STEP0 function)
# - the functions "remember" what has been done, so if the files were already
# created, nothing will happen. To recreate the files, you can use the argument
# hard = TRUE (all functions have this argument)
# example: step0_STAT_SE_names(hard = TRUE)

# - to explore the results of the functions, all files created by them are 
# detailed in FORTES-documentation.pdf
# - to read the ".fst" files, use the function readfst() or read_fst()

# - all functions are located in the file src_algo_steps.R
# - they make heavy use of the functions located in src_utilities.R
# - over 100 functions were created for this project. Some large functions are documented, 
# this is not the case for smaller or simpler functions 

####
#### STEP 0: Importation and cleaning ####
####


#
# Statistics Sweden
#

step0_cleaning_se()


#
# OECD patent data
#

step0_cleaning_patents()


####
#### STEP 1: Name matching ####
####


step1_name_matching()


####
#### STEP 2: Bilateral variables ####
####


step2_bilateral_vars()


####
#### STEP 3: EM matching ####
####


step3_EM_algorithm()


####
#### STEP 4: Adding patent information and rematching ####
####


step4_patent_match()


####
#### STEP 5: Descriptive data ####
####

# NOTE:
# - This data set is optional. 
# - its objective is to aggregate the data to investigate the quality of the matching

step5_descriptive()



####
#### General functions ####
####

# INFO:
# - this section illustrates two functions which can readily be applied to any other project:
#   * match_by_name: critical step in which persons from 2 data sets are matched by name
#   * em_matching: creation of groups based on variables, provides a probability to belong to 
#     each group. Critical to determine whether two identities are the same.
# - there is one example for each function, directly taken from this project


####
#### ... match_by_name ####
####

# INFO:
# - the documentation of this function is above its definition in src_utilities.R
# - you can CTRL+click on the function name to jump there directly
#

# Loading the inventor and persons names from two data sets
base_inv_names = readfst("_DATA/PATENT_inv-names.fst", forte = TRUE)
base_se_names = readfst("_DATA/STAT-SE_indiv-names.fst", forte = TRUE)

# we take a small subsample (it is just for illustration)
base_inv_names = head(base_inv_names, 1e6)
base_se_names = head(base_se_names, 1e6)

base_match = match_by_name(base_inv_names, base_se_names, 
                           by = c("F" = "first_name_1", "S" = "first_name_2", "T" = "first_name_3", 
                                  "L" = "fam_name_1", "M" = "fam_name_2"),
                           initials = "S, T", fuzzy = "F, S, T, L, M", empty = "S, T, M", 
                           allow.swap = "L, M",
                           varkeep = c("name_raw", "id_inv_seq", "id_se", "id_se_seq"),
                           prefix = c("inv_", "se_"), 
                           keep.only = ~ (year_prio - birth_year) >= 17) # you need to be at least 17 to patent


####
#### ... em_matching ####
####

# INFO:
# - the documentation of this function is above its definition in src_EM.R
# - you can CTRL+click on the function name to jump there directly
#

# Loading bilateral data:
# - pairs of id_se x id_inv_seq which are "potential" matches
# - contains many informative variables
base_bilat = readfst("_DATA/ALGO-2_all.fst", forte = TRUE)

# We reduce the sample and create an extra variable
base_bilat_small = base_bilat[n_match <= 5]
base_bilat_small[, name_prob_bin := bin(proba_name_rank, c("cut::3", "low", "mid", "high"))]
base_bilat_small[, match_qual_fact := ref(match_qual, .("low" = 1, "mid" = 2, "high" = 3))]
base_bilat_small[, prob_qual := ref(paste0("p:", name_prob_bin, "_q:", match_qual_fact))]

# The initialization: note that it can be missing (then it falls back to random assignment)
# Even with different initializations, the resulting classification is always the the same (for this data).
init = 1 * (base_bilat_small$address_match == "matched")

res_em = em_matching(base_bilat_small, c("address_match", "age", "same_emp", "prob_qual"), 
                     init = init, M = 2, em.iter = 500)

# To have an overview of the results:
summary(res_em)

plot(res_em)









