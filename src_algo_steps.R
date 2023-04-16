#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Jan 09 23:59:11 2023
# ~: functions computing each step of the
# algorithm
#----------------------------------------------#


# we create the appropriate folders if needed
if(!file.exists("_DATA/_RAW")){
  dir.create("_DATA/_RAW", recursive = TRUE)
}


####
#### STEP 0: Cleaning ####
####

####
#### ... STAT-SE ####
####


step0_cleaning_se = function(hard = FALSE){
  # hard: recreates the data sets even if they have already been created
  
  # STAT-SE stands for 'Statistics Sweden'
  
  step0_STAT_SE_indiv(hard)
  
  step0_STAT_SE_emp(hard)
  
  step0_STAT_SE_add(hard)
  
  step0_STAT_SE_firstnames(hard)
  
  step0_STAT_SE_names(hard)
  
}

step0_STAT_SE_indiv = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  
  # NOTE:
  # - this function is very long to run, the importation of the data is 
  # slow because of the encoding
  # - I also ensured to construct an aggregated data set which combines the information of multiple identical rows
  # => the way I have implemented is deliberately slow but saves a lot of RAM requirements
  # 
  
  outfile = start_outfile("_DATA/_RAW/STAT-SE_indiv.fst")
  
  # sanity checks
  
  folder = getOption("LOCATION_STAT_SE")
  if(is.null(folder)) folder = "_DATA/_RAW/STAT-SE"
  
  existing_files = list.files(folder)
  if(!any(grepl("^OE_lev_RTB_\\d{4}\\.txt", existing_files))){
    stop(dsb("It seems that the folder `.[folder]` does not contain the following source STAT-SE data sets:\n",
             "- OE_lev_RTB_xxxx.txt (with xxxx the year)\n Please put these files into the folder for the code to run.\n",
             "See FORTE-documentation.pdf for more information."))
  }
  
  #
  # STAT-SE_indiv.fst
  #
  
  # Dictionary renaming the variables
  dict_rtb = "
LopNr: id_se
Kon: gender
FodelseAr: birth_year
TillTal: firstname_code
FNamn: firstname_all       # later I'll create firstname_first, firstname_second, etc
MNamn: maiden_name        
ENamn: family_name
Adress: street_location    # street + number in the street 
PostNr: postcode
PostOrt: city
"
  dict_from = dsb("'\n's, ':.+'R, w ? dict_rtb")
  dict_to   = dsb("'\n's, '.+:|#.+'R, w ? dict_rtb")
  
  # NOTE:
  # We remove the variables:
  # - CoAdr (equivalent to "lives at", when the person live at sbdy's place with a different name)
  # - Namn: simple concatenation equal to "last_names, first_names". I didn't see any 
  # value added to it despite looking at 100s of records
  
  rtb_paths = list.files(folder, full.names = TRUE, pattern = "RTB.+\\.txt$")
  rtb_paths = sort(rtb_paths)
  
  base_rtb = NULL
  
  for(i in seq_along(rtb_paths)){
    
    i_start = Sys.time()
    
    # reading the file with the appropriate encoding
    path = rtb_paths[i]
    file_year = as.numeric(dsb("'[[:digit:]]{4}'x ? path"))
    
    cat(sprintf("%02i/%i, %i .", i, length(rtb_paths), file_year))
    
    f = file(path, encoding = "ISO-8859-1")
    new_rtb = read.delim(f, sep = "\t")
    setDT(new_rtb)
    
    cat(".")

    #
    # Cleaning the data base
    #

    # Removing unneeded variables
    var_rm = setdiff(names(new_rtb), dict_from)
    for(v in var_rm){
      set(new_rtb, j = v, value = NULL)
    }

    # Renaming the variables
    setnames(new_rtb, dict_from, dict_to, skip_absent = TRUE)
    
    # Turning character NAs to ""
    setNA_char(new_rtb)

    # converting to lowercase
    vars_lower = extract_vars(new_rtb, "^firstname, ^family, ^maiden", skip_absent = TRUE)
    for(v in vars_lower){
      set(new_rtb, j = v, value = tolower(new_rtb[[v]]))
    }

    # we lower + ASCII
    vars_clean = extract_vars(new_rtb, "^street, city")
    for(v in vars_clean){
      set(new_rtb, j = v,
          value = cleanCharVector(new_rtb[[v]], forceASCII = TRUE, changeCase = "lower"))
    }
    
    cat(".")
    
    #
    # Adding it to the previous observations
    #
    
    if(i == 1){
      base_rtb = new_rtb
      base_rtb[, year_range := file_year]
    } else {
      base_rtb = aggregate_rows(base_rtb, new_rtb, key = "id_se", year = file_year)
    }
    
    cat(". ", format_difftime(i_start), "\n", sep = "")
    
    gc()
  }
  
  base_rtb[, year_start := as.numeric(str_extract(year_range, 4))]
  base_rtb[, year_end := as.numeric(str_extract(year_range, -4))]
  
  base_rtb = base_rtb[order(id_se, year_start)]
  base_rtb[, id_se_seq := 1:.N]
  
  write_fst_forte(base_rtb, outfile)
  
}

step0_STAT_SE_emp = function(hard = FALSE, debug = FALSE){
  # debug: argument controlling whether to implement a debugging procedure
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/STAT-SE_jobb.fst")
  
  # sanity checks
  
  folder = getOption("LOCATION_STAT_SE")
  if(is.null(folder)) folder = "_DATA/_RAW/STAT-SE"
  
  existing_files = list.files(folder)
  if(!any(grepl("^OE_lev_Jobb_\\d{4}\\.txt", existing_files))){
    stop(dsb("It seems that the folder `.[folder]` does not contain the following source STAT-SE data sets:\n",
             "- OE_lev_Jobb_xxxx.txt (with xxxx the year)\n Please put these files into the folder for the code to run.\n",
             "See FORTE-documentation.pdf for more information."))
  }
  
  #
  # _DATA/STAT-SE_jobb.fst
  #
  
  # Dictionary
  dict_jobb = as.dict("
LopNr: id_se
FtgNamn: firm_name
FtgAdr: street_full
FtgPostNr: postcode
FtgPostAnst: city
")
  
  dict_from = names(dict_jobb)
  dict_to   = dict_jobb
  
  # NOTE:
  # We drop the following variables:
  # - LopNr_PeOrgNr, AstKommun, FtgKommun
  # => they are not needed since we use directly the firms names and addresses
  
  # Main loop
  jobb_paths = list.files(folder, full.names = TRUE, pattern = "Jobb.+\\.txt$")
  jobb_paths = sort(jobb_paths)
  
  if(debug){
    sink("DEBUG_step0_STAT_SE_emp.txt")
    on.exit(sink())
  }
  
  base_jobb = NULL
  for(i in seq_along(jobb_paths)){
    
    i_start = Sys.time()
    
    # Importing the file
    path = jobb_paths[i]
    file_year = as.numeric(dsb("'[[:digit:]]{4}'x ? path"))
    
    cat(sprintf("%02i/%i, %i .", i, length(jobb_paths), file_year))
    
    f = file(path, encoding = "ISO-8859-1")
    new_jobb = read.delim(f, sep = "\t")
    setDT(new_jobb)
    
    if(debug){
      cat("\n  general cleaning")
    }
    
    cat(".")
    
    #
    # Cleaning the data set
    #
    
    # Removing unneeded variables
    var_rm = setdiff(names(new_jobb), dict_from)
    for(v in var_rm){
      set(new_jobb, j = v, value = NULL)
    }
    
    # Renaming the variables
    setnames(new_jobb, dict_from, dict_to, skip_absent = TRUE)
    
    # turning NAs to ""
    setNA_char(new_jobb)
    
    # we lower + ASCII
    vars_clean = extract_vars(new_jobb, "firm_name, ^street, city")
    for(v in vars_clean){
      set(new_jobb, j = v, 
          value = cleanCharVector(new_jobb[[v]], forceASCII = TRUE, changeCase = "lower"))
    }
    
    if(debug){
      cat("\n  cleaning employer name")
    }
    
    cat(".")
    
    # we clean the employer name
    new_jobb[, firm_name := clean_employer_name_se(firm_name)]
    
    if(debug){
      cat("\n  extracting addresses")
    }
    
    cat(".")
    
    # Formatting the addresses
    new_jobb_add = with(new_jobb, extract_se_address_statse(postcode, city, street_full, silent = TRUE))
    new_jobb_add = cbind(new_jobb[, .(id_se, firm_name)], new_jobb_add)
    
    if(debug){
      cat("\n  aggregating the rows")
    }
    
    cat(".")
    
    #
    # Adding it to the previous observations
    #
    
    if(i == 1){
      base_jobb = new_jobb_add
      base_jobb[, year_range := file_year]
    } else {
      base_jobb = aggregate_rows(base_jobb, new_jobb_add, key = "id_se", year = file_year)
    }
    
    cat(". ", format_difftime(i_start), "\n", sep = "")
    
    gc()
  }
  
  # we rename and reorder
  base_jobb = base_jobb[, .(id_se, se_emp = firm_name, se_emp_postcode = postcode, 
                            se_emp_city = city, se_emp_street_name = street_name, 
                            se_emp_street_nb = street_nb, se_emp_building = building, 
                            year_range)]
  
  base_jobb[, year_start := as.numeric(str_extract(year_range, 4))]
  base_jobb[, year_end := as.numeric(str_extract(year_range, -4))]
  
  base_jobb = base_jobb[order(id_se, year_start)]
  
  write_fst_forte(base_jobb, outfile)
  
}


step0_STAT_SE_add = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/STAT-SE_indiv-addresses.fst")
  
  base_indiv = readfst("_DATA/_RAW/STAT-SE_indiv.fst", "id_se, ^post, city, ^street", forte = TRUE)
  
  base_address = extract_se_address_statse(base_indiv$postcode, base_indiv$city, 
                                           base_indiv$street_location, silent = TRUE)
  base_address[, id_se := base_indiv$id_se]
  
  base_address = unique(base_address)
  
  write_fst_forte(base_address, outfile)
  
}


step0_STAT_SE_firstnames = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/swedish_first_names.fst")
  
  base_indiv = readfst("_DATA/_RAW/STAT-SE_indiv.fst", "id_se, firstname_all", forte = TRUE)
  
  # we create a list of all the first names in Sweden
  all_names = unlist(strsplit(unique(base_indiv$firstname_all), " |-"))
  base_first_names = data.table(name = all_names)
  base_first_names = base_first_names[, .(freq = .N), by = name]
  base_first_names[, name_ascii := cleanCharVector(name, forceASCII = TRUE, cleanPunct = TRUE)]
  
  write_fst_forte(base_first_names, outfile)
  
}


step0_STAT_SE_names = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/STAT-SE_indiv-names.fst")
  
  base_indiv = readfst("_DATA/_RAW/STAT-SE_indiv.fst", 
                       "id_se, id_se_seq, @name, @year_(start|end), ^birth", forte = TRUE)
  
  # It should be direct
  
  firstname_all_raw = base_indiv$firstname_all
  fam_names_all_raw = paste(base_indiv$family_name, base_indiv$maiden_name)
  
  use_code = base_indiv$firstname_code
  
  #
  # FAMILY NAME
  
  # we:
  # - merge the ' as in "o'connor"
  # - separate the 'composed' names, as in charles-henri
  # - clean / as in "/topelius/"
  # - remove jr/ii/iii
  # - merge the particles
  
  # a = str_get("[^\\p{L} ]", fam_names_all)
  
  message("cleaning family names ...", appendLF = FALSE)
  
  fam_names_all = gsub("'", "", fam_names_all_raw, fixed = TRUE)
  fam_names_all = gsub("-", " ", fam_names_all, fixed = TRUE)
  
  fam_names_all = gsub("/", "", fam_names_all, fixed = TRUE)
  
  fam_names_all = str_clean(fam_names_all, c("(^| )(jr\\.?|ii|iii|dr)( |$)", "@t"))
  
  fam_names_all = merge_particle(fam_names_all)
  
  message("done")
  
  #
  # FIRST NAMES
  
  # Manipulation
  # - we separate the initials: e.r. => e. r.
  # - we clean the dots
  # - we separate the words in the first names and create the use names
  
  message("cleaning first names ...", appendLF = FALSE)
  
  # cleaning the dots
  firstname_all = gsub("\\.", " ", firstname_all_raw, perl = TRUE)
  firstname_all = gsub(" +", " ", firstname_all, perl = TRUE)
  
  message("done")
  
  #
  # create the "usage" names
  #
  
  message("creating usage names ...", appendLF = FALSE)
  
  uname_1 = uname_2 = character(length(firstname_all))
  
  # we also "detach" composed first names (this is how we should do)
  fnames_split = strsplit2df(firstname_all, split = " |-", addPos = TRUE)
  
  base_use = strsplit2df(use_code, split = "", addPos = TRUE)
  
  # We don't need use == "10" or use == "12" since they are embeded in the "normal" name search
  
  # id_ok: all use names that are exotic
  id_ok = which(!is.na(use_code) & !use_code %in% c("10", "12") & nchar(use_code) > 0)
  
  base_use[, ok := id %in% id_ok]
  base_use[, n := .N, by = id]
  
  base_use_1 = base_use[ok == TRUE & pos == 1, .(id, pos = as.numeric(x))]
  base_use_2 = base_use[ok == TRUE & pos == 2 & x != "0", .(id, pos = as.numeric(x))]
  
  tmp1 = merge(base_use_1, fnames_split, by = c("id", "pos"))
  uname_1[tmp1$id] = tmp1$x
  
  message(".", appendLF = FALSE)
  
  tmp2 = merge(base_use_2, fnames_split, by = c("id", "pos"))
  uname_2[tmp2$id] = tmp2$x
  
  message(".", appendLF = FALSE)
  
  #
  # First names
  #
  
  base_first_names = name_str_to_name_DB(firstname_all, "first_name_", sep = " |-")
  
  message(".", appendLF = FALSE)
  
  #
  # Last names
  #
  
  base_fam_names = name_str_to_name_DB(fam_names_all, "fam_name_", sep = " |-", nmax = 2)
  
  message(".", appendLF = FALSE)
  
  #
  # final DB
  #
  
  base_se_names_raw = base_indiv[, .(id_se_seq, id_se, firstname_all,
                                     family_name, maiden_name)]
  
  tmp = .cmerge(base_se_names_raw, base_first_names, 
                by.x = "id_se_seq", by.y = "id", all.x = TRUE,
                incl.y = TRUE, unik.y = TRUE)
  
  tmp2 = .cmerge(tmp, base_fam_names.IU, 
                 by.x = "id_se_seq", by.y = "id", all.x = TRUE,
                 incl.y = TRUE, unik.y = TRUE)
  
  base_se_names_raw = tmp2
  setNA(base_se_names_raw, "^first_name", "")
  
  message("done")
  
  #
  # COMPLETION
  #
  
  message("completing names", appendLF = FALSE)
  
  # Some first names are abbreviations, we correct them using later information
  base_se_names = base_se_names_raw
  
  base_se_names[, first_name_1 := complete_with_next(id_se, first_name_1)]
  base_se_names[, first_name_2 := complete_with_next(id_se, first_name_2)]
  base_se_names[, first_name_3 := complete_with_next(id_se, first_name_3)]
  
  message("done")
  
  
  #
  # DUPLICATION
  #
  
  message("adding usage name ...", appendLF = FALSE)
  
  # A) we add the usage first name
  
  stopifnot(length(uname_1) == nrow(base_se_names))
  
  qui_use = which(nchar(uname_1) > 0)
  base_usage = base_se_names[qui_use]
  base_usage[, first_name_1 := uname_1[qui_use]]
  base_usage[, first_name_2 := uname_2[qui_use]]
  base_usage[, first_name_3 := ""]
  
  base_se_names = rbindlist(list(base_se_names, base_usage))
  
  message("done")
  
  
  # B) we add the ö, ä and å variations
  
  # data = base_se_names
  # letter_dict = c("ö" = "oe")
  # vars = "(first|fam)_name_"
  
  message("accentuation variation ...", appendLF = FALSE)
  
  base_oe = dup_data_change_letter(base_se_names, c("ö" = "oe"), "(first|fam)_name_")
  
  message(".", appendLF = FALSE)
  
  base_ae = dup_data_change_letter(base_se_names, c("ä" = "ae"), "(first|fam)_name_")
  
  message(".", appendLF = FALSE)
  
  base_aa = dup_data_change_letter(base_se_names, c("å" = "aa"), "(first|fam)_name_")
  
  message(".", appendLF = FALSE)
  
  base_se_names = rbindlist(list(base_se_names, base_oe, base_ae, base_aa))
  
  #
  # ASCIIing
  #
  
  name_vars = str_get("(first|fam)_name_", names(base_se_names))
  for(v in name_vars){
    set(base_se_names, j = v, value = cleanCharVector(base_se_names[[v]], forceASCII = TRUE))
  }
  
  message("done")
  
  #
  # => we add the date and the id_se
  #
  
  se_dates = base_indiv[, "id_se_seq, year_start, year_end, ^birth"]
  
  tmp = .cmerge(base_se_names, se_dates, incl = TRUE, unik.y = TRUE)
  base_se_names = tmp
  
  #
  # full name information
  #
  
  base_se_names[, name_raw := trimws(paste0(firstname_all, ", ", family_name, " ", maiden_name))]
  base_se_names[, c("firstname_all", "family_name", "maiden_name") := NULL]
  
  #
  # save
  
  write_fst_forte(base_se_names, outfile)
  
}

####
#### ... patents ####
####


step0_cleaning_patents = function(hard = FALSE){
  # hard: recreates the data sets even if they have already been created
  
  step0_PATENT_inv(hard)
  
  step0_PATENT_app(hard)
  
  step0_PATENT_add(hard)
  
  step0_PATENT_add_missing(hard)
  
  step0_PATENT_names(hard)
  
  step0_PATENT_employer(hard)
  
}

step0_PATENT_inv = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/_RAW/OECD_inventors.fst")
  
  # sanity checks
  
  folder = getOption("LOCATION_OECD")
  if(is.null(folder)) folder = "_DATA/_RAW/OECD"
  
  data_sources = c("202202_EPO_Inv_reg_small.txt", "202202_EPO_IPC_small.txt")
  existing_files = list.files(folder)
  pblm = setdiff(data_sources, existing_files)
  if(length(pblm) > 0){
    stop(dsb("It seems that the folder `.[folder]` does not contain the following source OECD data sets:\n",
             ".['\n'c !- .[pblm]]\n Please put these files into the folder for the code to run.\n",
             "See FORTE-documentation.pdf for more information."))
  }
  
  # code
  
  base_inv = fread(dsb(".[folder]/202202_EPO_Inv_reg_small.txt"), encoding = "UTF-8")
  setnames(base_inv, 
           c("app_nbr",     "pub_nbr",     "ctry_code"), 
           c("epo_app_nbr", "epo_pub_nbr", "inv_country"))
  
  # We keep only swedish inventors 
  base_inv = base_inv[inv_country == "SE"]
  
  # We do not convert non-ASCII chars at the moment (will come later)
  base_inv[, inv_name := tolower(inv_name)]
  
  # we drop duplicate entries
  base_inv = unique(base_inv, by = c("appln_id", "inv_name"))
  
  # We do convert addresses to ASCII
  # update: I now clean the weird character encoding U0338
  #         utf8ToInt("\\U0338") => 824 (remove double backslash to make it work)
  cleaned_add = gsub(intToUtf8(824), "", base_inv$address, fixed = TRUE)
  base_inv[, inv_address := cleanCharVector(cleaned_add, forceASCII = TRUE, 
                                            changeCase = "lower")]
  # a few more failed encodings => turned into '_'
  base_inv[, inv_address := gsub("_", "", inv_address, fixed = TRUE)]
  
  base_inv[, c("reg_code", "reg_share", "inv_share", "address") := NULL]
  
  base_inv[, id_inv_seq := 1:.N]
  
  # we also add the priority date
  base_prio = fread(dsb(".[folder]/202202_EPO_IPC_small.txt"))
  base_prio = unique(base_prio[, .(appln_id, year_prio = prio_year)])
  base_inv = .cmerge(base_inv, base_prio, incl.x = TRUE, unik.y = TRUE)
  
  write_fst_forte(base_inv, outfile)
  
}

step0_PATENT_app = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/_RAW/OECD_applicants.fst")
  
  # sanity checks
  
  folder = getOption("LOCATION_OECD")
  if(is.null(folder)) folder = "_DATA/_RAW/OECD"
  
  data_sources = c("202202_EPO_App_reg_small.txt", "202202_EPO_IPC_small.txt")
  existing_files = list.files(folder)
  pblm = setdiff(data_sources, existing_files)
  if(length(pblm) > 0){
    stop(dsb("It seems that the folder `.[folder]` does not contain the following source OECD data sets:\n",
             ".['\n'c !- .[pblm]]\n Please put these files into the folder for the code to run.\n",
             "See FORTE-documentation.pdf for more information."))
  }
  
  # NOTE:
  #- the patents in 202202_EPO_App_reg_small.txt are all from swedish inventors
  
  base_app = fread(dsb(".[folder]/202202_EPO_App_reg_small.txt"), encoding = "UTF-8")
  setnames(base_app, 
           c("app_nbr",     "pub_nbr",     "ctry_code"), 
           c("epo_app_nbr", "epo_pub_nbr", "app_country"))
  
  # Here we transform to ASCII: that's OK
  clean_name = gsub(intToUtf8(824), "", base_app$app_name, fixed = TRUE)
  base_app[, app_name := cleanCharVector(clean_name, forceASCII = TRUE, changeCase = "lower")]
  
  # we remove weird U0338 characters
  clean_add = gsub(intToUtf8(824), "", base_app$address, fixed = TRUE)
  clean_add = gsub(intToUtf8(176), " ", clean_add, fixed = TRUE)
  base_app[, app_address := cleanCharVector(clean_add, 
                                            forceASCII = TRUE, 
                                            changeCase = "lower")]
  
  base_app[, c("reg_code", "reg_share", "app_share", "address") := NULL]
  
  write_fst_forte(base_app, outfile)
  
}

step0_PATENT_add = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/PATENT_inv-address.fst")
  
  base_inv = readfst("_DATA/_RAW/OECD_inventors.fst", forte = TRUE)
  
  base_inv_add = extract_se_address_patents(base_inv$inv_address)
  
  write_fst_forte(base_inv_add, outfile)
  
}

step0_PATENT_add_missing = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/PATENT_address-missing.fst")
  
  # INFO:
  # - we find out if the inventor's address is the one of the applicant
  
  # we select all the patents and look for all the inventors
  base_inv_se = readfst("_DATA/_RAW/OECD_inventors.fst", forte = TRUE)
  appln_id_select = sort(unique(base_inv_se$appln_id))
  
  # The address of the applicants
  base_app = readfst("_DATA/_RAW/OECD_applicants.fst", forte = TRUE)
  base_app_select = base_app[appln_id %in% appln_id_select & app_country == "SE", 
                             .(appln_id, app_name, app_country, app_address, app_name)]
  # We exclude individual applicants
  # names with a comma are ALMOST ALWAYS individuals. The rest can be found with "ab"
  base_app_select = base_app_select[!str_is(app_name, c("#,", "!\\b(ab|ltd|co)\\b"))]
  
  base_app_add = extract_se_address_patents(base_app_select$app_address)
  base_app_add[, appln_id := base_app_select$appln_id]
  
  # The address of the inventors
  base_inv_add = extract_se_address_patents(base_inv_se$inv_address)
  base_inv_add[, appln_id := base_inv_se$appln_id]
  base_inv_add[, obs := 1:.N]
  
  # We merge the applicants with the inventors and look at the proximity of the addresses
  base_left = base_inv_add[, .(appln_id, obs, inv_raw = raw, inv_city = city,
                               inv_postcode = postcode, inv_street = street_name,
                               inv_nb = street_nb)]
  base_right = base_app_add[, .(appln_id, app_raw = raw, app_city = city,
                                app_postcode = postcode, app_street = street_name,
                                app_nb = street_nb)]
  base_bilat = .cmerge(base_left, base_right, all.x = TRUE) 
  
  setNA(base_bilat, "^app_", "")
  
  base_bilat[, same_city := string_proxi(app_city, inv_city, method = "unitary")]
  base_bilat[, same_postcode := app_postcode == inv_postcode]
  base_bilat[, same_street := string_proxi(app_street, inv_street, method = "unitary")]
  
  base_bilat[, is_company := +((same_city == TRUE & same_postcode == TRUE & same_street == TRUE) | 
                                 grepl("\\b(c/o|ab|univ|university|instit\\w+)\\b", inv_raw, perl = TRUE) | 
                                 grepl("\\bbox\\b", inv_street) | 
                                 nchar(inv_street) == 0)]
  
  obs_missing = base_bilat[is_company == 1, obs]
  base_inv_no_address = base_inv_add[, .(obs)]
  base_inv_no_address[, address_missing := 1 * (obs %in% obs_missing)]
  
  setnames(base_inv_no_address, "obs", "id_inv_seq")
  
  write_fst_forte(base_inv_no_address, outfile)
  
}

step0_PATENT_names = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/PATENT_inv-names.fst")
  
  base_inv = readfst("_DATA/_RAW/OECD_inventors.fst", forte = TRUE)
  
  base_inv_names = extract_name_patents(base_inv$inv_name, "id_inv_seq")
  
  # adding the priority date
  base_inv_names = .cmerge(base_inv_names, base_inv[, .(id_inv_seq, year_prio)], 
                           by = "id_inv_seq", incl = TRUE, unik = TRUE)
  
  write_fst_forte(base_inv_names, outfile)
  
}

step0_PATENT_employer = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/PATENT_inv-employer.fst")
  
  base_inv_se = readfst("_DATA/_RAW/OECD_inventors.fst", forte = TRUE)
  appln_id_select = sort(unique(base_inv_se$appln_id))
  
  # we select the applicants
  base_app = readfst("_DATA/_RAW/OECD_applicants.fst", forte = TRUE)
  base_app_select = base_app[appln_id %in% appln_id_select, .(appln_id, app_name, app_address)]
  
  # we add the address
  base_app_add = extract_se_address_patents(base_app_select$app_address, box_valid = TRUE)
  base_app_select = cbind(base_app_select, base_app_add[, .(postcode, city, 
                                                            street_name, street_nb, building)])
  
  # We clean the applicants
  base_app_select[, app_clean := clean_employer_name_se(app_name)]
  
  # now we merge to the inventors
  base2merge = base_app_select[, .(appln_id, inv_emp = app_clean, inv_emp_postcode = postcode,
                                   inv_emp_city = city, inv_emp_street_name = street_name,
                                   inv_emp_street_nb = street_nb, inv_emp_building = building)]
  
  base_inv_app = .cmerge(base_inv_se[, .(appln_id, id_inv_seq, year_prio)],
                         base2merge, 
                         allow.cartesian = TRUE, all.x = TRUE, incl.y = TRUE)
  
  setNA(base_inv_app, "^inv_", "")
  
  write_fst_forte(base_inv_app, outfile)
  
}

####
#### STEP 1: Name matching ####
####

step1_name_matching = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/ALGO-1_matched-names.fst")
  
  base_inv_names = readfst("_DATA/PATENT_inv-names.fst", forte = TRUE)
  base_se_names = readfst("_DATA/STAT-SE_indiv-names.fst", 
                          vars = "id_se, ^first_, ^fam_, birth_year",
                          forte = TRUE)
  
  base_se_names_small = unique(base_se_names)
  rm(base_se_names) ; gc()
  
  base_match = match_by_name(base_inv_names, base_se_names_small, 
                             by = c("F" = "first_name_1", "S" = "first_name_2", "T" = "first_name_3", 
                                    "L" = "fam_name_1", "M" = "fam_name_2"),
                             initials = "S, T", fuzzy = "F, S, T, L, M", empty = "S, T, M", 
                             allow.swap = "L, M",
                             varkeep = c("id_inv_seq", "id_se"),
                             # you need to be at least 17 to patent and at most 85
                             keep.only = ~ (year_prio - birth_year) %in% 17:85, 
                             gc.all = TRUE)
  
  gc()
  
  # We compute the match quality and keep only the best match across all identical pairs 
  match_qual = recode_character(base_match$type, 
                                "2" = "",   # default
                                "1" = "#?", # fuzzy matched
                                "3" = c("#M", "|^([[:alpha:]]\\.?){3,}$")) # maiden name or 3+ names w/t fuzzy
  
  base_match_small = base_match[, .(id_inv_seq, id_se, match_type = type, match_qual)]
  
  base_match_small[, max_qual := max(match_qual), by = .(id_inv_seq, id_se)]
  base_match_small = base_match_small[match_qual == max_qual]
  base_match_small = unique(base_match_small, by = c("id_inv_seq", "id_se"))
  base_match_small[, max_qual := NULL]
  
  write_fst_forte(base_match_small, outfile)
  
}


####
#### STEP 2: Bilateral variables ####
####

step2_bilateral_vars = function(hard = FALSE){
  # hard: recreates the data sets even if they have already been created
  
  step2_BILAT_address(hard)
  
  step2_BILAT_employer(hard)
  
  step2_BILAT_age(hard)
  
  step2_BILAT_nameprob(hard)
  
  step2_BILAT_all(hard)
  
}

step2_BILAT_address = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/ALGO-2_address.fst")
  
  base_name_match = readfst("_DATA/ALGO-1_matched-names.fst", "^id_", forte = TRUE)
  
  # Inventors addresses
  base_inv_add = readfst("_DATA/PATENT_inv-address.fst", forte = TRUE)
  info_missing = readfst("_DATA/PATENT_address-missing.fst", forte = TRUE)
  base_inv_add[, is_missing := info_missing$address_missing]
  
  # SE addresses
  base_se_add = readfst("_DATA/STAT-SE_indiv-addresses.fst", forte = TRUE)
  
  # We merge the two address information
  base_left = base_inv_add[, .(inv_city = city, inv_postcode = postcode, 
                               inv_street = street_name, inv_nb = street_nb, 
                               inv_building = building, inv_missing = is_missing)]
  
  base_right = base_se_add[, .(id_se, se_city = city, se_postcode = postcode, 
                               se_street = street_name, se_nb = street_nb, 
                               se_building = building)]
  
  
  base_match_add = cbind(base_name_match, base_left[base_name_match$id_inv_seq])
  tmp = .cmerge(base_match_add.I, base_right, allow.cartesian = TRUE, incl.x = TRUE)
  base_match_add = tmp
  
  base_match_add[, same_postcode := inv_postcode == se_postcode]
  
  base_match_add[, same_city := string_proxi(inv_city, se_city, method = "inclusion", 
                                             incl.loose = TRUE, incl.misspell = TRUE, min_char = 4)]
  # to catch stockholm boroughs
  base_match_add[same_postcode == TRUE & same_city == 0 & (inv_city == "stockholm" | se_city == "stockholm"), 
                 same_city := 1]
  
  base_match_add[, same_street := string_proxi(inv_street, se_street, method = "inclusion", 
                                               incl.loose = TRUE, incl.misspell = TRUE, min_char = 4, empty = 1)]
  
  base_match_add[, same_nb := inv_nb == se_nb]
  base_match_add[, same_building := inv_building == se_building]
  
  my_cols = extract_vars(base_match_add, "^id, #postcode, #city, #street, #nb, #building")
  setcolorder(base_match_add, my_cols)
  
  # Same address
  
  base_match_add[, same_address := 
                   # we require non missing street name
                   (inv_street != "") & 
                   (
                     # same city + street + nber + building (we accept missing building info, but not != ones)
                     (same_city + same_street + same_nb == 3 & 
                        (same_building == 1 | inv_building == "" | se_building == "")) |
                       # same postcode + street + nb + build, we are loose on the city to catch formatting pblms
                       (same_postcode + same_street + same_nb + same_building == 4 & 
                          (inv_city %in% c("", "sweden") | se_city %in% c("", "sweden")) & 
                          nchar(inv_street) > 0)
                   )
  ]
  
  base_match_add[same_address == FALSE & same_city + same_nb + same_building == 3, 
                 same_address := part_string_proxi(inv_street, se_street)]
  
  # 2: match, 1: missing, 0: unmatched
  base_match_add[, address_cat := fifelse(same_address, "2", "0")]
  base_match_add[same_address == FALSE & (inv_missing == 1), address_cat := "1"]
  
  base_bilat_address = base_match_add[, .(address_cat = max(address_cat)), by = .(id_inv_seq, id_se)]
  base_bilat_address[, address_match := ref(address_cat, .("unmatched" = "0", "missing" = "1", "matched" = "2"))]
  
  write_fst_forte(base_bilat_address, outfile)
  
}


step2_BILAT_employer = function(hard = FALSE){
  # version 2023-03-03
  # - we split btw large and small namesakes:
  #   *  <50 namesakes: regular algorithm with thorough employer checking
  #   * >=50 namesakes: simplified algorithm with "simple" employer checking
  
  # end product: variable 0/1 same employer

  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/ALGO-2_employer.fst")
  
  base_name_match = readfst("_DATA/ALGO-1_matched-names.fst", "^id_", forte = TRUE)
  base_name_match[, n_namesakes := .N, by = id_inv_seq]
  base_name_match_thorough = base_name_match[n_namesakes < 50]
  base_name_match_simple = base_name_match[n_namesakes >= 50]
  
  rm(base_name_match)
  gc()
  
  inv_emp = readfst("_DATA/PATENT_inv-employer.fst", forte = TRUE)
  
  #
  # thorough case
  #
  
  cat("Case with < 50 namesakes\n")
  
  # we load and then rm that file for memory reasons (not efficient in terms of speed)
  se_emp = readfst("_DATA/STAT-SE_jobb.fst", vars = "!^year, !#building", forte = TRUE)
  
  cat("\nmerging employer information: SE....")
  base_emp_bilat = merge(base_name_match_thorough, se_emp, 
                         by = "id_se", all.x = TRUE, allow.cartesian = TRUE)
  fdim(base_emp_bilat)
  cat("\\=> size: ", osize(base_emp_bilat))
  
  rm(base_name_match_thorough, se_emp)
  gc()
  
  cat("\nmerging employer information: inv....")
  
  # base_emp_bilat = .cmerge(base_emp_bilat, inv_emp, allow.cartesian = TRUE, incl.x = TRUE)
  base_emp_bilat = merge(base_emp_bilat, inv_emp, 
                         by = "id_inv_seq",
                         allow.cartesian = TRUE)
  fdim(base_emp_bilat)
  cat("\\=> size: ", osize(base_emp_bilat))
  
  setNA(base_emp_bilat, "@se_emp", "")
  
  gc()
  
  #
  # Finding out whether this is the same employer
  #
  
  cat("\ncomputing string distances: employer name...")
  
  base_emp_bilat[, proxi_jw := string_proxi(se_emp, inv_emp)]
  base_emp_bilat[, proxi_incl := string_proxi(se_emp, inv_emp, "inclusion",
                                              incl.loose = TRUE, incl.misspell = TRUE)]
  base_emp_bilat[, proxi_incl_bis := 0]
  base_emp_bilat[proxi_incl == 0, 
                 proxi_incl_bis := string_proxi(str_op(se_emp, "'\\b(abet|ab)\\b'R, w"), 
                                                str_op(inv_emp, "'\\b(abet|ab)\\b'R, w"), 
                                                "inclusion", incl.loose = TRUE, 
                                                incl.misspell = TRUE, min_char = 5)]
  
  cat("\ncomputing string distances: addresses...")
  base_emp_bilat[, same_city := string_proxi(inv_emp_city, se_emp_city, "inclusion",
                                             incl.loose = TRUE, incl.misspell = TRUE)]
  
  base_emp_bilat[, same_street := string_proxi(inv_emp_street_name, se_emp_street_name, "inclusion",
                                               incl.loose = TRUE, incl.misspell = TRUE)]
  
  base_emp_bilat[, proxi_add := (inv_emp_postcode == se_emp_postcode & 
                                   same_city & same_street & 
                                   se_emp_street_nb == inv_emp_street_nb & 
                                   se_emp_street_nb != "")]
  
  cat("\nfinal employer proximity data set....")
  
  base_bilat_emp_thorough = base_emp_bilat[, .(id_inv_seq, id_se, 
                                      same_emp = +(proxi_incl == 1 | proxi_jw >= 0.95 | 
                                                     proxi_incl_bis == 1 |
                                                     (proxi_add == 1 & proxi_jw > 0.6)))]
  
  rm(base_emp_bilat)
  gc()
  

  #
  # simple comparisons ------------------------------------------------------- -
  #
  
  cat("\n-----------------------------------------------------", 
       "\ncase with >= 50 namesakes\n")
  
  se_emp = readfst("_DATA/STAT-SE_jobb.fst", vars = "!^year, !#building", forte = TRUE)
  
  id_se_simple = unique(base_name_match_simple$id_se)
  se_emp_simple = se_emp[id_se %in% id_se_simple & se_emp_street_name != "" & se_emp_street_nb != ""]
  
  cat("\nemployer information: SE...")
  fdim(se_emp_simple)
  cat("\\=> size: ", osize(se_emp_simple))
  
  id_inv_simple = unique(base_name_match_simple$id_inv_seq)
  inv_emp_simple = inv_emp[id_inv_seq %in% id_inv_simple & inv_emp_street_name != "" & inv_emp_street_nb != ""]
  
  cat("\nemployer information: inv...")
  fdim(inv_emp_simple)
  cat("\\=> size: ", osize(inv_emp_simple))
  
  #
  # same employer address + +/- close employer name
  #
  
  cat("\ncreating the address index....")
  
  city_street_number = to_integer(c(se_emp_simple$se_emp_city, inv_emp_simple$inv_emp_city),
                                  c(se_emp_simple$se_emp_street_name, inv_emp_simple$inv_emp_street_name),
                                  c(se_emp_simple$se_emp_street_nb, inv_emp_simple$inv_emp_street_nb))
  index_se = 1:nrow(se_emp_simple)
  se_emp_simple[, address_id := city_street_number[index_se]]
  
  index_inv = nrow(se_emp_simple) + (1:nrow(inv_emp_simple))
  inv_emp_simple[, address_id := city_street_number[index_inv]]
  
  rm(city_street_number) ; gc()
  
  # inventor emp
  
  cat("\nmerging with the address restriction...")
  
  base2merge = unique(inv_emp_simple[, .(id_inv_seq, inv_emp, address_id)])
  base_simple_add = merge(base_name_match_simple, 
                          base2merge,
                          by = "id_inv_seq", allow.cartesian = TRUE)
  
  # se emp: merge with address
  base2merge = unique(se_emp_simple[, .(id_se, se_emp, address_id)])
  base_simple_add = merge(base_simple_add, base2merge, 
                          by = c("id_se", "address_id"), allow.cartesian = TRUE)
  
  fdim(base_simple_add)
  cat("\\=> size: ", osize(base_simple_add))
  
  rm(base2merge) ; gc()
  
  base_simple_add[, proxi_jw := string_proxi(inv_emp, se_emp)]
  
  base_simple_add = base_simple_add[proxi_jw > 0.6]
  base_simple_add[, same_emp := 1]
  
  base_simple_add = unique(base_simple_add[, .(id_inv_seq, id_se, same_emp)])
  
  
  #
  # same employer name
  #
  
  cat("\ncomparing company names...")
  
  se_emp_name_simple = unique(se_emp[id_se %in% id_se_simple, .(id_se, se_emp)])
  inv_emp_name_simple = unique(inv_emp[id_inv_seq %in% id_inv_simple, .(id_inv_seq, inv_emp)])
  
  rm(se_emp, inv_emp) ; gc()
  
  se_emp_name_simple[, initial := substr(se_emp, 1, 3)]
  inv_emp_name_simple[, initial := substr(inv_emp, 1, 3)]
  
  cat("\nmerging with initial restriction...")
  
  base_simple_name = merge(base_name_match_simple, 
                           inv_emp_name_simple,
                           by = "id_inv_seq", allow.cartesian = TRUE)
  
  base_simple_name = merge(base_simple_name, 
                           se_emp_name_simple,
                           by = c("id_se", "initial"), allow.cartesian = TRUE)
  
  fdim(base_simple_name)
  cat("\\=> size: ", osize(base_simple_name))
  
  rm(base_name_match_simple, inv_emp_name_simple, se_emp_name_simple) ; gc()
  
  base_simple_name[, proxi_jw := string_proxi(se_emp, inv_emp)]
  base_simple_name[, proxi_incl := string_proxi(se_emp, inv_emp, "inclusion",
                                                incl.loose = TRUE, incl.misspell = TRUE)]
  base_simple_name[, proxi_incl_bis := 0]
  base_simple_name[proxi_incl == 0, 
                   proxi_incl_bis := string_proxi(str_op(se_emp, "'\\b(abet|ab)\\b'R, w"), 
                                                  str_op(inv_emp, "'\\b(abet|ab)\\b'R, w"), 
                                                  "inclusion", incl.loose = TRUE, 
                                                  incl.misspell = TRUE, min_char = 5)]
  
  base_simple_name[, same_emp := +(proxi_incl == 1 | proxi_jw >= 0.95 | proxi_incl_bis == 1)]
  
  base_simple_name = unique(base_simple_name[, .(id_inv_seq, id_se, same_emp)])
  
  #
  # merging for "simple"
  #
  
  base_bilat_emp_simple = rbindlist(list(base_simple_add, base_simple_name))
  
  rm(base_simple_add, base_simple_name) ; gc()
  
  
  #
  # final merge
  #
  
  cat("\ngathering the information from the two cases")
  
  base_name_match = readfst("_DATA/ALGO-1_matched-names.fst", "^id_")
  
  base_bilat_full = rbindlist(list(base_bilat_emp_thorough, base_bilat_emp_simple))
  
  rm(base_bilat_emp_thorough, base_bilat_emp_simple) ; gc()
  
  base_algo2_emp = merge(base_name_match, base_bilat_full, 
                         by = c("id_se", "id_inv_seq"), all.x = TRUE)
  
  setNA(base_algo2_emp, "same_emp", 0)
  
  cat("\nuniquifying....")
  
  base_algo2_emp[, max_emp := max(same_emp), by = .(id_inv_seq, id_se)]
  base_algo2_emp = base_algo2_emp[same_emp == max_emp]
  base_algo2_emp[, max_emp := NULL]
  base_algo2_emp = unique(base_algo2_emp, by = c("id_inv_seq", "id_se"))
  
  write_fst_forte(base_algo2_emp, outfile)
  
}


step2_BILAT_age = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/ALGO-2_age.fst")
  
  base_name_match = readfst("_DATA/ALGO-1_matched-names.fst", 
                            vars = "id_inv_seq, id_se", forte = TRUE)
  base_name_match = unique(base_name_match)
  
  # inventor year
  
  base_inv_year = readfst("_DATA/_RAW/OECD_inventors.fst", 
                          vars = "id_inv_seq, appln_id, ^year", forte = TRUE)
  
  # birth year
  base_se_birth = readfst("_DATA/_RAW/STAT-SE_indiv.fst", 
                          vars = "id_se, birth_year", forte = TRUE)
  base_se_birth = unique(base_se_birth)
  
  # base_bilat_age
  base_bilat_age = .cmerge(base_name_match, base_se_birth, incl.x = TRUE)
  
  rm(base_name_match, base_se_birth)
  gc()
  
  tmp = .cmerge(base_bilat_age, base_inv_year, incl.x = TRUE)
  base_bilat_age = tmp
  
  rm(base_inv_year)
  gc()
  
  base_bilat_age[, age := year_prio - birth_year]
  
  base_bilat_age = base_bilat_age[, .(id_inv_seq, id_se, age)]
  
  write_fst_forte(base_bilat_age, outfile)

}


step2_BILAT_nameprob = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/ALGO-2_name_proba.fst")
  
  # We use the SE names as a dictionary
  
  base_se_names = readfst("_DATA/_RAW/STAT-SE_indiv.fst", 
                          vars = "id_se, ^firstn, ^family, !#code", 
                          forte = TRUE)
  
  base_se_names = unique(base_se_names, by = "id_se")
  
  gc()
  
  first_names_all = strsplit2df(base_se_names$firstname_all, split = " ")
  fam_names_all = strsplit2df(base_se_names$family_name, split = " ")
  
  first_names_all[, x := cleanCharVector(x, forceASCII = TRUE)]
  fam_names_all[, x := cleanCharVector(x, forceASCII = TRUE)]
  
  first_names_all = first_names_all[, .(freq = .N), by = x]
  fam_names_all = fam_names_all[, .(freq = .N), by = x]
  
  
  first_names_all[, proba := freq / sum(first_names_all$freq)]
  fam_names_all[, proba := freq / sum(fam_names_all$freq)]
  
  
  min_prob = min(first_names_all$proba, fam_names_all$proba)
  
  base_se_names[, x_first := str_clean(firstname_all, " .+")]
  base_se_names[, x_fam := str_clean(family_name, " .+")]
  
  tmp = .cmerge(base_se_names, first_names_all[, .(x_first = x, proba_first = proba)], 
                all.x = TRUE)
  
  rm(base_se_names)
  gc()
  
  base_se_names = .cmerge(tmp, 
                          fam_names_all[, .(x_fam = x, proba_fam = proba)], 
                          all.x = TRUE)
  
  rm(tmp)
  gc()
  
  setNA(base_se_names, "^proba", min_prob)
  
  base_se_names[, proba_name_se := proba_first * proba_fam]
  
  base_prob_se = base_se_names[, .(id_se, proba_name_se)]
  
  rm(base_se_names)
  gc()
  
  
  # Inventors
  base_inv_names = readfst("_DATA/PATENT_inv-names.fst", 
                           vars = "id_inv_seq, @f.+_name_1", forte = TRUE)
  setnames(base_inv_names, dsb("/first_name_1, fam_name_1"), 
           dsb("/x_first, x_fam"))
  
  tmp = .cmerge(base_inv_names, 
                first_names_all[, .(x_first = x, proba_first = proba)], 
                all.x = TRUE)
  base_inv_names = .cmerge(tmp, 
                           fam_names_all[, .(x_fam = x, proba_fam = proba)], 
                           all.x = TRUE)
  
  setNA(base_inv_names, "^proba", min_prob)
  
  base_inv_names[, proba_name_inv := proba_first * proba_fam]
  
  base_prob_inv_seq = base_inv_names[, .(id_inv_seq, proba_name_inv)]
  
  # bilat
  base_name_match = readfst("_DATA/ALGO-1_matched-names.fst", 
                            "!^match", forte = TRUE)
  
  base_bilat_name_proba = .cmerge(base_name_match, base_prob_se, 
                                  incl.x = TRUE, unik.y = TRUE)
  
  rm(base_name_match, base_prob_se)
  gc()
  
  base_bilat_name_proba = .cmerge(base_bilat_name_proba, base_prob_inv_seq, 
                                  incl.x = TRUE, unik.y = TRUE)
  
  base_bilat_name_proba = unique(base_bilat_name_proba)
  
  base_bilat_name_proba[, proba_name := pmax(proba_name_se,  proba_name_inv)]
  base_bilat_name_proba[, proba_name := proba_name / max(proba_name)]
  
  base_bilat_name_proba = base_bilat_name_proba[, .(proba_name = max(proba_name)), 
                                                by = .(id_inv_seq, id_se)]
  
  my_rank = function(x){
    # I turn stg into 0 and 1
    x_int = to_integer(x, sorted = TRUE)
    res = x_int / max(x_int)
    res
  }
  
  base_bilat_name_proba[, proba_name_rank := my_rank(proba_name)]
  
  write_fst_forte(base_bilat_name_proba, outfile)
  
}


step2_BILAT_all = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/ALGO-2_all.fst")
  
  base_name_match = readfst("_DATA/ALGO-1_matched-names.fst", "!match_type", forte = TRUE)
  bilat_address = readfst("_DATA/ALGO-2_address.fst", forte = TRUE)
  bilat_age = readfst("_DATA/ALGO-2_age.fst", forte = TRUE)
  bilat_emp = readfst("_DATA/ALGO-2_employer.fst", forte = TRUE)
  bilat_name_prob = readfst("_DATA/ALGO-2_name_proba.fst", forte = TRUE)
  
  # match qual + address
  bilat_all = .cmerge(base_name_match, bilat_address, incl = TRUE, unik = TRUE)
  
  # age
  bilat_all = .cmerge(bilat_all, bilat_age, incl = TRUE, unik = TRUE)
  
  # emp
  bilat_all = .cmerge(bilat_all, bilat_emp, incl = TRUE, unik = TRUE)
  
  # name proba
  bilat_all = .cmerge(bilat_all, bilat_name_prob, incl = TRUE, unik = TRUE)
  
  # We add the nber of matches per inventor
  bilat_all[, n_match := .N, by = id_inv_seq]
  
  write_fst_forte(bilat_all, outfile)
  
}


####
#### STEP 3: EM algorithm ####
####

step3_EM_algorithm = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  #
  # EM step
  #
  
  outfile_em = start_outfile("_DATA/ALGO-3_em-result-n_match-LE-5.RData")
  
  base_bilat = readfst("_DATA/ALGO-2_all.fst", forte = TRUE)
  
  base_bilat_small = base_bilat[n_match <= 5]
  base_bilat_small[, name_prob_bin := bin(proba_name_rank, c("cut::3", "low", "mid", "high"))]
  base_bilat_small[, match_qual_fact := ref(match_qual, .("low" = 1, "mid" = 2, "high" = 3))]
  base_bilat_small[, prob_qual := ref(paste0("p:", name_prob_bin, "_q:", match_qual_fact))]
  
  init = 1 * (base_bilat_small$address_match == "matched")
  
  res_em = em_matching(base_bilat_small, c("address_match", "age", "same_emp", "prob_qual"), 
                       init = init, M = 2, em.iter = 500)
  
  save(res_em, file = outfile_em$path)
  message("=> `", outfile_em$path, "` built in ", format_difftime(outfile_em$time), ".")
  
  #
  # base match
  #
  
  outfile_match = start_outfile("_DATA/ALGO-3_base-all-matches.fst")
  
  base_bilat_small[, prob := res_em$tau[, 2]]
  base_bilat_small[, max_prob := max(prob), by = id_inv_seq]
  
  #
  # We save the information on the matches ud_inv_seq x id_se
  #
  
  base_match_small = base_bilat_small[prob == max_prob & prob > 0.8, 
                                      .(id_inv_seq, id_se, type_match = "EM", prob)]
  base_match_small = unique(base_match_small)
  base_match_small[, n := .N, keyby = id_inv_seq]
  
  #
  # many
  base_bilat_many = base_bilat[n_match > 5]
  base_bilat_many[, same_address := 10 * (address_match == "matched")]
  base_bilat_many[, val := same_address + same_emp]
  base_bilat_many[, max_val := max(val), by = id_inv_seq]
  base_match_many = base_bilat_many[val == max_val & val > 0, 
                                    .(id_inv_seq, id_se, val, type_match = "address")]
  base_match_many = unique(base_match_many)
  base_match_many[, n := .N, keyby = id_inv_seq]
  
  #
  # all matches, unambiguous
  base_matches = rbind(base_match_small[n == 1, .(id_inv_seq, id_se, type_match, prob)],
                       base_match_many[n == 1, .(id_inv_seq, id_se, type_match, prob = NA)])
  
  
  write_fst_forte(base_matches, outfile_match)
  
}



####
#### STEP 4: Patent information ####
####

step4_patent_match = function(hard = FALSE){
  # hard: recreates the data sets even if they have already been created
  
  step4_EXTRA_bilat(hard)
  
  step4_EXTRA_EM(hard)
  
  step4_EXTRA_match(hard)
  
}

step4_EXTRA_bilat = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/ALGO-4_bilateral_patents.fst")
  
  folder = getOption("LOCATION_OECD")
  if(is.null(folder)) folder = "_DATA/_RAW/OECD"
  
  base_inv = readfst("_DATA/_RAW/OECD_inventors.fst", forte = TRUE)
  # we need to use the raw patent data containing the information on ALL inventors, not only the Swedish ones!
  base_patent = fread(dsb(".[folder]/202202_EPO_Inv_reg_small.txt"), encoding = "UTF-8")
  base_matches = readfst("_DATA/ALGO-3_base-all-matches.fst", forte = TRUE)
  base_bilat = readfst("_DATA/ALGO-2_all.fst", forte = TRUE)
  
  # we unique
  base_patent = unique(base_patent, by = c("appln_id", "inv_name"))
  
  # base of all id_se that were matched, and all potential patent matches
  base_bilat_small = base_bilat[id_se %in% base_matches$id_se]
  
  # all the patents: either the ones matched, or the ones potentially matched
  appln_id_keep = base_inv[id_inv_seq %in% base_matches$id_inv_seq | id_inv_seq %in% base_bilat_small$id_inv_seq, appln_id]
  # 41,844
  
  # MATCHED
  # base with id_se x id_inv_seq x appln_id for the ones matched
  base_match_se_appln_id = .cmerge(base_matches[, .(id_se, id_inv_seq)], base_inv[, .(id_inv_seq, appln_id)], 
                                  by = "id_inv_seq",
                                  unik = TRUE, incl.x = TRUE)
  
  # UNMATCHED
  # base with id_se x id_inv_seq x appln_id for the ones **not** matched
  base_unmatch_se_inv = base_bilat_small[!id_inv_seq %in% base_matches$id_inv_seq, .(id_se, id_inv_seq)]
  quoi = .cmerge(base_unmatch_se_inv, base_inv[, .(id_inv_seq, appln_id)], 
                by = "id_inv_seq",
                unik.y = TRUE, incl.x = TRUE)
  base_unmatch_se_inv = quoi
  
  ####
  #### ... co-inventors ####
  ####
  
  message("coinventors ... ", appendLF = FALSE)
  
  # objective:
  # create a 0/1 indicator of whether the appln_id share at least one coauthor with the matched id_se
  
  # we first create the base of all inventor names (not only swedish)
  
  # base of co inventors
  base_coinventors = unique(base_patent[appln_id %in% appln_id_keep])
  
  # => add id_inv_seq
  tmp = .cmerge(base_coinventors, base_inv[, .(appln_id, inv_name, id_inv_seq)], 
               by = c("appln_id", "inv_name"), all.x = TRUE, unik = TRUE)
  base_coinventors = tmp
  setNA(base_coinventors, "id_inv_seq", -1)
  
  base_coinventors_names = extract_name_patents(base_coinventors$inv_name, silent = TRUE)
  base_coinventors_names[, appln_id := base_coinventors$appln_id]
  base_coinventors_names[, id_inv_seq := base_coinventors$id_inv_seq]
  
  base_coinventors_names[, name_id := to_integer(first_name_1, fam_name_1)]
  
  base_coinv = base_coinventors_names[, .(appln_id, id_inv_seq, name_id_co = name_id)]
  
  # MATCHED
  # Coauthors of the id_se that were matched to patents
  quoi = .cmerge(base_match_se_appln_id, base_coinv[, .(appln_id, id_inv_seq_co = id_inv_seq, name_id_co)],
                allow.cartesian = TRUE, incl.x = TRUE)
  # base of all coauthors names per id_se
  base_match_coauth = unique(quoi[id_inv_seq != id_inv_seq_co, .(id_se, name_id_co)])
  
  # UNMATCHED
  
  # we merge the coauthor information
  quoi = .cmerge(base_unmatch_se_inv, base_coinv[, .(appln_id, id_inv_seq_co = id_inv_seq, name_id_co)],
                allow.cartesian = TRUE, incl.x = TRUE)
  # base of all coauthors names per id_se x id_inv_seq
  base_unmatch_coauth = unique(quoi[id_inv_seq != id_inv_seq_co, .(id_se, id_inv_seq, name_id_co)])
  
  # BILATERAL variable: same_coauth
  base_same_coauth = merge(base_match_coauth, base_unmatch_coauth, by = c("id_se", "name_id_co"))
  base_same_coauth = base_same_coauth[, .(n_same_coauth = .N), by = .(id_se, id_inv_seq)]
  base_same_coauth[, same_coauth := 1]
  
  message("done")
  
  
  ####
  #### ... technologies ####
  ####
  
  message("technologies ... ", appendLF = FALSE)
  
  folder = getOption("LOCATION_OECD")
  if(is.null(folder)) folder = "_DATA/_RAW/OECD"
  
  base_ipc = fread(dsb(".[folder]/202202_EPO_IPC_small.txt"))
  setnames(base_ipc, "IPC", "ipc")
  
  base_ipc[, ipc := gsub("/.*", "", ipc, perl = TRUE)]
  
  # MATCHED
  quoi = .cmerge(base_match_se_appln_id, base_ipc, 
                by = "appln_id", allow.cartesian = TRUE,
                incl.x = TRUE)
  base_match_ipc = unique(quoi[, .(id_se, ipc_se = ipc)])
  base_match_ipc[, ipc1 := substr(ipc_se, 1, 1)]
  
  # UNMATCHED
  quoi = .cmerge(base_unmatch_se_inv, base_ipc, 
                by = "appln_id", allow.cartesian = TRUE,
                incl.x = TRUE)
  base_unmatch_ipc = unique(quoi[, .(id_se, id_inv_seq, ipc_inv = ipc)]) 
  base_unmatch_ipc[, ipc1 := substr(ipc_inv, 1, 1)]
  
  # BILATERAL technology variable
  base_same_tech = .cmerge(base_match_ipc, base_unmatch_ipc,
                          by = c("id_se", "ipc1"), allow.cartesian = TRUE)
  
  base_same_tech[, max_tech_sim := tech_sim(ipc_se, ipc_inv)]
  base_same_tech = base_same_tech[, .(max_tech_sim = max(max_tech_sim)), 
                                  by = .(id_se, id_inv_seq)]
  
  message("done")
  
  ####
  #### ... applicants ####
  ####
  
  message("applicants ... ", appendLF = FALSE)
  
  base_inv_emp = readfst("_DATA/PATENT_inv-employer.fst", forte = TRUE)
  
  # MATCHED
  base_match_emp = .cmerge(base_match_se_appln_id[, .(id_se, id_inv_seq)], 
                          base_inv_emp[, .(id_inv_seq, app_name_se = inv_emp, 
                                           app_city_se = inv_emp_city, 
                                           app_street_se = inv_emp_street_name,
                                           app_nb_se = inv_emp_street_nb)], 
                          by = "id_inv_seq", incl.x = TRUE, unik.x = TRUE)
  
  base_match_emp = unique(base_match_emp[, !"id_inv_seq"])
  
  
  # UNMATCHED
  base_unmatch_emp = .cmerge(base_unmatch_se_inv[, .(id_se, id_inv_seq)], 
                            base_inv_emp[, .(id_inv_seq, app_name_inv = inv_emp, 
                                             app_city_inv = inv_emp_city, 
                                             app_street_inv = inv_emp_street_name,
                                             app_nb_inv = inv_emp_street_nb)], 
                            by = "id_inv_seq", incl.x = TRUE)
  
  base_unmatch_emp = unique(base_unmatch_emp)
  
  
  # BILATERAL variable: same applicant
  
  base_bilat_app = .cmerge(base_match_emp, base_unmatch_emp, 
                          by = "id_se", allow.cartesian = TRUE)
  
  base_bilat_app = setcolorder(base_bilat_app, 
                               extract_vars(base_bilat_app, "^id, ^app_name, ^app_city, ^app_stree, ^app_nb"))
  
  # Computing whether the companies are the same:
  # - same (or close to same) name
  # - exact same address
  
  base_bilat_app[, proxi_jw := string_proxi(app_name_se, app_name_inv)]
  base_bilat_app[, proxi_incl := string_proxi(app_name_se, app_name_inv, "inclusion",
                                              incl.loose = TRUE, incl.misspell = TRUE)]
  
  base_bilat_app[, same_city := string_proxi(app_city_inv, app_city_se, "inclusion",
                                             incl.loose = TRUE, incl.misspell = TRUE)]
  
  base_bilat_app[, same_street := string_proxi(app_street_inv, app_street_se, "inclusion",
                                               incl.loose = TRUE, incl.misspell = TRUE, min_char = 4)]
  
  base_bilat_app[, proxi_add := (same_city & same_street & app_nb_inv == app_nb_se & 
                                   !grepl("box", app_street_se, fixed = TRUE) & 
                                   app_nb_inv != "")]
  
  base_same_app = base_bilat_app[, .(id_inv_seq, id_se, same_app = +(proxi_incl == 1 | proxi_jw >= 0.95 | proxi_add == 1))]
  base_same_app = base_same_app[, .(same_app = max(same_app)), by = .(id_se, id_inv_seq)]
  
  message("done")
  
  ####
  #### ... bilateral variables ####
  ####
  
  message("aggregation ... ", appendLF = FALSE)
  
  base_potential_bilat = base_bilat_small[!id_inv_seq %in% base_matches$id_inv_seq]
  
  # co author
  quoi = .cmerge(base_potential_bilat, base_same_coauth[, "^id, ^same"],
                by = "id_se, id_inv_seq", all.x = TRUE,
                unik = TRUE, incl.y = TRUE)
  setNA(quoi, "same_coauth", 0)
  base_potential_bilat = quoi
  
  # technology
  quoi = .cmerge(base_potential_bilat, base_same_tech,
                by = "id_se, id_inv_seq", all.x = TRUE,
                unik = TRUE, incl.y = TRUE)
  setNA(quoi, "max_tech_sim", 0)
  base_potential_bilat = quoi
  
  # applicant
  quoi = .cmerge(base_potential_bilat, base_same_app,
                by = "id_se, id_inv_seq", all.x = TRUE,
                unik = TRUE, incl.y = TRUE)
  base_potential_bilat = quoi
  
  message("done")
  
  
  write_fst_forte(base_potential_bilat, outfile)
  
}

step4_EXTRA_EM = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/ALGO-4_em-result-patent_match.RData")
  
  base_potential_bilat = readfst("_DATA/ALGO-4_bilateral_patents.fst", forte = TRUE)
  
  base_potential_bilat[, name_prob_bin := bin(proba_name_rank, c("cut::3", "low", "mid", "high"))]
  base_potential_bilat[, match_qual_fact := ref(match_qual, .("low" = 1, "mid" = 2, "high" = 3))]
  base_potential_bilat[, prob_qual := ref(paste0("p:", name_prob_bin, "_q:", match_qual_fact))]
  
  init = base_potential_bilat$same_coauth == 1
  
  res_em_pat = em_matching(base_potential_bilat, 
                           c("address_match", "age", "same_emp", 
                             "prob_qual", "same_coauth", "max_tech_sim", "same_app"), 
                           init = init, M = 2, em.iter = 500)
  
  save(res_em_pat, file = outfile$path)
  message("=> `", outfile$path, "` built in ", format_difftime(outfile$time), ".")
}


step4_EXTRA_match = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/ALGO-4_base_match_patent.fst")
  
  # this is the final data set 
  base_inv = readfst("_DATA/_RAW/OECD_inventors.fst", forte = TRUE)
  base_matches = readfst("_DATA/ALGO-3_base-all-matches.fst", forte = TRUE)
  base_potential_bilat = readfst("_DATA/ALGO-4_bilateral_patents.fst", forte = TRUE)
  load("_DATA/ALGO-4_em-result-patent_match.RData")
  
  base_potential_bilat[, prob := res_em_pat$tau[, 2]]
  base_potential_bilat[is.na(prob), prob := 0]
  base_potential_bilat[, max_prob := max(prob), by = id_inv_seq]
  
  base_potential_bilat = base_potential_bilat[order(id_inv_seq, -prob)]
  
  
  base_match_patent = base_potential_bilat[prob == max_prob & prob > 0.50, 
                                           .(id_se, id_inv_seq, type_match = "EM-patent", prob)]
  
  
  base_match_patent = rbindlist(list(base_match_patent, base_matches), use.names = TRUE)
  
  # merging the appln_id
  base_match_patent = .cmerge(base_match_patent, base_inv[, .(appln_id, id_inv_seq)], 
                             by = "id_inv_seq", incl.x = TRUE, unik.y = TRUE)
  
  base_match_patent = unique(base_match_patent, by = "id_inv_seq")
  
  write_fst_forte(base_match_patent, outfile)
  
}

####
#### STEP 5: Descriptive data sets ####
####


step5_descriptive = function(hard = FALSE){
  
  skip = should_fun_be_skipped(hard)
  if(skip) return(invisible(NULL))
  
  outfile = start_outfile("_DATA/ALGO-5_EM-sample-info.fst")
  
  #
  # Main EM
  #
  
  # the EM is applied only to id_inv_seq matched to 5 or less id_se
  
  base_bilat = readfst("_DATA/ALGO-2_all.fst", forte = TRUE)
  
  # variable on name probability (based on the distribution in the small data set)
  base_bilat_small = base_bilat[n_match <= 5]
  base_bilat_small[, name_prob_bin := bin(proba_name_rank, c("cut::3", "low", "mid", "high"))]
  base_bilat_small[, match_qual_fact := ref(match_qual, .("low" = 1, "mid" = 2, "high" = 3))]
  base_bilat_small[, prob_qual := ref(paste0("p:", name_prob_bin, "_q:", match_qual_fact))]
  
  load("_DATA/ALGO-3_em-result-n_match-LE-5.RData")
  
  base_bilat_small[, prob := res_em$tau[, 2]]
  base_bilat_small[, max_prob := max(prob), by = id_inv_seq]
  base_bilat_small[, type_match := "EM"]
  
  #
  # EM-patent
  #
  
  base_bilat_patent = readfst("_DATA/ALGO-4_bilateral_patents.fst", forte = TRUE)
  
  base_bilat_patent[, name_prob_bin := bin(proba_name_rank, c("cut::3", "low", "mid", "high"))]
  base_bilat_patent[, match_qual_fact := ref(match_qual, .("low" = 1, "mid" = 2, "high" = 3))]
  base_bilat_patent[, prob_qual := ref(paste0("p:", name_prob_bin, "_q:", match_qual_fact))]
  
  load("_DATA/ALGO-4_em-result-patent_match.RData")
  
  base_bilat_patent[, prob := res_em_pat$tau[, 2]]
  base_bilat_patent[, max_prob := max(prob), by = id_inv_seq]
  base_bilat_patent[, type_match := "EM-patent"]
  
  
  #
  # Combining EM to EM-patent
  #
  
  base_bilat_all = rbindlist(list(base_bilat_small, base_bilat_patent), 
                             use.names = TRUE, fill = TRUE)
  
  vars = extract_vars(base_bilat_all, "@.||!@(max_)?prob$, !type_match")
  setcolorder(base_bilat_all, vars)
  
  base_bilat_all[, c("name_prob_bin", "match_qual_fact") := NULL]
  
  # We merge information on:
  # - name
  # - employer
  # - address
  
  id_inv_seq_keep = sort(unique(base_bilat_all$id_inv_seq))
  id_se_keep = sort(unique(base_bilat_all$id_se))
  
  #
  # inventor 
  
  base_inv_names = readfst("_DATA/PATENT_inv-names.fst", forte = TRUE)
  base_inv_add = readfst("_DATA/PATENT_inv-address.fst", forte = TRUE)
  inv_emp = readfst("_DATA/PATENT_inv-employer.fst", forte = TRUE)
  
  base_inv_info = base_inv_names[, .(id_inv_seq, inv_name = name_raw, year_prio)]
  base_inv_info[, inv_address := base_inv_add$raw]
  
  # we aggregate the applicant information
  base_inv_emp = inv_emp[id_inv_seq %in% id_inv_seq_keep, .(inv_emp = paste(inv_emp, collapse = "; ")), keyby = id_inv_seq]
  
  # we merge the pieces of information
  base_inv_info = .cmerge(base_inv_info, base_inv_emp, 
                         by = "id_inv_seq", unik = TRUE, incl.y = TRUE)
  
  
  # 
  # se
  
  base_indiv = readfst("_DATA/_RAW/STAT-SE_indiv.fst", 
                       "id_se, ^firstname_all, @name$, ^street, postcode, city", forte = TRUE)
  se_emp = readfst("_DATA/STAT-SE_jobb.fst", "id_se, se_emp", forte = TRUE)
  
  base_indiv_small = base_indiv[id_se %in% id_se_keep]
  
  base_indiv_small[, se_address := paste0(street_location, ", ", postcode, ", ", city)]
  base_indiv_small[, se_name := paste0(firstname_all, ", ", family_name, " ", maiden_name)]
  base_indiv_small[, se_name := str_op(se_name, "w")]
  
  # name
  quoi = unique(base_indiv_small[, .(id_se, se_name)])
  se_name = quoi[, .(se_name = paste(se_name, collapse = "; ")), by = id_se]
  
  # address
  quoi = unique(base_indiv_small[, .(id_se, se_address)])
  se_add = quoi[, .(se_address = paste(se_address, collapse = "; ")), by = id_se]
  
  base_se_info = .cmerge(se_name.IU, se_add.IU)
  
  # employment
  base_se_emp = unique(se_emp[id_se %in% id_se_keep, .(id_se, se_emp)])
  base_se_emp_unik = base_se_emp[, .(se_emp = paste(se_emp, collapse = "; ")), by = id_se]
  
  # merge all
  tmp = .cmerge(base_se_info, base_se_emp_unik, all.x = TRUE, unik.y = TRUE, incl.y = TRUE)
  base_se_info = tmp
  setNA(base_se_info, "se_emp", "")
  
  # 
  # merge to bilateral
  
  base_bilat_all_info = .cmerge(base_bilat_all, base_inv_info, 
                               incl.x = TRUE, unik.y = TRUE)
  base_bilat_all_info = .cmerge(base_bilat_all_info, base_se_info,
                               incl.x = TRUE, unik.y = TRUE)
  
  base_bilat_all_info = base_bilat_all_info[order(id_inv_seq, -prob)]
  
  write_fst_forte(base_bilat_all_info, outfile)
  
}







