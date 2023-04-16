#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Jan 09 22:26:11 2023
# ~: various utility functions
#----------------------------------------------#

# Required packages
library(fst)
library(fixest)
library(data.table)
library(dreamerr)

####
#### Main algorithm ####
####

# INFO:
# - critical function that makes the matching by names of the inventors 
# to the Swedish persons

#' Match data sets using name information
#'
#' @param x A data.frame.
#' @param y A data.frame.
#' @param id A character vector containing variables common in both x and y used for the matching. Names in the vector can be used for aliasing.
#' @param initials A character vector containing the variables allowed to be initials. Names in the vector can be used for aliasing.
#' @param fuzzy A character vector containing the variables allowed to be fuzzy matched. Names in the vector can be used for aliasing.
#' @param empty A character vector containing the variables allowed to be empty. By default, none is allowed.
#' @param varkeep A character vector containing variables to be kept after matching. Can be missing. If a variable is common to both data sets (x and y), then you must also provide the argument \code{prefix} or \code{suffix}.
#' @param prefix A character vector of length 2. Used only when \code{varkeep} contains variables common to the two data sets, this prefix will be added to the names.
#' @param suffix A character vector of length 2. Used only when \code{varkeep} contains variables common to the two data sets, this suffix will be added to the names.
#' @param allow.swap Character vector of length 2, must correspond to variables in the argument \code{by} (or their alias). This is optional. If provided, two variables are allowed to be swapped, meaning that one can be taken for the other. If one of the values are empty, the match is performed only on the one non empty.
#' @param fuzzy.firstCommon Integer, default is 2. Number of first characters in common for the fuzzy match to be performed. Ex: if \code{fuzzy.firstCommon = 2}, then only the values that have the first two characters in common will be fuzzy matched.
#' @param fuzzy.method Character scalar, default to \code{unitary}. The type of string distance to be used. Can be \code{unitary} or \code{jaroWinkler}.
#' @param fuzzy.threshold Numeric scalar, default to 0.85. Minimal acceptable distance between two characters strings, lower values are discarded from the final data set.
#' @param fuzzy.minChar Integer, default is 4. Minimum number of characters for a character string to be allowed to be fuzzy matched.
#' @param fuzzy.maxVars Integer, default is 1. Maximum number of variables allowed to be fuzzy matched at once.
#' @param type.collate Character scalar, default to \code{""}.
#' @param keep.only Optional. A one-sided formula containing a condition in the form of an expression involving variables from the two data sets. Only pairs that validate this condition are kept in the process, the others are left out. Typical use: `~year >= year_min & year <= year_max` with `year` a variable from the first data set and `year_min` and `year_max` two variables from the second data set. The expression must contain variables from the two data sets (otherwise the selection could have been done beforehand).
#' @param gc.all Default is \code{FALSE}. Should \code{gc} be performed after each merge?
#'
#' @return
#' It returns a data.table containing the observations from the first data set matched to the observations from the second data set.
#' Optionally, with the argument \code{varkeep}, you can add variables from the original data sets.
#'
#' @examples
#' x_raw = c("julien leconte dujardin", 
#'           "francis lalanne dela_borderie",
#'           "justine dujardin")
#' 
#' y_raw = c("julien dujardin", 
#'           "francis dela_borderie lalanne",
#'           "justine rochambord dujardin",
#'           "justine aubert dujardin")
#' 
#' x = name_str_to_name_DB(x_raw, "name")
#' x[, raw := x_raw]
#' y = name_str_to_name_DB(y_raw, "name")
#' y[, raw := y_raw]
#' 
#' ex = match_by_name(x, y, varkeep = "raw", prefix = c("x_", "y_"),
#'                    by = c("F" = "name1", "L" = "name2", "M" = "name3"),
#'                    empty = "M", allow.swap = c("L, M"))
#' ex
#'
match_by_name = function(x, y, by, initials = NULL, fuzzy = NULL, empty = NULL, 
                         varkeep = NULL, prefix, suffix, allow.swap = NULL,
                         fuzzy.firstCommon = 2, fuzzy.method = "unitary", 
                         fuzzy.threshold = 0.85, fuzzy.minChar = 4, keep.only = NULL,
                         fuzzy.maxVars = 1, type.collate = "", gc.all = FALSE){
  
  
  # Example of call:
  # base_match = match_by_name(base_pub_names, base_phd_names,
  #                            by = c("F" = "first_name", "S" = "second_name", 
  #                                   "T" = "third_name", "L" = "last_name_collate"), 
  #                            initials = c("f" = "F", "s" = "S", "t" = "T"), 
  #                            fuzzy = c("F", "S", "T", "L"), empty = c("S", "T"), 
  #                            varkeep = c("author_id", "phd_id", "full_name"), 
  #                            prefix = c("pub_", "phd_"), fuzzy.firstCommon = 3, gc.all = TRUE)
  
  # Setting up the data
  check_arg(x, y, "data.frame mbt")
  check_arg(by, "character vector mbt")
  check_arg(initials, empty, fuzzy, varkeep, allow.swap, "NULL character vector")
  check_arg(prefix, suffix, "character vector len(2)")
  check_arg(fuzzy.firstCommon, fuzzy.minChar, fuzzy.maxVars, "integer scalar GE{1}")
  check_set_arg(fuzzy.method, "match(unitary, jaroWinkler)")
  check_arg(fuzzy.threshold, "numeric scalar GE{0} LE{1}")
  check_arg(type.collate, "character scalar")
  check_arg(keep.only, "NULL os formula")
  
  time_start = Sys.time()
  
  # We check that at least one variable is OK
  pblm = setdiff(by, names(x))
  if(length(pblm) > 0){
    stop("Argument 'by' must consist of variable names common to both data sets. Currently the variable", enumerate_items(pblm, "s.quote.is"), " not in x.")
  }
  pblm = setdiff(by, names(y))
  if(length(pblm) > 0){
    stop("Argument 'by' must consist of variable names common to both data sets. Currently the variable", enumerate_items(pblm, "s.quote.is"), " not in y.")
  }
  
  # varkeep
  var_common = c()
  if(!missing(varkeep)){
    pblm = !varkeep %in% names(x) & !varkeep %in% names(y)
    if(any(pblm)){
      pblm = varkeep[pblm]
      stop("Argument 'varkeep' must be a character vector referring to variables in either x or y. Currenlty the variable", enumerate_items(pblm, "s.quote.is"), " not in any of the data sets.")
    }
    
    var_common = varkeep[varkeep %in% names(x) & varkeep %in% names(y)]
    if(length(var_common) > 0){
      if(missing(prefix)){
        prefix = rep("", 2)
        if(missing(suffix)){
          stop("Since 'varkeep' contains variables present in both data sets (", enumerate_items(var_common), "), you must provide the argument 'prefix' or 'suffix' in order to distinguish them in the matched data set.")
        } else if(length(unique(suffix)) == 1){
          stop("Argument 'suffix' must consist of two distinct character values. Currently, they're both equal to '", unique(suffix), "'.")
        }
        
      } else {
        if(missing(suffix)) suffix = rep("", 2)
        if(length(unique(prefix)) == 1){
          stop("Argument 'prefix' must consist of two distinct character values. Currently, they're both equal to '", unique(prefix), "'.")
        }
      }
    }
  } else {
    varkeep = c()
  }
  
  # keep only corresponds to blocking
  do_block = !is.null(keep.only)
  blockvars = c()
  if(do_block){
    blockvars = all.vars(keep.only)
    
    vx = intersect(blockvars, names(x))
    vy = intersect(blockvars, names(y))
    
    if(length(vx) == 0){
      stop("The argument `keep.only` must involve at least one variable from the first data set (`x`).")
    }
    
    if(length(vy) == 0){
      stop("The argument `keep.only` must involve at least one variable from the second data set (`y`).")
    }
    
    pblm = intersect(vx, vy)
    if(length(pblm) > 0){
      stop("The argument `keep.only` must **not** involve any variable belonging to the two data sets. Problem: ",
           "the variable", enumerate_items(pblm, "s.quote.belong"), " to both data sets.")
    }
    
    qui_pblm = !blockvars %in% names(x) & !blockvars %in% names(y)
    if(any(qui_pblm)){
      pblm = blockvars[qui_pblm]
      stop("The argument `keep.only` must consist of only variables belonging to one of the two data sets. Problem: ",
           "the variable", enumerate_items(pblm, "s.quote.does"), " not belong to any data set.")
    }
    
    # We check no NA
    for(v in vx){
      if(anyNA(x[[v]])){
        qui_pblm = which(is.na(x[[v]]))
        stop("The variables used in the argument `keep.only` should not contain NA values. ", 
             "Problem: in the first data set, the variable `", v, "` contains ", fsignif(length(qui_pblm)), 
             " NA value", plural_len(qui_blm), ".")
        # new dreamerr (so much better!):
        # "Problem: in the first data set, the variable {bq ? v} contains {$n ? qui_pblm} NA value{$s}."
      }
    }
    
    for(v in vy){
      if(anyNA(y[[v]])){
        qui_pblm = which(is.na(y[[v]]))
        stop("The variables used in the argument `keep.only` should not contain NA values. ", 
             "Problem: in the second data set, the variable `", v, "` contains ", fsignif(length(qui_pblm)), 
             " NA value", plural_len(qui_blm), ".")
      }
    }
    
    block_expr = keep.only[[2]]
  }
  
  # The data: base_x and base_y
  keep_x = intersect(c(by, varkeep, blockvars), names(x))
  if(is.data.table(x)){
    base_x = subset(x, select = keep_x)
  } else {
    base_x = as.data.table(x)
    base_x = subset(base_x, select = keep_x)
  }
  
  keep_y = intersect(c(by, varkeep, blockvars), names(y))
  if(is.data.table(y)){
    base_y = subset(y, select = keep_y)
  } else {
    base_y = as.data.table(y)
    base_y = subset(base_y, select = keep_y)
  }
  
  # K: number of items on which to match
  K = length(by)
  
  # Checking the data
  is_character = rep(FALSE, K)
  for(k in 1:K){
    v = by[k]
    c_x = class(base_x[[v]])
    c_y = class(base_y[[v]])
    
    if(!identical(c_x, c_y)){
      stop("Each variable in 'by' must be of the same type in both data sets. But '", v, "' is of type ", enumerate_items(c_x, "quote"), " in x and ", enumerate_items(c_y, "quote"), " in y.")
    }
    
    is_character[k] = identical(c_x, "character")
    if(is_character[k]){
      # Normalizing the NAs
      setNA(base_x, v, "")
      setNA(base_y, v, "")
    }
    
  }
  
  if(any(!is_character)){
    # Later I will include non-character values
    # But only when I'll need it
    stop("So far only character variables are accepted in 'by'. The variable", enumerate_items(by[!is_character], "s.quote.is"), " not character.")
  }
  
  
  # The aliases
  by_alias = names(by)
  if(is.null(by_alias)){
    by_alias = by
  }
  
  qui0 = nchar(by_alias) == 0
  if(any(qui0)){
    by_alias[qui0] = by[qui0]
  }
  
  # there's a bug in check_value that I'll correct later
  valid_by = unique(c(by, by_alias))
  check_set_charin(initials, valid_by, "'by' (or its alias)")
  initial_alias = names(initials)
  
  do_initial = by %in% initials | by_alias %in% initials
  
  if(is.null(initial_alias)){
    initial_alias = paste0(by_alias, ".")
  } else {
    tmp = by_alias
    tmp[do_initial] = initial_alias
    initial_alias = tmp
  }
  
  fuzzy_alias = paste0(by_alias, "?")
  
  check_set_charin(empty, valid_by, "'by' (or its alias)")
  allowed_empty = by %in% empty | by_alias %in% empty
  
  check_set_charin(fuzzy, valid_by, "'by' (or its alias)")
  is_fuzzy = by %in% fuzzy | by_alias %in% fuzzy
  
  # Two variables can be swapped
  # One of the two can be empty. But not the two!!!!
  # The non empty one is the "main" one, the other is the "other" one
  # Say the two variables to be swapped are 'a' and 'b'
  #
  # What do we do?
  # 1) we order the two variables, such that it is always the most favorable case 
  #    that will be compared. ex: a:"leconte" b:"dujardin" will become a:"dujardin" b:"leconte"
  #    this means that x: a:"leconte" b:"dujardin" will be matched to y: a:"dujardin", b:"leconte"
  #
  # 2) if one is empty in one data set, we will duplicate rows for which the other data set is not missing
  #    ex: x: a:"dujardin" b:"leconte" will become two rows, a:"leconte" b:"" et a:"dujardin" b:""
  #    this means that y: a:"leconte" b:"" will be effectively matched to one row of x
  #
  check_set_charin(allow.swap, valid_by, "'by' (or its alias)")
  check_arg(allow.swap, "NULL character vector len(2)")
  allowed_swap = by %in% allow.swap | by_alias %in% allow.swap
  
  qui_swap = which(allowed_swap)
  is_swap = length(qui_swap) > 0
  is_swap_empty = FALSE
  if(is_swap){
    
    qui_swap_empty = which(allowed_swap & allowed_empty)
    if(length(qui_swap_empty) == 2){
      stop("If two variables are allowed to swap, **at most** one of them can be empty.",
           " Currently the two are allowed to be empty, this is not possible.")
    } else if(length(qui_swap_empty) == 1){
      is_swap_empty = TRUE
      
      id_swap_main = which(allowed_swap & !allowed_empty)
      id_swap_other = qui_swap_empty
      
      swap_main = by[id_swap_main]
      swap_other = by[qui_swap_empty]
      
    } else {
      swap_main = by[qui_swap[1]]
      swap_other = by[qui_swap[2]]
      
      id_swap_main = qui_swap[1]
      id_swap_other = qui_swap[2]
    }
    
    # we swap the two columns
    
    # for x
    cx_main = base_x[[swap_main]]
    cx_other = base_x[[swap_other]]
    
    qui = which(cx_main > cx_other & nchar(cx_other) > 0)
    tmp = cx_main[qui]
    cx_main[qui] = cx_other[qui]
    cx_other[qui] = tmp
    
    set(base_x, j = swap_main, value = cx_main)
    set(base_x, j = swap_other, value = cx_other)
    
    # for y
    cy_main = base_y[[swap_main]]
    cy_other = base_y[[swap_other]]
    
    qui = which(cy_main > cy_other & nchar(cy_other) > 0)
    tmp = cy_main[qui]
    cy_main[qui] = cy_other[qui]
    cy_other[qui] = tmp
    
    set(base_y, j = swap_main, value = cy_main)
    set(base_y, j = swap_other, value = cy_other)
    
    # I'm not keen on rm'ing but can be useful here
    rm(cx_main, cx_other, cy_main, cy_other, qui, tmp)
  }
  
  
  # Finding the total width (for display)
  ncollate = nchar(type.collate)
  w_type = 0
  for(k in 1:K){
    w = nchar(by_alias[k])
    if(is_fuzzy[k] && sum(is_fuzzy[1:k]) <= fuzzy.maxVars) w = w + 1
    if(do_initial[k] && nchar(initial_alias[k]) > w) w = nchar(initial_alias[k])
    w_type = w_type + w + (ncollate*(k<K))
  }
  
  # w_type = max(nchar(paste(by_alias, collapse = type.collate))) + (length(fuzzy) > 0)
  
  # The identifiers
  base_x[, id_x := 1:.N]
  base_y[, id_y := 1:.N]
  
  # SETTING KEYS LEAD TO A MAJOR BUG
  # setkeyv(base_x, by, physical = TRUE)
  # setkeyv(base_y, by, physical = TRUE)
  
  # Finding the types of each merging variable
  nchar_x = lapply(base_x[, ..by], nchar)
  nchar_y = lapply(base_y[, ..by], nchar)
  
  is_empty_x = sapply(nchar_x, min) == 0
  is_empty_y = sapply(nchar_y, min) == 0
  
  is_initial_x = sapply(nchar_x, function(n) 1 %in% n)
  is_initial_y = sapply(nchar_y, function(n) 1 %in% n)
  
  is_empty = is_empty_x | is_empty_y
  
  par = list()
  for(k in 1:K){
    par[[k]] = "full"
    
    if(is_fuzzy[k]){
      par[[k]] = c(par[[k]], "fuzzy")
    }
    
    if(allowed_empty[k]){
      if(is_empty[k]){
        par[[k]] = c(par[[k]], "empty")
      } else {
        stop("The variable '", by[k], "' is allowed to be empty, but in practice, it is never empty.")
      }
    }
    
    if(do_initial[k]){
      if(is_initial_x[k] || is_initial_y[k]){
        par[[k]] = c(par[[k]], "initial")
      } else {
        stop("The variable '", by[k], "' cannot be used as an initial since it is never composed of only 1 character. Doing so you would lose information and make wrong matches. I don't know if I'll implement it one day.")
      }
    }
  }
  
  if(fuzzy.maxVars >= 2) stop("You cannot have more than one fuzzy variable at a time. This may be implemented later.")
  
  cases_all = as.matrix(do.call("expand.grid", par))
  row2drop = apply(cases_all, 1, function(x) sum(x == "fuzzy")) > fuzzy.maxVars
  cases_all = cases_all[!row2drop, , drop = FALSE]
  
  # Computing the total number of cases
  cases_nb = list()
  for(i in 1:ncol(cases_all)){
    col_value = cases_all[, i]
    col_nb = rep(1, length(col_value))
    col_nb[col_value == "empty"] = 3
    col_nb[col_value == "initial"] = 2
    cases_nb[[i]] = col_nb
  }
  cases_nb = do.call(cbind, cases_nb)
  total_cases = sum(apply(cases_nb, 1, prod))
  
  message("Total of ", total_cases, " cases.")
  
  
  is_initial = names(by) %in% initials | by %in% initials
  
  n_x = nrow(base_x)
  n_y = nrow(base_y)
  
  w_nb = ceiling(log10(max(n_x, n_y)))
  w_nb = w_nb + floor((w_nb - 1) / 3)
  w_nb = max(w_nb, 3)
  
  all_titles = sfill(substr(by_alias, 1, 10), 10)
  message(sfill(" ", w_type + 1), " (", paste(all_titles, collapse = ", "), ") ", 
          sfill("# x", w_nb), " || ", sfill("# y", w_nb))
  
  n_merges = 0
  
  # Exact match
  n_cases = nrow(cases_all)
  res_all = vector("list", total_cases)
  for(case in 1:n_cases){
    # we recreate the cases from the initials
    
    main_case = cases_all[case, ]
    
    # We (again) expand the cases
    par = list()
    for(k in 1:K){
      if(main_case[k] == "initial"){
        # Initial is requested: either x or y
        par[[k]] = c("x:initial", "y:initial")
      } else if(main_case[k] == "empty"){
        # the k's variable can be empty
        par[[k]] = paste0("empty:", c("x&y", "!x&y", "x&!y"))
      } else if(main_case[k] == "fuzzy") {
        # we request a non-initial non-empty value
        par[[k]] = "both:fuzzy"
      } else {
        # we request a non-initial non-empty value
        par[[k]] = "both:full"
      }
    }
    par$stringsAsFactors = FALSE
    
    selections_all = as.matrix(do.call("expand.grid", par))
    # It reports all the cases.
    # It refers to what x should be
    # If x is "initial", then y is "full+initial"
    
    # display width (for the messages)
    sel_msg_all = sfill(apply(selections_all, 1, function(x) paste(sfill(x, 10), collapse = ", ")))
    
    # Identifiers of the merge
    id_merge = by[main_case != "empty"]
    
    id_alias_merge = by_alias
    id_alias_merge[main_case == "initial"] = initial_alias[main_case == "initial"]
    id_alias_merge[main_case == "fuzzy"] = fuzzy_alias[main_case == "fuzzy"]
    id_alias_merge = id_alias_merge[main_case != "empty"]
    my_type = paste0(id_alias_merge, collapse = type.collate)
    
    for(s in 1:nrow(selections_all)){
      # HERE WE MERGE
      
      sel = selections_all[s, ]
      
      # We create the selection
      qui_select_x = rep(TRUE, n_x)
      qui_select_y = rep(TRUE, n_y)
      
      # Whether we apply the algorithm for swapping values
      # We will create duplicates of the data, but only when appropriate 
      do_swap = FALSE
      if(is_swap_empty){
        sel_other = sel[id_swap_other]
        # here swapping means duplicating lines
        
        # ex. with the variables swapped named a and b
        # empty:!x&y (means only y is empty)
        # x: a:"dujardin" b:"leconte"
        # y: a: "leconte" b: ""
        # we duplicate x to have a: "dujardin" and another line a: "leconte"
        
        # if(grepl("empty", sel_other, fixed = TRUE)){
        if(sel_other %in% c("empty:x&!y", "empty:!x&y")){
          do_swap = TRUE
          is_empty_x = sel_other == "empty:x&!y"
          # => the selection for k == id_swap_other
          #    is performed normally in the next loop
          # Indeed, the only changes in selection affect the "main"
        }
      }
      
      for(k in 1:K){
        
        if(do_swap && k == id_swap_main){
          # we delay the selection for the main element
          next
        }
        
        if(sel[k] == "x:initial"){
          qui_select_x = qui_select_x & (nchar_x[[k]] == 1)
          qui_select_y = qui_select_y & (nchar_y[[k]] >= 1)
          
        } else if(sel[k] == "y:initial"){
          qui_select_x = qui_select_x & (nchar_x[[k]] >= 1)
          qui_select_y = qui_select_y & (nchar_y[[k]] == 1)
          
        } else if(sel[k] == "empty:x&y"){
          qui_select_x = qui_select_x & (nchar_x[[k]] == 0)
          qui_select_y = qui_select_y & (nchar_y[[k]] == 0)
          
        } else if(sel[k] == "empty:x&!y"){
          qui_select_x = qui_select_x & (nchar_x[[k]] == 0)
          qui_select_y = qui_select_y & (nchar_y[[k]] >= 1)
          
        } else if(sel[k] == "empty:!x&y"){
          qui_select_x = qui_select_x & (nchar_x[[k]] >= 1)
          qui_select_y = qui_select_y & (nchar_y[[k]] == 0)
          
        } else if(sel[k] == "both:full"){
          # non-empty, non-initial
          qui_select_x = qui_select_x & (nchar_x[[k]] >= 2)
          qui_select_y = qui_select_y & (nchar_y[[k]] >= 2)
        } else if(sel[k] == "both:fuzzy"){
          # non-empty, non-initial
          qui_select_x = qui_select_x & (nchar_x[[k]] >= fuzzy.minChar)
          qui_select_y = qui_select_y & (nchar_y[[k]] >= fuzzy.minChar)
        } else {
          stop("Internal error.")
        }
        
      }
      
      if(!(any(qui_select_x) && any(qui_select_y))) next
      
      # The merge
      if(!do_swap){
        # REGULAR case
        base_left = subset(base_x, subset = qui_select_x, select = c("id_x", id_merge))
        base_right = subset(base_y, subset = qui_select_y, select = c("id_y", id_merge))
        
      } else {
        # We apply do_swap
        # 
        # If we apply the swap, this means that we create duplicates for the non empty DB
        # Note that for the two DB before (left/right), all the selections are already applied
        
        base_left = subset(base_x, subset = qui_select_x)
        base_right = subset(base_y, subset = qui_select_y)
        
        if(is_empty_x){
          # we duplicate y
          qui_dup = base_right[[swap_other]] != ""
          extra = subset(base_right, qui_dup)
          set(extra, j = swap_main, value = extra[[swap_other]])
          
          base_right = rbindlist(list(base_right, extra))
        } else {
          # we duplicate x: symmetrical
          qui_dup = base_left[[swap_other]] != ""
          extra = subset(base_left, qui_dup)
          set(extra, j = swap_main, value = extra[[swap_other]])
          
          base_left = rbindlist(list(base_left, extra))
        }
        
        # We now apply the selection on id_swap_main that we had delayed until then
        qui_select_x = rep(TRUE, nrow(base_left))
        qui_select_y = rep(TRUE, nrow(base_right))
        
        nchar_left = nchar(base_left[[swap_main]])
        nchar_right = nchar(base_right[[swap_main]])
        
        sel_main = sel[id_swap_main]
        
        if(sel_main == "x:initial"){
          qui_select_x = qui_select_x & (nchar_left == 1)
          qui_select_y = qui_select_y & (nchar_right >= 1)
          
        } else if(sel_main == "y:initial"){
          qui_select_x = qui_select_x & (nchar_left >= 1)
          qui_select_y = qui_select_y & (nchar_right == 1)
          
        } else if(sel_main == "both:full"){
          qui_select_x = qui_select_x & (nchar_left >= 2)
          qui_select_y = qui_select_y & (nchar_right >= 2)
          
        } else if(sel_main == "both:fuzzy"){
          qui_select_x = qui_select_x & (nchar_left >= fuzzy.minChar)
          qui_select_y = qui_select_y & (nchar_right >= fuzzy.minChar)
          
        } else {
          stop("Internal error.")
        }
        
        if(!(any(qui_select_x) && any(qui_select_y))) next
        
        # The final data (containing duplicates)
        base_left = subset(base_left, subset = qui_select_x, select = c("id_x", id_merge))
        base_right = subset(base_right, subset = qui_select_y, select = c("id_y", id_merge))
      }
      
      # we create the initial variable(s)
      for(k in 1:K){
        if(sel[k] == "x:initial"){
          set(base_right, j = by[k], value = substr(base_right[[by[k]]], 1, 1))
          
        } else if(sel[k] == "y:initial"){
          set(base_left, j = by[k], value = substr(base_left[[by[k]]], 1, 1))
          
        } else if(sel[k] == "both:fuzzy"){
          # we create copies
          set(base_left, j = "fuzzy.x", value = base_left[[by[k]]])
          set(base_right, j = "fuzzy.y", value = base_right[[by[k]]])
          set(base_left, j = by[k], value = substr(base_left[[by[k]]], 1, fuzzy.firstCommon))
          set(base_right, j = by[k], value = substr(base_right[[by[k]]], 1, fuzzy.firstCommon))
        }
      }
      
      # for fuzzy matching, I must allow cartesian
      tmp = merge(base_left, base_right, by = id_merge, allow.cartesian = TRUE)
      
      n_merges = n_merges + 1
      
      check_fuzzy = sel == "both:fuzzy"
      if(any(check_fuzzy)){
        tmp = tmp[fuzzy.x != fuzzy.y]
        if(nrow(tmp) == 0) next
        tmp = tmp[string_proxi(fuzzy.x, fuzzy.y, method = fuzzy.method) >= fuzzy.threshold]
      }
      
      if(do_block && nrow(tmp) > 0){
        
        block_data = list()
        for(v in blockvars){
          if(v %in% vx){
            block_data[[v]] = base_x[[v]][tmp$id_x]
          }
          if(v %in% vy){
            block_data[[v]] = base_y[[v]][tmp$id_y]
          }
        }
        
        qui_ok = eval(block_expr, block_data)
        
        # The checking is a bit extensive but it can be worth it (at least in the development stage)
        check_value(qui_ok, "logical vector no na len(data)", .data = tmp, 
                    .message = "The expression to be evaluated in `keep.only` must lead to a logical vector of the same length as the data.")
        
        if(!all(qui_ok)){
          if(any(qui_ok)){
            tmp = tmp[qui_ok]
          } else {
            tmp = data.table()
          }
        }
        
      }
      
      if(nrow(tmp) > 0){
        tmp[, type := ..my_type]
        
        # Some info
        message(sfill(my_type, w_type), ": (", sel_msg_all[s], ") ", 
                sfill(signif_plus(length(unique(tmp$id_x))), w_nb), " || ", 
                sfill(signif_plus(length(unique(tmp$id_y))), w_nb))
        
        if(any(is_fuzzy)){
          # We save the variables that were fuzzy matched
          if(!"fuzzy.x" %in% names(tmp)){
            tmp[, fuzzy.x := ""]
            tmp[, fuzzy.y := ""]
          }
          
          res_all[[n_merges]] = tmp[, .(id_x, id_y, type, fuzzy.x, fuzzy.y)]
          
        } else {
          res_all[[n_merges]] = tmp[, .(id_x, id_y, type)]
        }
        
      } else {
        message(sfill(my_type, w_type), ": (", sel_msg_all[s], ") ", sfill(0, w_nb), " || ", sfill(0, w_nb))
      }
      
      if(gc.all) gc()
      
    }
  }
  
  message("--\n", signif_plus(n_merges), " merges in total, in ", format_difftime(time_start))
  
  base_match = unique(rbindlist(res_all))
  
  if(length(varkeep) > 0){
    for(v in varkeep){
      if(v %in% var_common){
        set(base_match, j = paste0(prefix[1], v, suffix[1]), value = base_x[[v]][base_match$id_x])
        set(base_match, j = paste0(prefix[2], v, suffix[2]), value = base_y[[v]][base_match$id_y])
      } else if(v %in% names(x)){
        set(base_match, j = v, value = base_x[[v]][base_match$id_x])
      } else {
        set(base_match, j = v, value = base_y[[v]][base_match$id_y])
      }
      
    }
  }
  
  base_match
}



####
#### string tools ####
####

# INFO:
# - some handy string manipulation tools

str_op = function(x, op, do_unik = NULL){
  
  check_arg(x, "character vector mbt l0")
  check_arg(op, "character scalar mbt")
  check_arg(do_unik, "NULL logical scalar")
  
  # For very large vectors, we unique
  n = length(x)
  
  if(n == 0) return(x)
  
  if(is.null(do_unik)) do_unik = n > 1e6
  
  if(do_unik){
    x_int = cpp_to_integer(x)
    x_small = x[!duplicated(x_int)]
    
    res_small = str_op(x_small, op, do_unik = FALSE)
    res = res_small[x_int]
  } else {
    operation = paste0(op, " ? x")
    res = .dsb(operation)
  }
  
  res
}

str_clean = function(x, pat){
  check_arg(x,  "character vector l0")
  check_arg(pat, "character vector no na")
  
  if(length(x) == 0) {
    return(character(0))
  }
  
  # t: trim
  specials = c("@t" = "(^[ \t]+)|([ \t]+$)")
  
  if(any(pat %in% names(specials))){
    qui = pat %in% names(specials)
    pat[qui] = specials[pat[qui]]
  }
  
  res = x
  for(v in pat){
    res = gsub(v, "", res, perl = TRUE)
  }
  
  res
}

str_get = function(x, pattern, special = TRUE, or = FALSE, seq = FALSE, seq.unique = TRUE){
  check_arg(pattern, "character vector no na mbt")
  check_arg(x, "character vector mbt")
  check_arg(special, or, seq, "logical scalar")
  
  set_check(FALSE)
  
  if(seq){
    for(i in seq_along(pattern)){
      value = str_get(x, pattern[i], special = special, or = or, seq = FALSE)
      if(i == 1){
        res = value
      } else {
        res = c(res, value)
      }
    }
    
    if(seq.unique){
      res = unique(res)
    }
    
    return(res)
  }
  
  index = str_is(x, pattern, special, or)
  x[index]
}

str_is = function(x, pattern, special = TRUE, or = FALSE){
  check_arg(pattern, "character vector no na mbt")
  check_arg(x, "character vector mbt")
  check_arg(special, or, "logical scalar")
  
  or_origin = or
  negate = FALSE
  do_negate = function(z) if(negate) !z else z
  logical_op = function(a, b) if(or) a | b else a & b
  
  for(i in seq_along(pattern)){
    pat = pattern[i]
    
    if(special){
      first_char = substr(pat, 1, 1)
      
      if(i > 1){
        if(first_char == "|"){
          or = TRUE
          pat = substr(pat, 2, nchar(pat))
          first_char = substr(pat, 1, 1)
        } else {
          or = or_origin
        }
      }
      
      negate = first_char == "!"
      if(negate){
        pat = substr(pat, 2, nchar(pat))
        first_char = substr(pat, 1, 1)
      }
      
      fixed = first_char == "#"
      if(fixed){
        pat = substr(pat, 2, nchar(pat))
      }
    }
    
    value = do_negate(grepl(pat, x, perl = !fixed, fixed = fixed))
    
    if(i == 1){
      res = value
    } else {
      res = logical_op(res, value)
    }
  }
  
  res
}

str_extract = function(x, n){
  
  check_arg(x, "vector l0")
  check_arg(n, "integer scalar")
  
  if(n == 0) return(character(length(x)))
  
  if(n > 0){
    res = substr(x, 1, n)
  } else {
    res = substr(x, nchar(x) - abs(n) + 1, nchar(x))
  }
  
  res
}

cleanCharVector = function(x, forceASCII = FALSE, 
                           changeCase = c("no", "upper", "lower"), 
                           cleanDigits = FALSE, cleanPunct = FALSE, 
                           cleanOneLetter = FALSE, 
                           custom_convert = NULL){
  # This function just "cleans" a character vector from its specific characters
  
  check_arg(x, "character vector")
  
  x = as.character(x)
  
  changeCase = match.arg(changeCase)
  
  ANY_FAILED = FALSE
  if(forceASCII){
    # force to ASCII using iconv. Keeps track of encoding error
    
    if(!is.null(custom_convert)){
      check_arg(custom_convert, "named character vector no na")
      for(i in seq_along(custom_convert)){
        from = names(custom_convert)[i]
        to = custom_convert[i]
        x = gsub(from, to, x, fixed = TRUE)
      }
    }
    
    # encoding 
    enco = setdiff(unique(Encoding(x)), "unknown")
    enc_from = if(any(enco %in% "UTF-8")) "UTF-8" else ""
    
    old_char = try(nchar(x), silent = TRUE)
    if("try-error" %in% class(old_char)){
      # super nasty encoding => nchar can't be used
      x_conv = iconv(x, from = enc_from, to = "ASCII//TRANSLIT//IGNORE")
      nbfail = 0
    } else {
      x_conv = iconv(x, from = enc_from, to = "ASCII//TRANSLIT//IGNORE")
      which_failed = which(old_char != nchar(x_conv) | grepl("\\?", x_conv, perl = TRUE))
      nbfail = length(which_failed)
    }
    
    if(nbfail > 0){
      # We manually remove the problems
      x_manual = ascii_convert_manual(x[which_failed])
      
      x_conv[which_failed] = x_manual
      
      # if necessary, we update the isFailed attribute
      if("isFailed" %in% names(attributes(x_manual))){
        ANY_FAILED = TRUE
        which_failed = which_failed[attr(x_manual, "isFailed")]
      }
    }
    
    # Now we save it
    x = x_conv
  }
  
  # Now we apply some cleaning:
  if(cleanDigits) x = gsub("[[:digit:]]", "", x, perl=TRUE) # supression des chiffres
  if(cleanPunct) x = gsub("[[:punct:]]", " ", x, perl=TRUE) # suppression de la ponctuation
  if(cleanOneLetter) x = gsub("\\b[[:alpha:]]\\b", " ", x, perl=TRUE) # suppression des mots d'une lettre
  
  if(changeCase != "no"){
    x = if(changeCase == "upper") toupper(x) else tolower(x)
  }
  
  x = gsub(" +", " ", x, perl = TRUE) # suppression des espaces
  x = trimws(x) # suppression des espaces
  
  if(ANY_FAILED){
    attr(x, "isFailed") = which_failed
  }
  
  x
}


ascii_convert_manual = function(x, type = c("all", "upper", "lower")){
  # This is an internal function that translates some characters into ascii
  # such characters could not be transformed using iconv
  # You can find country specific characters in http://german.typeit.org/
  
  type = match.arg(type)
  
  #
  # Lower characters
  #
  
  from_lower = c("á|à|å|æ|ä|â|ã|ą",
                 "é|è|ë|ê",
                 "í|ï|î",
                 "ó|ò|ô|ø|œ|õ",
                 "ú|ü|û|ù",
                 "ç",
                 "ñ",
                 "ß|ś")
  
  to_lower = c("a", "e", "i", "o", "u", "c", "n", "s")
  
  #
  # Upper characters
  #
  
  # toupper(from_lower)
  from_upper = c("Á|À|Å|Æ|Ä|Â|Ã",
                 "É|È|Ë|Ê", 
                 "Í|Ï|Î", 
                 "Ó|Ò|Ô|Ø|Œ|Õ", 
                 "Ú|Ü|Û|Ù", 
                 "Ÿ", 
                 "Ç", 
                 "Ñ", 
                 "ß")
  
  to_upper = c("A", "E", "I", "O", "U", "Y", "C", "N", "S")
  
  #
  # Go!
  #
  
  if(type == "upper"){
    from = from_upper
    to = to_upper
  } else if(type == "lower"){
    from = from_lower
    to = to_lower
  } else {
    from = c(from_upper, from_lower)
    to = c(to_upper, to_lower)
  }
  
  for(i in 1:length(from)){
    x = gsub(from[i], to[i], x, perl = TRUE)
  }
  
  # We check for the remaining problems
  x_conv = iconv(x, to = "ASCII")
  which_failed = which(is.na(x_conv))
  nb_failed = length(which_failed)
  
  # Chaque failed encoding a une signture speciale
  # eg oe => A\" at OE => A'
  
  #
  # SPECIFIC ENCODINGS
  #
  
  # It does not work => faux positifs	
  # from_special = c("A\"", "A'", # oe and OE
  # 		     "A?", "A-") # ö and Ö
  # to_special = c("o", "O",
  # 		   "o", "O")
  
  if(nb_failed > 0){
    
    x_new = iconv(x[which_failed], to = "ASCII//TRANSLIT//IGNORE")
    
    # for(i in 1:length(from_special)){
    # 	x_new = gsub(from_special[i], to_special[i], x_new, fixed = TRUE)
    # }
    
    # We check AGAIN for the remaining problems
    which_failed_special = which(nchar(x[which_failed]) != nchar(x_new))
    nb_failed = length(which_failed_special)
    
    # For the failed encodings, we just replace the failed values by underscores
    # We don't let the value of "ASCII//TRANSLIT//IGNORE" because it can be very problematic
    if(nb_failed > 0) x_new[which_failed_special] = iconv(x[which_failed][which_failed_special], to = "ASCII", sub = "_")
    
    x[which_failed] = x_new
    
    if(nb_failed > 0){
      warning("There has been ", nb_failed, " failed encoding. Attribute isFailed is created to track the problems.")
      attr(x, "isFailed") = which_failed
    }
    
  }
  
  return(x)
}


extract_vars = function(x, pattern, order = NULL, skip_absent = FALSE){
  
  
  mc = match.call()
  if("x" %in% names(mc) && mc$x == "."){
    # this is a data table call
    sc_all = sys.calls()
    if(length(sc_all) >= 4 && as.character(sc_all[[length(sc_all) - 3]][1])[1] == "[.data.table"){
      x = eval(quote(names_x), parent.frame(3))
    }
  }
  
  check_arg(x, "mbt vector | data.frame")
  check_arg(pattern, "character vector no na mbt")
  check_arg(order, "NULL character vector no na")
  check_arg(skip_absent, "logical scalar")
  
  is_data_set = is.data.frame(x)
  if(is_data_set){
    x = names(x)
  } else if(!is.character(x)){
    x = as.character(x)
  }
  
  x_origin = x
  
  if(any(grepl("||", pattern, fixed = TRUE))){
    if(length(pattern) > 1) stop("You cannot use the `||` ordering if `pattern` has length > 1.")
    pat_split = str_op(pattern, "'||'s, w")
    pattern = pat_split[1]
    order = str_op(pat_split[2], "'[, \t\n]+'S")
  }
  
  pattern = str_op(pattern, "'[, \t\n]+'S")
  
  # default: full var
  qui_all = rep(FALSE, length(x))
  init = TRUE
  res = c()
  for(p in pattern){
    p_raw = p
    p_clean = NULL
    first_char = substr(p, 1, 1)
    
    is_and = first_char == "&"
    if(is_and){
      p = substr(p, 2, nchar(p))
      first_char = substr(p, 1, 1)
      all_vars = x_origin
    } else {
      all_vars = x
    }
    
    negate = first_char == "!"
    if(negate){
      p = substr(p, 2, nchar(p))
      first_char = substr(p, 1, 1)
    }
    
    if(first_char %in% c("#", "@", "^")){
      # fixed char search
      fixed = first_char == "#"
      p = substr(p, 2, nchar(p))
      
      if(first_char == "^"){
        p_clean = p
        p = paste0("^\\Q", p, "\\E")
      }
      
      qui = grepl(p, all_vars, perl = !fixed, fixed = fixed)
      
    } else {
      qui = all_vars == p
    }
    
    if(!any(qui) || (negate && all(qui))){
      
      if(skip_absent){
        next
      }
      
      info = switch(first_char, 
                    "#" = "fixed pattern", 
                    "@" = "regular expression", 
                    "^" = "partial matching of", 
                    "value")
      
      if(!is.null(p_clean)) p = p_clean
      
      p_info = if(p != p_raw) paste0("(raw is `", p_raw, "`) ") else ""
      consequence = "led to no variable being selected."
      if(info == "value"){
        consequence = paste0("is not ", ifelse(is_data_set, "a variable from the data set.", "present."))
      } 
      extra = if(info == "value") " Note that you can use regex with `@` and partial matching with `^`." else ""
      
      stop("In the argument `pattern`, the ", info, " `", p, "` ", p_info, consequence, extra)
    }
    
    if(init){
      if(negate){
        res = all_vars[!qui]
        if(!is_and){
          # we restrict the values (unless asked explicitly not too)
          x = res
        }
      } else {
        res = all_vars[qui]
      }
      
    } else {
      if(negate){
        if(is_and){
          # we want the negated variables in
          wanted = setdiff(all_vars[!qui], res)
          res = c(res, wanted)
        } else {
          # we want the negated variables out
          unwanted = all_vars[qui]
          x = all_vars[!qui]
          res = setdiff(res, unwanted)
        }
      } else {
        # we respect the order of the user
        wanted = setdiff(all_vars[qui], res)
        res = c(res, wanted)
      }
      
    }
    
    init = FALSE
  }
  
  n_o = length(order)
  if(n_o == 0){
    return(res)
  }
  
  for(i in n_o:1){
    or_raw = or = order[i]
    or_clean = NULL
    
    first_char = substr(or, 1, 1)
    negate = first_char == "!"
    if(negate){
      or = substr(or, 2, nchar(or))
      first_char = substr(or, 1, 1)
    }
    
    if(first_char %in% c("#", "@", "^")){
      fixed = first_char == "#"
      or = substr(or, 2, nchar(or))
      
      if(first_char == "^"){
        or_clean = or
        or = paste0("^\\Q", or, "\\E")
      }
      
      qui = grepl(or, res, perl = !fixed, fixed = fixed)
      
    } else {
      qui = res == or
    }
    
    if(!any(qui)){
      
      if(skip_absent){
        next
      }
      
      info = switch(first_char, 
                    "#" = "fixed pattern", 
                    "@" = "regular expression", 
                    "^" = "partial matching of", 
                    "value")
      
      if(!is.null(or_clean)) or = or_clean
      or_info = if(or != or_raw) paste0("(raw is `", or_raw, "`) ") else ""
      consequence = "led to no variable being selected."
      if(info == "value"){
        consequence = paste0("is not ", ifelse(is_data_set, "a variable from the data set.", " present."))
      } 
      extra = if(info == "value") " Note that you can use regex with `@` and partial matching with `^`." else ""
      
      stop("In the argument `pattern`, the ", info, " `", or, "` ", or_info, consequence, extra)
    }
    
    if(negate){
      res = c(res[!qui], res[qui])
    } else {
      res = c(res[qui], res[!qui])
    }
    
  }
  
  res
}

#' Computes proximities between strings
#' 
#' Computes proximities between character strings. 
#'
#' @param x A character vector.
#' @param y A character scalar, or a character vector of the same length as x.
#' @param min_char Minimum number of characters to perform a comparison. Default is 0, meaning all pairs of strings are compared. Example, if \code{min_char = 5} then \code{FALSE} is returned for any pair of string containing one element with strictly less than 5 characters.
#' @param incl.loose Logical, default is `FALSE`. Whether the inclusion method should use pattern matching or word matching. Ex: `"bonjour Ed"` will not match `"bonjour Edouard"` by default.
#' @param incl.misspell Logical, default is \code{FALSE}. Whether the test of inclusion should allow a single misspell.
#' @param method Either "jaroWinkler", "unitary" or "inclusion". Note that the "inclusion" method by default corresponds to word inclusions (and not pattern inclusions: "bol" will not match "bolaget"). You can have a pattern inclusion with `incl.loose = TRUE`.
#' @param empty Numeric scalar equal to 0 (default) or 1. The value to assign to pairs of empty character strings.
#' @param nthreads Integer, defaults to 1.
#' @param notes Logical, default is \code{TRUE}. Whether to display notes.
#' 
#' 
#' @details 
#' The \code{unitary} distance returns a boolean where 1 means that the two strings are either identical, either are different from only one i) addition, ii) suppression, iii) permutation, or iv) modification.
#'
#' @return
#' It returns a vector of the same length as x.
#'
#' @examples
#' 
#' x = c("bonjour", "au revoir")
#' y = c("boijour", "au revoir")
#' 
#' string_distance(x, y)
#' 
#' # Unitary distance: everything is at distance 1 but the last:
#' x = c("bonjour", "boijour", "bojour", "obnjour", "bonjours", "aunjour")
#' y = c("bonjour")
#' 
#' string_distance(x, y, "u")
#' 
#' 
string_proxi = function(x, y, method = "jaroWinkler", min_char = 0, 
                        incl.loose = FALSE, incl.misspell = FALSE, empty = 0,
                        nthreads = getFixest_nthreads(), notes = TRUE){
  
  check_arg(x, "character vector NA OK mbt")
  check_arg(y, "mbt character vector len(1) | character vector len(data) NA OK", .data = x)
  check_set_arg(method, "match(jaroWinkler, unitary, inclusion)")
  check_arg(nthreads, "integer scalar GE{1}")
  check_arg(incl.loose, incl.misspell, "logical scalar")
  check_arg(empty, "integer scalar GE{0} LE{1}")
  
  if(nthreads > 1 && nthreads > get_nb_threads()){
    nthreads = max(get_nb_threads(), 1)
    message("Argument `nthreads` cannot be greater than the maximum number of threads (here ", get_nb_threads(), "). It has been set to ", nthreads, ".")
  }
  
  # Handling NAs
  n_x = length(x)
  n_y = length(y)
  
  qui_NA = is.na(x)
  n_na_x = sum(qui_NA)
  
  if(n_na_x == n_x){
    if(n_x > 1 && notes) message("All values in x are NA.")
    return(rep(NA, n_x))
  }
  
  if(n_y > 1){
    qui_NA_y = is.na(y)
    n_na_y = sum(qui_NA_y)
    qui_NA = qui_NA | qui_NA_y
    n_na = sum(qui_NA)
    
  } else if(is.na(y)){
    if(n_x > 1 && notes) message("y is a scalar equal to NA: all results are NA.")
    return(rep(NA, n_x))
    
  } else {
    n_na_y = 0
    n_na = n_na_x
  }
  
  if(n_na > 0){
    if(n_na == n_x){
      if(notes) message("All results are NAs due to NA values in either x or y.")
      return(rep(NA, n_x))
    }
    
    if(notes) message("NOTE: ", n_na, " NA values (x: ", n_na_x, ", y: ", n_na_y, ")")
    
    x_ok = x[!qui_NA]
    if(n_y > 1){
      y_ok = y[!qui_NA]
    } else {
      y_ok = y
    }
    
  } else {
    x_ok = x
    y_ok = y
  }
  
  if(method == "jaroWinkler"){
    res_ok = cpp_jaroWinkler(x_ok, y_ok, min_char, empty, nthreads)
    
  } else if(method == "unitary"){
    res_ok = cpp_unitary_stringDist(x_ok, y_ok, min_char, empty, nthreads)
    
  } else if(method == "inclusion"){
    res_ok = cpp_inclusion(x_ok, y_ok, incl.loose, incl.misspell, min_char, empty, nthreads)
  }
  
  if(n_na > 0){
    res = rep(NA_real_, n_x)
    res[!qui_NA] = res_ok
  } else {
    res = res_ok
  }
  
  res
}

part_string_proxi = function(x, y, irrelevant = NULL){
  # We look at the inclusion of each word of x in y and vice versa
  # This is pretty loose
  
  check_arg(x, "character vector mbt")
  check_arg(y, "character vector mbt len(data)", .data = x)
  check_arg(irrelevant, "NULL character vector no na")
  
  x_split = strsplit2df(x, split = " ", addPos = TRUE)
  y_split = strsplit2df(y, split = " ", addPos = TRUE)
  
  x_split[, first_letter := substr(x, 1, 1)]
  x_split[, n_x := .N, by = id]
  setnames(x_split, "pos", "pos_x")
  
  y_split[, first_letter := substr(x, 1, 1)]
  y_split[, n_y := .N, by = id]
  setnames(y_split, "x", "y")
  setnames(y_split, "pos", "pos_y")
  
  xy = merge(x_split, y_split, all = TRUE)
  xy[, same := string_proxi(x, y, "inclusion", incl.loose = TRUE, incl.misspell = TRUE, notes = FALSE)]
  setNA(xy, "same", 0)
  
  xy[, relevant := TRUE]
  if(length(irrelevant) > 0){
    xy[, relevant := !x %in% irrelevant & !y %in% irrelevant]
  }
  
  # on x:
  x_agg = xy[!is.na(pos_x), .(same = max(same), relevant = min(relevant)), by = .(id, pos_x)]
  x_agg_agg = x_agg[, .(n = .N, n_relevant = sum(relevant), 
                        n_same = sum(same), n_same_relevant = sum(same * relevant)), 
                    by = id]
  id_x = x_agg_agg[n_relevant > 0 & n_relevant == n_same_relevant, id]
  
  # on y:
  y_agg = xy[!is.na(pos_y), .(same = max(same), relevant = min(relevant)), by = .(id, pos_y)]
  y_agg_agg = y_agg[, .(n = .N, n_relevant = sum(relevant), 
                        n_same = sum(same), n_same_relevant = sum(same * relevant)), 
                    by = id]
  id_y = y_agg_agg[n_relevant > 0 & n_relevant == n_same_relevant, id]
  
  res = numeric(length(x))
  
  id_keep = unique(id_x, id_y)
  res[id_keep] = 1
  
  res
}


sample_pattern = function(x, pattern, n1 = 10, n2 = 5, SEED = NULL, cumul = TRUE){
  # x: a character vector
  # pattern: the pattern of x (a category)
  # n1: the number of categories
  # n2: the number of values to display for each category
  
  check_arg(x, "character vector")
  check_set_arg(pattern, "character vector conv len(data)", .data = x)
  check_arg(n1, n2, "integer scalar")
  
  if(!is.null(SEED)){
    set.seed(SEED)
  }
  
  info = sort(table(pattern), decreasing = TRUE)
  n1 = min(length(info), n1)
  nm = names(info)
  
  sh = if(cumul) cumsum(info) / sum(info) * 100 else  as.vector(info) / sum(info) * 100
  sh = head(sh, n1)
  
  shares = format(sprintf("%.1f", sh), justify = "r")
  
  res = c()
  for(i in 1:n1){
    
    pat = nm[i]
    if(info[i] <= n2){
      v = x[pattern == pat]
      value = character(n2)
      value[1:length(v)] = v
    } else {
      value = sample(x[pattern == pat], n2)
    }
    
    res = rbind(res, value)
  }
  
  # row numbers
  nb = format(c("  ", sprintf("%02i", 1:n1)))
  
  n2 = ncol(res)
  
  res = rbind(1:n2, res)
  
  i_add_nb = Inf
  if(n2 > 3){
    i_add_nb = floor(n2 / 2)
  }
  
  for(i in 1:n2){
    fmt = format(res[, i])
    suffix = if(i %in% c(i_add_nb, n2)) paste0(" |", nb, "| ") else " | "
    res[, i] = paste0(fmt, suffix)
  }
  
  all_values = apply(res, 1, paste, collapse = "")
  
  pattern_fmt = format(c("", head(nm, n1)))
  shares_fmt = c(substr("       ", 1, nchar(shares[1])), head(shares, n1))
  
  res = cbind("#", nb, "| ", pattern_fmt, " (", shares_fmt, "%): ", all_values)
  res = apply(res, 1, paste, collapse = "")
  
  res[1] = gsub("[^[:digit:]#]", " ", res[1])
  
  cat(res, sep = "\n")
  
}

#' Splits strings-id pairs and format them to a data frame
#' 
#' This function splits a character vector associated to a ID vector. The outcome is a data.table with two columns, one for the Id and another for the split elements of the character vector.
#'
#' @param x_or_fml A formula of the type x~id, or a character vector. The formula can contain multiple IDs, like \code{x ~ id_1 + id_2}.
#' @param base A data base containing the elements of \code{x_or_fml}.
#' @param split Character scalar.
#' @param id The vector (or data.frame) of IDs. Used only of the argument \code{x_or_fml} is a vector. If \code{x} is a vector and \code{id} is missing, then by default \code{id = 1:length(x)}.
#' @param addPos Logical, default is \code{FALSE}. If \code{TRUE}, it adds the position of each word in the sentence of each ID.
#' @param fixed Logical, default is \code{FALSE}. Whether regular expressions should be used.
#' @param perl Logical, default is \code{TRUE}. Should Perl-compatible regexps be used?
#' @param id_unik Logical: if \code{TRUE} (default) then an error is thrown if the identifier is not unique. 
#'
#' @return
#' It returns a two column data table. Note that if the data in entry was a data.frame, a data.frame is returned. Otherwise, it is a data.table.
#'
#' @examples
#' 
#' base = data.frame(myID = 1:4, fruits = c("banana", "banana, apple, orange", "lemon, cherry", "strawberry, blueberry, pinapple"))
#' 
#' strsplit2df(fruits ~ myID, base, split = ", ")
#' 
#' base = data.frame(id1 = c(1, 1, 2, 3, 3), id2 = c("a", "b", "c", "a", "b"), 
#'                   text = c("bonjour", "les gens", "le bonheur", "ressemble souvent", "a du cassoulet"), 
#'                   stringsAsFactors = FALSE)
#'                   
#' strsplit2df(text ~ id1 + id2, base, split = " ")
#' 
strsplit2df = function(x_or_fml, base, split, id, addPos = FALSE, fixed = FALSE, 
                       perl = TRUE, id_unik = TRUE){
  
  # IDEAS:
  # - add op, op.post, to perform string operations
  #   |-> do it in all string.magick functions
  # - do not use data.table, only if the class(base) is a data table or if data.table is loaded
  #   |-> create a plain list of results, convert to DF or other at the very end
  # - skip.empty = FALSE, whether to skip the empty strings.
  #  |-> x_len[x_len == 0] = 1
  
  # Extraction of the data
  # Extracting x and the moderator:
  useDF = FALSE
  isSeveral = FALSE
  isDefault = FALSE
  names_both = c("id", "x")
  info_not_unique = FALSE
  if("formula" %in% class(x_or_fml)){
    if(missing(base) || !is.data.frame(base)){
      stop("If you provide a formula, a data.frame must be given in argument 'base'.")
    }
    
    if(!is.data.table(base)){
      useDF = TRUE
    }
    
    # Creation of x and id
    fml = x_or_fml
    if(!length(fml) == 3){
      stop("The formula must be of the type x ~ id.")
    }
    
    x = eval(fml[[2]], base)
    id_names = all.vars(fml[[3]])
    if(length(id_names) > 1){
      isSeveral = TRUE
      
      if(!all(id_names %in% names(base))){
        stop("All IDs in the formula must be in the base (", enumerate_items(setdiff(id_names, names(base)), "is"), " not).")
      }
      
      if(is.data.table(base)){
        id_several = base[, id_names, with = FALSE]
      } else {
        id_several = base[, id_names]
      }
      
      if(id_unik && nrow(unique(id_several)) != nrow(id_several)){
        info_not_unique = TRUE
        message("The identifiers are not unique, you will not be able to distinguish the sentences).")
      }
      
      id = 1:nrow(id_several)
      names_both = as.character(c("xxidxx", fml[[2]]))
    } else {
      id = eval(fml[[3]], base)
      names_both = as.character(c(fml[[3]], fml[[2]]))
    }
    
  } else if(!missing(id) && !is.null(id)){
    x = x_or_fml
    
    if(!isVector(x)){
      stop("If 'x' is not a formula, it must be a character vector.")
    }
    
    if(NCOL(id) == 1){
      if(length(x) != length(id)){
        stop("The arguments 'x' and 'id' must be of the same length.")
      }
      id = as.vector(id)
    } else {
      # several IDs
      isSeveral = TRUE
      
      id_several = id
      id_names = names(id_several)
      names_both = c("xxidxx", "x")
      if(id_unik && nrow(unique(id_several)) != nrow(id_several)){
        stop("The identifiers are not unique, please provide unique ones (otherwise you cannot distinguish the sentences.")
      }
      
      id = 1:nrow(id_several)
    }
    
  } else {
    x = x_or_fml
    
    if(!isVector(x)){
      stop("If 'x' is not a formula, it must be a character vector.")
    }
    
    # By default, we create the id is 1:length(x)
    id = 1:length(x)
    isDefault = TRUE
  }
  
  # Check that the id is unique
  n_id = length(id)
  if(!isDefault && id_unik && n_id != length(unique(id))){
    if(!info_not_unique) message("The identifiers are not unique, you will not be able to distinguish the sentences.")
  }
  
  if(!is.character(x)){
    x = as.character(x)
  }
  
  x_split = strsplit(x, split, fixed = fixed, perl = perl)
  
  n_all = lengths(x_split)
  
  # the result table
  res = data.table(id = rep(id, n_all), x = unlist(x_split))
  
  if(addPos){
    # the position
    res[, pos := rowid(id)]
  }
  
  setnames(res, c("id", "x"), names_both[1:2])
  
  if(isSeveral){
    for(i in id_names){
      res[, ":="(eval(i), id_several[[i]][xxidxx])]
    }
    res[, xxidxx := NULL]
  }
  
  if(useDF){
    setDF(res)
  }
  
  res
}

####
#### data manipulation ####
####


readfst = function(path, vars = NULL, forte = FALSE){
  # reads fst files and set the data set to data.table
  # enhances the variable selection with the selvars paradigm
  
  check_arg(path, "character scalar mbt")
  check_arg(vars, "NULL character vector no na")
  
  path = dsb(path, frame = parent.frame())
  
  if(forte && !file.exists(path)){
    stop(dsb("The file `.['.+/'R ? path]` does not exist. Maybe the step leading to its creation ", 
             "has been missed? ",
             "\nLook at the FORTE-documentation.pdf to find out which step needs ", 
             "to be run in the file `main.R` to create it."))
  }
  
  if(!is.null(vars)){
    
    vnames = names(fst(path))
    
    if(identical(vars, "?")){
      message("The variables are:\n", fit_screen(str_op(vnames, "C"), width = 80))
      return()
    }
    
    vars = extract_vars(vnames, vars)
    
  }
  
  read_fst(path, columns = vars, as.data.table = TRUE)
}

start_outfile = function(outfile){
  # function to lighten the writing of the too many functions
  
  # we systematically garbage collect before running the functions 
  # and start_outfile is always called at the beginning of the functions
  gc()
  
  res = list(path = outfile, time = Sys.time())
  class(res) = "outfile"
  
  message("\nStarting the construction of `", outfile, "`...")
  
  res
}

write_fst_forte = function(x, outfile){
  # Just adds a message regarding building time after writing
  
  check_arg(outfile, "class(outfile) mbt")
  write_fst(x, outfile$path)
  
  message("=> `", outfile$path, "` built in ", format_difftime(outfile$time), ".")
}

# enhancement of the variables selection in data table
"[.data.table" = function(x, i, j, ...){
  
  mc = match.call()
  if("j" %in% names(mc) && is.character(mc$j)){
    j = extract_vars(x, j)
    mc$j = str2lang(capture.output(dput(j)))
  }
  
  mc[[1]] = as.name("sub_dt")
  my_list = list(sub_dt = asNamespace("data.table")[["[.data.table"]])
  
  eval(mc, my_list, enclos = parent.frame())
}

setNA = function (x, var, value, silent = FALSE){
  
  if(length(var) == 0) return(invisible(NULL))
  
  check_arg(x, "class(data.table) mbt")
  check_arg(var, "character vector no na mbt")
  check_arg(value, "scalar mbt")
  
  var_all = extract_vars(x, var)
  
  is_na_var = sapply(var_all, function(v) anyNA(x[[v]]))
  var_all = var_all[is_na_var]
  
  for(v in var_all){
    
    if(is.character(value) && !is.character(x[[v]])){
      stop(dsb("The types of the variable .[q ? v] (.[q, C ? class(v)]) ",
               "does not match the type of the replacement which is character."))
    }

    set(x, which(is.na(x[[v]])), v, value)
  }
}


setNA_char = function(data, value = ""){
  
  who_na_char = sapply(data, function(x) is.character(x) && anyNA(x))
  vars_NA_char = names(data)[who_na_char]
  setNA(data, vars_NA_char, "")
  
}


merge_value = function(key, key_ext, value_ext){
  # the key_ext must be unique
  # pretty slow: to improve later
  
  check_arg(key, "vector")
  check_arg(key_ext, "vector no na")
  check_arg(value_ext, "vector len(data)", .data = key_ext)
  
  if(anyDuplicated(key_ext)){
    stop("The argument `key` must not contain duplicate values!")
  }
  
  value = setNames(value_ext, key_ext)
  
  value[as.character(key)]
}


#' Creates a bilateral data et
#' 
#' This function creates a bilateral data set of nodes connected via a link. For each link, we end up with the combination of all pairs of nodes. Think to network data.
#'
#' @param x A data.frame containing the link and node information.
#' @param link The name of the link.
#' @param node The name of the node.
#' @param prefix Character vector of length 2 (mandatory if suffix is missing). The prefix to be applied to the node names.
#' @param suffix Character vector of length 2 (mandatory if prefix is missing). The suffix to be applied to the node names.
#' @param varkeep Character vector of the variables to keep ONLY unique to the link. To add variables on both sides, use function \code{bilateral_merge}
#' @param dropSame Default is \code{TRUE}. Should identical nodes be dropped?
#'
#' @return
#' It returns a data.table with three variables: the link and the two nodes.
#' 
#' @seealso 
#' \code{\link{bilateral_merge}}
#'
#' @examples
#' base = data.frame(patent_id = c(1, 1, 1, 2, 3, 3), 
#' inventor = c("a", "b", "c", "a", "b", "d"))
#' 
#' base_value = data.frame(inventor = letters[1:4], value = rpois(4, 10))
#' 
#' (base_bi <- bilateral_make(base, "patent_id", "inventor", suffix = c("_i", "_j")))
#' 
#' bilateral_merge(base_bi, base_value, suffix = c("_i", "_j"))
#' 
bilateral_make = function(x, link, node, varkeep, prefix, suffix, dropSame = TRUE){
  # This function makes a bilateral dataset
  # It does NOT drop the link
  
  #
  # Controls
  #
  
  check_arg(x, "data.frame mbt")
  check_arg(link, "character scalar mbt")
  check_arg(node, "character scalar mbt")
  check_arg(varkeep, "character vector")
  check_arg(dropSame, "logical scalar")
  
  if(missing(prefix) && missing(suffix)){
    stop("You must provide a prefix or suffix, otherwise the variables cannot be differentiated.")
  } else if(missing(prefix)){
    prefix = c("", "")
    if(length(suffix) != 2) stop("Argument 'suffix' must be of lenght 2.")
  } else if(missing(suffix)){
    suffix = c("", "")
    if(length(prefix) != 2) stop("Argument 'prefix' must be of lenght 2.")
  }
  check_arg(prefix, "characterVector")
  check_arg(suffix, "characterVector")
  
  if(!link %in% names(x)){
    stop("Argument 'link' must be a variable from 'x' (", link, " isn't).")
  }
  if(!node %in% names(x)){
    stop("Argument 'node' must be a variable from 'x' (", node, " isn't).")
  }
  if(missing(varkeep)){
    varkeep = c()
  }
  if(!all(varkeep %in% names(x))){
    stop("Variables in 'varkeep' must be valid columns of 'x' (", enumerate_items(setdiff(varkeep, names(x)), "is"), " not).")
  }
  if(any(varkeep %in% c(link, node))){
    stop("Variables in 'varkeep' cannot be the ", c("link", "node")[which.max(c(link, node) %in% varkeep)], " name.")
  }
  
  #
  # Algorithm
  #
  
  library(data.table)
  if("data.table" %in% class(x)){
    base_left = x[, c(link, node, varkeep), with = FALSE]
  } else {
    base_left = as.data.table(x[, c(link, node, varkeep)])
  }
  names(base_left)[1:2] = c("link", "node_left")
  
  if(dropSame){
    base_left[, nb := .N, by = link]
    base_left = base_left[nb > 1]
    base_left[, nb := NULL]
  } 
  
  base2merge = base_left[, 1:2]
  names(base2merge)[2] = "node_right"
  
  res = merge(base_left, base2merge, allow.cartesian = TRUE, by = "link")
  
  if(dropSame){
    res = res[node_left != node_right]
  }
  
  setnames(res, c("link", "node_left", "node_right"), c(link, paste0(prefix, node, suffix)))
  
  res
}




#' Merge information to bilateral data
#' 
#' This function merges the information of y twice to the bilateral table x. 
#' 
#' @inheritParams bilateral_make
#'
#' @param x A data.frame.
#' @param y A data.frame. It should contain the data to augment the information in x.
#' @param key A character vector of keys used to merge x and y and whose names won't be modified by the prefix and suffix. 
#' @param all.x Default is \code{TRUE}.
#'
#' @return
#' This function returns a data.table of the number of columns of x plus twice the number of columns of y (minus the keys).
#' 
#' @seealso 
#' \code{\link{bilateral_make}}
#'
#' @examples
#' base = data.frame(patent_id = c(1, 1, 1, 2, 3, 3), 
#' inventor = c("a", "b", "c", "a", "b", "d"))
#' 
#' base_value = data.frame(inventor = letters[1:4], value = sample(4))
#' 
#' (base_bi <- bilateral_make(base, "patent_id", "inventor", suffix = c("_i", "_j")))
#' 
#' bilateral_merge(base_bi, base_value, suffix = c("_i", "_j"))
#' 
#' # Example with key:
#' 
#' base = data.frame(patent_id = c(1, 1, 1, 2, 3, 3), 
#'   inventor = c("a", "b", "c", "a", "b", "d"), 
#'   year = c(rep(1999, 4), rep(2000, 2)))
#' 
#' base_value = data.frame(inventor = rep(letters[1:4], 2), 
#'   value = sample(8), year = rep(1999:2000, each = 4))
#' 
#' (base_bi <- bilateral_make(base, "patent_id", "inventor", varkeep = "year", suffix = c("_i", "_j")))
#' 
#' bilateral_merge(base_bi, base_value, suffix = c("_i", "_j")) # don't work => cartesian
#' bilateral_merge(base_bi, base_value, key = "year", suffix = c("_i", "_j"))
#' 
bilateral_merge = function(x, y, key, prefix, suffix, all.x = TRUE){
  # merge twice the y data to x
  # changes the y_names
  # key: fixed key (name won't change)
  
  if(missing(prefix) && missing(suffix)){
    stop("You must provide a prefix or suffix, otherwise the variables cannot be differentiated.")
  } else if(missing(prefix)){
    prefix = c("", "")
    if(length(suffix) != 2) stop("Argument 'suffix' must be of lenght 2.")
  } else if(missing(suffix)){
    suffix = c("", "")
    if(length(prefix) != 2) stop("Argument 'prefix' must be of lenght 2.")
  }
  
  check_arg(prefix, suffix, "character vector len(2)")
  check_arg(x, y, "data.frame mbt")
  
  if(missing(key) || length(key) == 0){
    key = c()
  } else {
    check_arg(key, "character vector")
    miss_x = setdiff(key, names(x))
    miss_y = setdiff(key, names(y))
    if(length(miss_x) > 0){
      stop("Variables of argument 'key' must be in both tables (", enumerate_items(miss_x, "is"), " not in 'x').")
    }
    if(length(miss_y) > 0){
      stop("Variables of argument 'key' must be in both tables (", enumerate_items(miss_y, "is"), " not in 'y').")
    }
  }
  
  # copy because of the behavior of data.table, too risky
  if("data.table" %in% class(y)){
    base2merge = copy(y)
  } else {
    base2merge = y
  }
  
  names_left = names_right = names(y)
  
  # var2keep = names(y) %in% names(x)
  var2keep = names(y) %in% key
  if(sum(var2keep) == ncol(y)){
    stop("The function cannot work because all names of y are in the names of x.")
  } else {
    names_left[!var2keep] = paste0(prefix[1], names(y)[!var2keep], suffix[1])
    names_right[!var2keep] = paste0(prefix[2], names(y)[!var2keep], suffix[2])
  }
  
  by_left = intersect(names(x), names_left)
  by_right = intersect(names(x), names_right)
  if(length(by_left) == 0){
    stop("No variable was found to merge. None of these variables were in x: ", enumerate_items(names_left, verb = FALSE))
  } else if(length(by_right) == 0){
    stop("No variable was found to merge. None of these variables were in x: ", enumerate_items(names_right, verb = FALSE))
  }
  
  names(base2merge) = names_left
  res = merge(x, base2merge, all.x = all.x, by = by_left)
  
  # second merge
  names(base2merge) = names_right
  res = merge(res, base2merge, all.x = all.x, by = by_right)
  
  res
}


rename_loose = function(x, from, to){
  # we rename with from-to
  
  check_arg(x, "data.frame")
  check_arg(from, "character vector no na")
  check_arg(to, "character vector no na len(data)", .data = from)
  
  dict = setNames(to, from)
  
  x_names = names(x)
  qui = which(x_names %in% from)
  x_names[qui] = dict[x_names[qui]]
  
  names(x) = x_names
  
  x
}

update_year_range = function(year_range, year){
  # we gather the information on the years and compactly lose no information in a single variable
  # year_range: "2002",      year: 2003 => "2002-2003"
  # year_range: "2002-2003", year: 2004 => "2002-2004"
  # year_range: "2002-2003", year: 2005 => "2002-2003;2005"
  
  if(length(year_range) == 0){
    return(character(0))
  }
  
  check_arg(year_range, "vector(integer, character) no na l0")
  check_arg(year, "integer scalar GT{1500} LT{2050}")
  
  y_last = as.numeric(str_extract(year_range, -4))
  
  is_increment = year - y_last == 1
  
  res = year_range
  
  # 2003 // 2002      => 2002-2003
  # 2004 // 2002-2003 => 2002-2004
  qui = is_increment
  res[qui] = dsb(".[4k ? year_range[qui]]-.[year]")
  
  # 2003 // 2001      => 2001;2003
  # 2003 // 1999-2001 => 1999-2001;2003
  qui = !is_increment
  res[qui] = dsb(".[year_range[qui]];.[year]")
  
  res
}


aggregate_rows = function(old, new, key, year){
  # old: old data set to which data will be aggregated
  # new: data set to be aggregated
  # key: the name of the variable(s) used as key
  # year: the year to which the data set new refers to: it must be a single year
  
  check_arg(old, new, "class(data.table)")
  check_arg(key, "character scalar") # => if needed I can update code to make it a vector
  check_arg(year, "integer scalar") 
  
  if(!"year_range" %in% names(old)){
    stop("The data set in the argument `old` must contain the variable `year_range`.")
  }
  
  if("year_range" %in% names(new)){
    stop("The data set in the argument `new` must **not** contain the variable `year_range`.")
  }
  
  base_full = rbindlist(list(old[, !"year_range"], new), use.names = TRUE, fill = TRUE)
  
  # Turning character NAs to "" => EXTREMELY important to normalize that, 
  # otherwise we duplicate unnecessarily
  setNA_char(base_full)
  
  # Creating the identifiers
  id = to_integer_fast(base_full[[key]])
  
  vars = setdiff(names(base_full), key)
  arglist = unclass(base_full)[vars]
  id_all_vars = do.call(to_integer_fast, arglist)
  
  # Finding out duplicate rows
  n_old = nrow(old)
  obs_old = 1:n_old
  obs_new = n_old + 1:nrow(new)
  
  base_old = data.table(id = id[obs_old], id_all_vars = id_all_vars[obs_old])
  base_new = data.table(id = id[obs_new], id_all_vars = id_all_vars[obs_new])
  
  info_merge = .imerge(base_old, base_new)
  
  x_matched = old[info_merge$x.matched$obs_]
  x_unmatched = old[info_merge$x.unmatched$obs_]
  y_unmatched = new[info_merge$y.unmatched$obs_]
  
  # we update the year of the ones matched
  x_matched[, year_range := update_year_range(year_range, year)]
  y_unmatched[, year_range := year]
  
  res = rbindlist(list(
    x_matched,
    x_unmatched,
    y_unmatched
  ), use.names = TRUE, fill = TRUE)
  
  setNA_char(res)
  
  res
}

reduce_to_integer = function(int_list){
  # recursive function to transform a list of QUFed integers into a single integer vector
  
  # NOTE:
  # - to be on the safe side in terms of conversion of int64 to numeric, we 
  # use only 52 bits
  
  # number of shifters
  n_shifters_all = ceiling(log(sapply(int_list, max), 2))
  
  # Simple case: total shifters <= 52
  if(sum(n_shifters_all) <= 52){
    agg_value = cpp_multi_integer(int_list)
    res = cpp_to_integer(agg_value)
    
    return(res)
  }
  
  #
  # The general case
  #
  
  # we recursively turn multiple vectors into single QUFed vectors
  
  # new_int_list: reduced vectors of integers
  is_single = c()
  
  batch_list = list()
  batch = c()
  n_shift = 0
  for(i in seq_along(int_list)){
    
    if(n_shifters_all[i] + n_shift > 52){
      # we save the current batch and create the next
      batch_list[[length(batch_list) + 1]] = batch
      
      batch = i
      n_shift = n_shifters_all[i]
      
    } else {
      n_shift = n_shift + n_shifters_all[i]
      batch = c(batch, i)
    }
  }
  
  # we save the last batch
  batch_list[[length(batch_list) + 1]] = batch
  
  # now we turn to integer
  n_batch = length(batch_list)
  new_int_list = vector("list", n_batch)
  for(i in 1:n_batch){
    
    batch = batch_list[[i]]
    if(length(batch) == 1){
      # no need to reput to integer
      is_single = c(is_single, TRUE)
      new_int_list[[i]] = int_list[[batch]]
    } else {
      is_single = c(is_single, FALSE)
      agg_value = cpp_multi_integer(int_list[batch])
      new_int = cpp_to_integer(agg_value)
      new_int_list[[i]] = new_int
    }
  }
  
  if(all(is_single)){
    # We can't rely on the current algorithm
    stop("This case has not yet been implemented.")
  }
  
  # We may reapply the function
  reduce_to_integer(new_int_list)
}


to_integer_fast = function(...){
  # turns vector(s) into a single integer vector
  
  dots = list(...)
  n_dots = length(dots)
  
  if(length(dots) == 0){
    stop("The arguments in `...` must be non empty. Currently there is no vector to transform into integer.")
  }
  
  n_all = lengths(dots)
  if(any(n_all != n_all[1])){
    pblm = which(n_all != n_all[1])
    n_info = fsignif(n_all[pblm])
    stop(dsb("The vectors in input of `to_integer_fast` must all be of the same length. ", 
             "\nPROBLEM: the vector", plural_len(pblm), " ", enumerate_items(pblm), " ", 
             plural_len(pblm, verb = "has"), " a length different from the first vector:\n ", 
             "Lengths: V1: .[fsignif(n_all[1])] vs .[', 'c!V.[pblm]: .[n_info]]"))
  }
  
  for(i in 1:n_dots){
    dots[[i]] = cpp_to_integer(dots[[i]])
  }
  
  if(n_dots == 1){
    return(dots[[1]])
  }
  
  # we take care of aggregating multiple variables in a recursive function to account for the very
  # general case
  res = reduce_to_integer(dots)
  
  res
}


should_fun_be_skipped = function(hard = FALSE){
  # Decide whether a function should be skipped or not
  # based on its output and inputs

  
  if(hard) return(FALSE)
  
  # we find out the input/output
  
  fun = sys.function(sys.parent())
  
  fun_dp = deparse(fun, width.cutoff = 500)
  
  # preprocessing the outfiles
  i_outfile = which(grepl("start_outfile(", fun_dp, fixed = TRUE))
  for(i in i_outfile){
    line = fun_dp[i]
    
    if(!str_is(line, c("=", "\""))){
      stop("Internal error: calls to `start_outfile` should always contain a path",
           " and be assigned to a variable. The following call is not valid:",
           "\n", trimws(line))
    }
    
    var = str_clean(line, "^ +| ?=.+")
    path = str_clean(line, '^[^"]+"|"[^"]+$')
    
    pattern = paste0("(?<![[:alnum:]_.])", var, "(\\$path)?(?![[:alnum:]_.])")
    
    fun_dp = gsub(pattern, paste0('"', path, '"'), fun_dp, perl = TRUE)
    
  }
  
  # Outputs:
  output_dp = str_get(fun_dp, "^[^#]*(write_fst[[:alnum:]_.]*|save)\\(")
  output_files = str_clean(output_dp, c('^[^"]+"', '".*'))
  
  if(length(output_files) == 0 || any(nchar(output_files) == 0)){
    stop("Internal error: output files must be present.")
  }  
  
  for(f in output_files){
    if(!file.exists(f)){
      return(FALSE)
    }
  }
  
  # Inputs
  input_dp = str_get(fun_dp, "^[^#]*(readfst|load)\\(")
  input_files = str_clean(input_dp, c('^[^"]+"', '".*'))
  
  for(f in input_files){
    # We run the code and the error will pop in the right function
    if(!file.exists(f)){
      return(FALSE)
    }
  }
  
  # to make skipping the functions noisy 
  # is_user = sys.nframe() == 2 # before: only end user functions
  is_user = TRUE                # now: always
  
  if(length(input_files) == 0){
    # No input => no need to rerun
    if(is_user){
      proc_hard_msg()
      fun_name = deparse_long(sys.call(sys.parent())[[1]])
      message(dsb("The function `.[fun_name]()` is skipped because the output file", 
                  plural_len(output_files), " already ", 
                  plural_len(output_files, verb = "exist"), ". "))
    }
    
    return(TRUE)
  }
  
  # Deciding based on make times
  output_time = sapply(output_files, file.mtime)
  input_time  = sapply(input_files, file.mtime)
  
  min_output = min(output_time)
  max_input = max(input_time)
  
  # at least one input is posterior to at least one output
  if(max_input > min_output){
    return(FALSE)
  }
  
  if(is_user){
    proc_hard_msg()
    fun_name = deparse_long(sys.call(sys.parent())[[1]])
    message(dsb("The function `.[fun_name]()` is skipped because the output file", 
                plural_len(output_files), " already ", 
                plural_len(output_files, verb = "exist"), " and ", 
                plural_len(output_files, verb = "is"), " posterior to the input file",
                plural_len(input_files,), ". "))
  }
  
  return(TRUE)
  
}

proc_hard_msg = function(){
  
  fun_name = deparse_long(sys.call(sys.parent(2))[[1]])
  current_step = str_op(fun_name, "'step\\d(_[[:upper:]]+)?'x")
  previous_step = getOption("HARD_MESSAGE_STEP", default = "NONE")
  
  SHOW_MSG = current_step != previous_step
  
  now = Sys.time()
  
  if(!SHOW_MSG){
    # we check the timing (if different runs)
    time = getOption("HARD_MESSAGE_TIME", 
                     # the default is a random date
                     default = structure(1676976033.75033, 
                                         class = c("POSIXct", "POSIXt")))
    
    SHOW_MSG = as.difftime(now - time, units = "secs") > 0.5
  }
  
  if(SHOW_MSG){
    message("NOTA: To force the skipped functions to be run, use `hard = TRUE`.")
  }
  
  options(HARD_MESSAGE_STEP = current_step)
  options(HARD_MESSAGE_TIME = now)
  
}


####
#### misc ####
####

deparse_long = function (x) {
  dep_x = deparse(x, width.cutoff = 500)
  if (length(dep_x) == 1) {
    return(dep_x)
  }
  else {
    return(paste(gsub("^ +", "", dep_x), collapse = ""))
  }
}

isVector = function(x){
  
  if(is.atomic(x) && is.null(dim(x))){
    return(TRUE)
  }
  
  return(FALSE)
}

check_set_charin = function(x, choices, choice_name){
  # checks that the values in x belong to choices
  # + expands 'x' with dsb
  
  if(length(x) == 0) return(NULL)
  
  x_nm = names(x)
  
  x_dp = deparse(substitute(x))
  set_up(1)
  
  if(missing(choice_name)) stop("Internal error: you must provide the argument msg.")
  
  if(length(x) == 1){
    x = dsb("S ? x")
    
    if(!is.null(x_nm)){
      if(length(x_nm) == 1){
        x_nm = .dsb("S ? x_nm")
      }
      
      if(length(x_nm) != length(x)){
        stop_up("In the argument `", x_dp, "`, the length of the names does not match ",
                "the length of the vector (", length(x_nm), " vs ", length(x), ").")
      }
    }
  }
  
  pblm = setdiff(x, choices)
  if(length(pblm) > 0){
    stop_up("The argument `", x_dp, "` must be a value of ", choice_name, 
            ". Currently the value", enumerate_items(pblm, "is.quote.s"), " not valid.")
  }
  
  if(!is.null(x_nm) && any(nchar(x_nm) == 0)){
    qui = nchar(x_nm) == 0
    x_nm[qui] = x[qui]
  }
  
  assign(x_dp, x, parent.frame())
}

format_output = function(x, comment = "#> ", tab = 4){
  
  check_arg(x, "character vector")
  check_arg(comment, "character scalar")
  check_arg(tab, "integer scalar GE{1}")
  
  if (missing(x)) {
    x = readClipboard()
  }
  
  xx = strsplit(paste(x, collapse = "\n"), "\n")[[1]]
  new_xx = character(length(xx))
  is_command = FALSE
  n_spaces = NULL
  for (i in seq_along(xx)) {
    line = xx[i]
    
    if (grepl("^> ", line)) {
      is_command = TRUE
      n_spaces = NULL
      new_xx[i] = gsub("^> ", "", line)
      
    } else if (grepl("^\\+ ", line) && is_command) {
      
      if(is.null(n_spaces)) {
        line_prev = xx[i - 1]
        line_prev = gsub(" *#.*$", "", line_prev)
        
        if (grepl("\\{$", line_prev)) {
          n_spaces = tab
          
        } else {
          open = gregexpr("\\(|\\[", line_prev)[[1]]
          
          if (open[1] == -1) {
            n_spaces = tab
            
          } else {
            close = gregexpr("\\)|\\]", line_prev)[[1]]
            if (close[1] == -1) {
              n_spaces = tail(open, 1) - 2
              
            } else {
              w = outer(close, open, "-")
              w[w < 0] = Inf
              ok = max.col(-w)
              n_spaces = open[max(setdiff(1:length(open), ok))] - 2
            }
          }
        }
      }
      
      new_xx[i] = paste0(sprintf("% *s", n_spaces, " "), 
                         gsub("^\\+( |\t)*", "", line))
      
    } else {
      is_command = FALSE
      new_xx[i] = paste0(comment, line)
    }
  }
  
  cat(new_xx, sep = "\n")
}

char2names = function (str, dt = FALSE, sep = "%"){
  
  if (!is.character(str) || length(str) > 1) {
    if (is.data.frame(str)) {
      if (dt) {
        cat(".(", paste0(names(str), collapse = ", "), 
            ")", sep = "")
      } else {
        cat("c(\"", paste0(names(str), collapse = "\", \""), 
            "\")", sep = "")
      }
      return(invisible(NULL))
      
    } else {
      stop("str must be a character string or a data frame.")
    }
  }
  
  str1 = gsub(" ", sep, str)
  str2 = gsub("(\\[[[:digit:]]+\\])|(\n)|(\\+)", "", str1)
  str3 = gsub(paste0("\"", sep, "+\""), "_!_", str2)
  str4 = gsub("(^[[:punct:]]+\")|(^\")", "", str3)
  str5 = gsub("(\".+$)|(\"$)", "", str4)
  str6 = c(strsplit(str5, "_!_"), recursive = TRUE)
  str7 = gsub(sep, " ", str6)
  
  if (dt) {
    cat(".(", paste(str7, collapse = ", "), ")", sep = "")
  } else {
    cat("c(\"", paste(str7, collapse = "\", \""), "\")", 
        sep = "")
  }
}

recode_character = function(code, ...){
  # code: match code. Ex: "FSL?"
  # ...: new_value = old_value
  
  check_arg(code, "character vector no na")
  
  dots = list(...)
  dot_names = names(dots)
  
  if(is.null(dot_names)){
    stop("You must provide argument names in `...`.")
  }
  
  res = character(length(code))
  
  for(i in seq_along(dots)){
    qui = str_is(code, dots[[i]])
    res[qui] = dot_names[i]
  }
  
  if(all(str_is(dot_names, "^\\d+$"))){
    res = as.numeric(res)
  } 
  
  res
}

missnull = function(x){
  missing(x) || is.null(x)
}

####
#### controlled merge ####
####

# INFO:
# - this is a set of function to make merging a safe and informative operation

#' @describeIn cmerge Information on the merge (no display)
.imerge = function(x, y, by = NULL, flag = FALSE, by.x = NULL, by.y = NULL, ...){
  # Everything in ... is discarded
  # It is needed so that it allows to just change the function name to switch from cmerge to imerge
  
  if(missing(x)) stop("Argument 'x' is required but is currently missing.")
  if(missing(y)) stop("Argument 'y' is required but is currently missing.")
  
  x_dp = deparse_long(substitute(x))
  y_dp = deparse_long(substitute(y))
  
  cmerge(x = x, y = y, by = by, by.x = by.x, by.y = by.y,
         # SPECIFIC to this function
         internal = TRUE, x_dp = x_dp, y_dp = y_dp, 
         display = "none", env = parent.frame(),
         flag = flag, no_check = TRUE, fun = "imerge")
}

#' @describeIn cmerge Information on the merge
imerge = function(x, y, by = NULL, flag = FALSE, display = "compact", 
                  legend = NULL, overhead = NULL, by.x = NULL, by.y = NULL, ...){
  # Everything in ... is discarded
  # It is needed so that it allows to just change the function name to switch from cmerge to imerge
  
  if(missing(x)) stop("Argument 'x' is required but is currently missing.")
  if(missing(y)) stop("Argument 'y' is required but is currently missing.")
  
  x_dp = deparse_long(substitute(x))
  y_dp = deparse_long(substitute(y))
  
  cmerge(x = x, y = y, by = by, by.x = by.x, by.y = by.y, display = display,
         legend = legend, overhead = overhead,
         # SPECIFIC to this function
         internal = TRUE, x_dp = x_dp, y_dp = y_dp, flag = flag, 
         env = parent.frame(), no_check = TRUE, fun = "imerge")
}

#' @describeIn cmerge Controlled merge (no display)
.cmerge = function(x, y, by = NULL, 
                   unik = FALSE, unik.x = unik, unik.y = unik, 
                   incl = FALSE, incl.x = incl, incl.y = incl,
                   by.x = NULL, by.y = NULL, all = FALSE,
                   all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE,
                   allow.cartesian = getOption("datatable.allow.cartesian"), ...){
  
  if(missing(x)) stop("Argument 'x' is required but is currently missing.")
  if(missing(y)) stop("Argument 'y' is required but is currently missing.")
  
  x_dp = deparse_long(substitute(x))
  y_dp = deparse_long(substitute(y))
  
  cmerge(x = x, y = y, by = by, unik = unik, unik.x = unik.x, unik.y = unik.y, 
         incl = incl, incl.x = incl.x, incl.y = incl.y, 
         by.x = by.x, by.y = by.y, all = all, all.x = all.x, all.y = all.y, 
         sort = sort, suffixes = suffixes, no.dups = no.dups, 
         allow.cartesian = allow.cartesian,
         # SPECIFIC to this function
         env = parent.frame(), internal = TRUE,
         x_dp = x_dp, y_dp = y_dp, display = "none", 
         ...)
}


#' Controlled merge
#' 
#' Merge data sets while: i) controlling the quality of the merge by checking the unicity of the keys and their inclusion in the data sets, ii) informing on the keys.
#'
#' @param x A data.frame.
#' @param y A data.frame.
#' @param by A character vector or `NULL` (default). The names of the variables used to merge the data sets. If `NULL`, the names in common across the two data sets are used for merging.  
#' @param unik Logical scalar, default is `FALSE`. If `TRUE`, then the keys (variables used for merging) in the two data sets are required to be unique, otherwise an error is thrown. See the arguments `unik.x` or `unik.y` to apply this only to `x` or `y`.
#' @param unik.x Logical scalar, default is `FALSE`. If `TRUE`, then the keys (variables used for merging) of the first data set (`x`) are required to be unique, otherwise an error is thrown.
#' @param unik.y Logical scalar, default is `FALSE`. If `TRUE`, then the keys (variables used for merging) of the second data set (`y`) are required to be unique, otherwise an error is thrown.
#' @param incl Logical scalar, default is `FALSE`. If `TRUE`, then all the keys (variables used for merging) of `x`  are required to be included in `y`, and all the keys of `y` are require to be included in `x`. If this is not the case, an error is thrown. For example, if `x`'s key is `c("a", "b")` and the key of `y` is `c("a", "b", "c")` and error will be thrown because `"c"` is not in the key of `x`. See the arguments `incl.x` or `incl.y` to apply this only to `x` or `y`.
#' @param incl.x Logical scalar, default is `FALSE`. If `TRUE`, then all the keys (variables used for merging) of `x`  are required to be included in `y`. If this is not the case, an error is thrown. For example, if `x`'s key is `c("a", "b")` and the key of `y` is `c("a", "b", "c")`, that's fine. But if the key of `y` is equal to `c("a", "c", "d")` error will be thrown because `x`'s key value `"b"` is not included in `y`'s key.
#' @param incl.y Logical scalar, default is `FALSE`. If `TRUE`, then all the keys (variables used for merging) of `y`  are required to be included in `x`. If this is not the case, an error is thrown. For example, if `y`'s key is `c("a", "b")` and the key of `x` is `c("a", "b", "c")`, that's fine. But if the key of `x` is equal to `c("a", "c", "d")` error will be thrown because `y`'s key value `"b"` is not included in `x`'s key.
#' @param display Character scalar equal to "extensive", "compact" (default) or "none" (note: partial matching is enabled). Controls how the information from the merge is displayed. Just try it.
#' @param legend Logical scalar, default is `FALSE`. If `TRUE` a legend is printed to explain how to read the information displayed. Highly advised for new users.
#' @param overhead Logical, default is `FALSE`. If `TRUE` it displays the overhead incurred by checking the keys.
#' @param by.x Character vector or `NULL`. The keys to be used in the first data set.
#' @param by.y Character vector or `NULL`. The keys to be used in the second data set.
#' @param all Logical, default is `FALSE`. If `TRUE`, all rows of `x` which had no match in `y` are added to the output, the content of the missing values for the `y` variables filled with `NA`s. The rows of `y` that had no match in `x` are added in a similar way. See the arguments `all.x` and `all.y` to enable this behavior for only `x` or `y`.
#' @param all.x Logical, default is `FALSE`. If `TRUE`, all rows of `x` which had no match in `y` are added to the output, the content of the missing values for the `y` variables filled with `NA`s. 
#' @param all.y Logical, default is `FALSE`. If `TRUE`, all rows of `y` which had no match in `x` are added to the output, the content of the missing values for the `x` variables filled with `NA`s. 
#' @param sort Logical, default is `FALSE`. Should be rows of the output data set be sorted with respect to the columns used for merging (`by` columns)?
#' @param suffixes Character vector of length 2, default is `c(".x", ".y")`. If the two data sets `x` and `y` contain columns with similar names but which are not used for merging, this will create a conflict (we can't have two different variables with the same name!). To resolve this conflict, these suffixes are appended to the original variables names. For example say `x` and `y` contain two variables: `"product"` and `"price"`. If only `"product"` is used as a key to merge `x` and `y`, then by default the final data set will contain three variables: `"product"`, `"price.x"` and `"price.y"`.
#' @param no.dups Logical scalar, default is `TRUE`. Linked to the argument `suffixes`, only used if many variables have the same names in data sets `x` or `y`. 
#' @param allow.cartesian Logical scalar, default is `FALSE`. Whether to allow Cartesian matches. That is, if the size of the output data set is expected to be larger than the sum of the sizes of the two data sets, this argument should be TRUE to allow the merging to proceed. Otherwise an error will be thrown (this is to avoid excessively long merges only done by mistake).
#' @param ... arguments to be passed to or from other methods.
#'
#' @return
#' It returns a data set that is the result of the merge.
#'
#' @examples
#' 
#' # data sets 
#' x = data.frame(id = 1:5, age = 25 + 1:5)
#' y = data.frame(id = c(3:6, 3), performance = 1:5)
#' 
#' # by default, it should work with information prompted
#' res = cmerge(x, y, legend = TRUE)
#' 
#' # Let's require that all x keys should be in y: an error will pop
#' try(cmerge(x, y, legend = TRUE, incl.x = TRUE))
#' 
#' # .cmerge does not display any information
#' try(.cmerge(x, y, incl.x = TRUE))
#' 
#' # use imerge to obtain the information on the keys of x not matched
#' info = imerge(x, y) 
#' info$x.unmatched
#' 
#' # if we require the keys of y to be unique: also an error
#' try(cmerge(x, y, legend = TRUE, unik.y = TRUE))
#' 
#' # use .imerge to obtain the information on the duplicated keys (without display)
#' info = .imerge(x, y)
#' info$y.duplicated
#' 
cmerge = function(x, y, by = NULL, 
                  unik = FALSE, unik.x = unik, unik.y = unik, 
                  incl = FALSE, incl.x = incl, incl.y = incl,
                  display = "compact", legend = NULL, overhead = NULL, 
                  by.x = NULL, by.y = NULL, all = FALSE,
                  all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE,
                  allow.cartesian = getOption("datatable.allow.cartesian"), 
                  ...){
  
  # Controlled merge:
  # - error if things are not as expected
  # - same arguments as in data.table
  #
  # Two main suffixes: 
  # - U: key must be unique
  # - I: all the keys must be included in the keys of the other data base
  
  dots = list(...)
  
  ts = Sys.time()
  
  if(isTRUE(dots$debug)){
    gt = function(x){
      ts_new = Sys.time()
      message(sprintf("% 2.2f", as.numeric(ts_new - ts)), "s: ", x)
      ts <<- ts_new
      
    }
  } else {
    gt = function(x) NULL
  }
  
  
  check_arg(unik, unik.x, unik.y, incl, incl.x, incl.y, "logical scalar")
  
  if(missing(x)) stop("Argument 'x' is required but is currently missing.")
  if(missing(y)) stop("Argument 'y' is required but is currently missing.")
  
  # Getting the meta commands
  if(isTRUE(dots$internal)){
    set_up(1)
    x_dp = dots$x_dp
    y_dp = dots$y_dp
    env = dots$env
    IS_ERROR = isTRUE(dots$no_check)
    IS_IMERGE = identical(dots$fun, "imerge")
  } else {
    set_up(0)
    x_dp = deparse_long(substitute(x))
    y_dp = deparse_long(substitute(y))
    env = parent.frame()
    IS_ERROR = FALSE
    IS_IMERGE = FALSE
  }
  
  if(grepl("\\.(U|I|UI|IU)$", x_dp)){
    info = gsub(".*\\.", "", x_dp)
    x_dp = gsub("\\.(U|I|UI|IU)$", "", x_dp)
    unik.x = grepl("U", info)
    incl.x = grepl("I", info)
    
    msg = if(unik.x && incl.x) "U and I suffixes" else if(incl.x) "I suffix" else "U suffix"
    
    x = str2lang(x_dp)
    check_set_value(x, "data.frame evalset", .env = env,
                    .message = paste0("After trailing the ", msg, 
                                      ", the argument 'x' (equal to '", x_dp, 
                                      "') must be the *name* of a data frame."))
  }
  
  if(grepl("\\.(U|I|UI|IU)$", y_dp)){
    info = gsub(".*\\.", "", y_dp)
    y_dp = gsub("\\.(U|I|UI|IU)$", "", y_dp)
    unik.y = grepl("U", info)
    incl.y = grepl("I", info)
    
    msg = if(unik.y && incl.y) "U and I suffixes" else if(incl.y) "I suffix" else "U suffix"
    
    y = str2lang(y_dp)
    check_set_value(y, "data.frame evalset", .env = env, 
                    .message = paste0("After trailing the ", msg, 
                                      ", the argument 'y' (equal to '", y_dp, 
                                      "') must be the *name* of a data frame."))
  }
  
  check_arg(x, y, "data.frame")
  check_arg(by, by.x, by.y, "NULL character vector no na")
  check_arg(all, all.x, all.y, sort, no.dups, "logical scalar")
  check_arg(suffixes, "character vector len(2) no na")
  check_arg(allow.cartesian, legend, overhead, "NULL logical scalar")
  check_set_arg(display, "match(extensive, compact, none)")
  
  opts = getOption("fixest_cmerge")
  if(is.null(legend)) legend = isTRUE(opts$legend)
  if(is.null(overhead)) overhead = isTRUE(opts$overhead)
  
  NO_DISPLAY = display == "none"
  IS_DISPLAY = !NO_DISPLAY
  
  x_nm = names(x)
  y_nm = names(y)
  
  # Handling the `by` argument
  is_by = !is.null(by)
  is_by.x = !is.null(by.x)
  is_by.y = !is.null(by.x)
  if(is_by.x || is_by.y){
    
    if(length(by.x) == 1) by.x = dsb("S, w ? by.x")
    if(length(by.y) == 1) by.y = dsb("S, w ? by.y")
    
    if(is_by.x + is_by.y == 1){
      # We do not use 'by' for 'by.x'
      # The user MUST be explicit
      # error
      if(is_by.x){
        m1 = "by.x"
        m2 = "by.y"
      } else {
        m1 = "by.y"
        m2 = "by.x"
      }
      
      stop_up("If you provide the argument '", m1, "', you must also provide the argument '", 
              m2, "', which is currently missing.")
    } else if(length(by.x) != length(by.y)) {
      stop_up("The argument 'by.x' must be of the same length as 'by.y' (currently ", 
              length(by.x), " vs ", length(by.y), ").")
    }
  } else {
    if(!is_by){
      by = intersect(x_nm, y_nm)
      if(length(by) == 0){
        stop_up("The two data sets don't have any variable with the same name: the key to merge couldn't be identified.\n",
                "Either correct the problem in the data sets, or use the arguments 'by.x' and 'by.y'.")
      }
    } else if(length(by) == 1){
      by = dsb("S, w ? by")
    }
    by.x = by.y = by
  } 
  
  # We check that the values in 'by' are valid
  check_value(by.x, "multi charin", .choices = x_nm, 
              .message = paste0("The argument 'by", ifelse(is_by.x, ".x", ""), 
                                "' must correspond to variables of the first data set."))
  
  check_value(by.y, "multi charin", .choices = y_nm, 
              .message = paste0("The argument 'by", ifelse(is_by.y, ".y", ""), 
                                "' must correspond to variables of the second data set."))
  
  gt("preamble")
  
  time_start = Sys.time()
  
  # All this is required for information purposes
  
  x.keys = lapply(by.x, function(var) x[[var]])
  y.keys = lapply(by.y, function(var) y[[var]])
  
  gt("first data extraction")
  
  if(!all(sapply(x.keys, is.atomic))){
    pblm = which(!sapply(x.keys, is.atomic))
    var_pblm = by.x[pblm]
    stop_up("The function'cmerge' only works when the merge is performed on atomic keys.\n ",
            "In the first data set, the variable", enumerate_items(var_pblm, "s.quote.are"), " not atomic.")
  }
  
  if(!all(sapply(y.keys, is.atomic))){
    pblm = which(!sapply(y.keys, is.atomic))
    var_pblm = by.y[pblm]
    stop_up("The function'cmerge' only works when the merge is performed on atomic keys.\n ",
            "In the second data set, the variable", enumerate_items(var_pblm, "s.quote.are"), " not atomic.")
  }
  
  n_keys = length(by.x)
  n_x = length(x.keys[[1]])
  n_y = length(y.keys[[1]])
  
  # we take care of factors (which would lead to errors otherwise)
  fact_conv = function(v) if(is.factor(v)) as.character(v) else v
  
  full.keys = vector("list", n_keys)
  for(i in 1:n_keys){
    full.keys[[i]] = c(fact_conv(x.keys[[i]]), fact_conv(y.keys[[i]]))
  }
  
  gt("concatenation into a big vector")
  
  # From now on we only manipulate integers, so everything is fast
  single_key = do.call(to_integer, full.keys)
  
  gt("to_integer on the vector")
  
  x.skey = single_key[1:n_x]
  y.skey = single_key[(n_x + 1):(n_x + n_y)]
  
  if(!IS_ERROR && (unik.x && anyDuplicated(x.skey))){
    is_dup = duplicated(x.skey)
    dup_vals = x.skey[is_dup]
    t_dup_vals = table(dup_vals)
    
    # first duplicate
    fdup_id = which(x.skey == dup_vals[1])
    fdup_id_1 = fdup_id[1]
    
    first_keyval = sapply(x.keys, function(v) fact_conv(v)[fdup_id_1])
    fkey = dsb("', 'c !.[by.x]: .[first_keyval]")
    
    if(length(t_dup_vals) == 1){
      extra = paste0("The key of that value is: `", fkey, "`.")
    } else {
      extra = paste0("For example, key `", fkey, "` appears ", n_letter(length(fdup_id)), " times.")
    }
    
    IS_ERROR = TRUE
    ERROR_MSG = paste0("The keys in the first data set (`", x_dp, "`) are not unique. There ", 
                       plural_len(t_dup_vals, "is"),  fsignif(sum(t_dup_vals)), 
                       " duplicate observation", plural(sum(t_dup_vals)), ".\n",
                       extra, " (First dup. obs.: ", fsignif(fdup_id_1), " and ", 
                       fsignif(fdup_id[2]), ".)",
                       "Use `imerge` to get direct information on the duplicates ($x.duplicated).")
    
    if(NO_DISPLAY) stop_up(ERROR_MSG)
  }
  
  if(!IS_ERROR && (unik.y && anyDuplicated(y.skey))){
    is_dup = duplicated(y.skey)
    dup_vals = y.skey[is_dup]
    t_dup_vals = table(dup_vals)
    
    # first duplicate
    fdup_id = which(y.skey == dup_vals[1])
    fdup_id_1 = fdup_id[1]
    
    first_keyval = sapply(y.keys, function(v) fact_conv(v)[fdup_id_1])
    fkey = dsb("', 'c !.[by.y]: .[first_keyval]")
    
    if(length(t_dup_vals) == 1){
      extra = paste0("The key of that value is: `", fkey, "`.")
    } else {
      extra = paste0("For example, key `", fkey, "` appears ", n_times(length(fdup_id)), ".")
    }
    
    IS_ERROR = TRUE
    ERROR_MSG = paste0("The keys in the second data set (`", y_dp, "`) are not unique. There ", 
                       plural_len(t_dup_vals, "is"),  fsignif(sum(t_dup_vals)), 
                       " duplicate observation", plural(sum(t_dup_vals)), ".\n", 
                       extra,  " (First dup. obs.: ", fsignif(fdup_id_1), " and ", 
                       fsignif(fdup_id[2]), ".)",
                       "\nUse `imerge` to get direct information on the duplicates ($y.duplicated).")
    
    if(NO_DISPLAY) stop_up(ERROR_MSG)
  }
  
  gt("unicity checking")
  
  # Note: we always compute the inclusion, even if all.x/all.y is provided
  # OK it's an overhead but it's informative
  # Unless there is no display, in which case this is not needed
  
  if(IS_DISPLAY || (NO_DISPLAY && incl.x) || IS_IMERGE){
    x_in_y = x.skey %in% y.skey
    n_xmatched = sum(x_in_y)
    
    if(!IS_ERROR && (incl.x && !all(x_in_y))){
      is_out = which(!x_in_y)
      out_vals = x.skey[is_out]
      t_out_vals = table(out_vals)
      
      n_out = length(t_out_vals)
      
      # first key
      first_keyval = sapply(x.keys, function(v) fact_conv(v)[is_out[1]])
      fkey = dsb("', 'c !.[by.x]: .[first_keyval]")
      
      if(n_out == 1){
        extra = paste0("This concerns the following key: `", fkey, "`.")
      } else {
        extra = paste0("For example, this concerns the key: `", fkey, "`.")
      }
      
      
      IS_ERROR = TRUE
      ERROR_MSG = paste0("There ", plural(n_out, "is"), " ", fsignif(n_out), 
                         " key", plural(n_out), " from the first ",
                         "data set (`", x_dp, "`) which ", plural(n_out, "is"),
                         " not in the second data set (`", y_dp, "`).\n",
                         extra, " (Obs. ", fsignif(is_out[1]), ".)",
                         "\nUse `imerge` to get direct information on the unmatched keys ($x.unmatched).")
      
      if(NO_DISPLAY) stop_up(ERROR_MSG)
    }
  }
  
  if(IS_DISPLAY || (NO_DISPLAY && incl.y) || IS_IMERGE){
    y_in_x = y.skey %in% x.skey
    n_ymatched = sum(y_in_x)
    
    if(!IS_ERROR && (incl.y && !all(y_in_x))){
      is_out = which(!y_in_x)
      out_vals = y.skey[is_out]
      t_out_vals = table(out_vals)
      
      n_out = length(t_out_vals)
      
      # first key
      first_keyval = sapply(y.keys, function(v) fact_conv(v)[is_out[1]])
      fkey = dsb("', 'c !.[by.y]: .[first_keyval]")
      
      if(n_out == 1){
        extra = paste0("This concerns the following key: `", fkey, "`.")
      } else {
        extra = paste0("For example, this concerns the key: `", fkey, "`.")
      }
      
      
      IS_ERROR = TRUE
      ERROR_MSG = paste0("There ", plural(n_out, "is"), " ", fsignif(n_out), 
                         " key", plural(n_out), " from the second ",
                         "data set (`", y_dp, "`) which ", plural(n_out, "is"), 
                         " not in the first data set (`", x_dp, "`).\n",
                         extra, " (Obs. ", fsignif(is_out[1]), ".)",
                         "\nUse `imerge` to get direct information on the unmatched keys ($y.unmatched).")
      
      if(NO_DISPLAY) stop_up(ERROR_MSG)
    }
  }
  
  gt("inclusion checking")
  
  info_unik_na = NULL
  if(IS_DISPLAY){
    # Information on the merge
    prefix = if(IS_ERROR) "(Would be) " else ""
    
    # Note that we always add a 4th column which may or may not be used
    row_data     = c("", "x", "y", if(display == "compact") "# common" else "")
    row_obs      = c("Obs.", fsignif(n_x), fsignif(n_y), "")
    row_matched  = c(paste0(prefix, "Matched"), 
                     format(n_xmatched, big.mark = ","), 
                     format(n_ymatched, big.mark = ","), "")
    
    row_unmatched = NULL
    if(all.x + all.y < 2){
      row_unmatched  = c(paste0(prefix, "Not Matched"), 
                         if(all.x) "--" else fsignif(n_x - n_xmatched), 
                         if(all.y) "--" else fsignif(n_y - n_ymatched), 
                         "")
    }
    row_keys = c("__KEYS__", "", "", "")
    
    core = rbind(row_data, row_obs, row_matched, row_unmatched, row_keys)
    
    # on the current key:
    info_main_key = compute_unik_exclusive(x.skey, y.skey, keys = by.x, 
                                           display = display, add_attr = TRUE, is_imerge = IS_IMERGE)
    
    gt("exclusive keys")
    
    info_unik_na = attr(info_main_key, "info")
    
    # on the sub keys if needed
    main_data = info_main_key
    if(n_keys > 1){
      info_sub_keys = compute_unik_exclusive_sub(x.keys, y.keys, by.x, display = display)
      
      main_data = rbind(main_data, info_sub_keys)
    }
    
    gt("sub keys")
    
    if(display == "compact"){
      
      fmt = function(s){
        # we align everything at the '('
        s_split = strsplit(s, "(", fixed = TRUE)
        s_left = sapply(s_split, `[[`, 1)
        s_right = sapply(s_split, `[[`, 2)
        
        paste0(s_left, "(", format(s_right, justify = "right"))
      }
      
      main_data[, 2] = fmt(main_data[, 2])
      main_data[, 3] = fmt(main_data[, 3])
    }
    
    mat_show = rbind(core, main_data)
    
    mat_show[, 1] = format(mat_show[, 1])
    
    for(i in 2:3){
      mat_show[, i] = format(mat_show[, i], justify = "right")
    }
    
    mat_show[, 4] = format(mat_show[, 4], justify = if(display == "compact") "right" else "left")
    
    show_vec = apply(mat_show, 1, paste, collapse = " ")
    
    qui_keys = which(grepl("__KEYS__", show_vec, fixed = TRUE))
    row_keys = show_vec[qui_keys]
    row_keys = gsub(" ", "-", row_keys, fixed = TRUE)
    substr(row_keys, 1, 8) = "Keys----"
    show_vec[qui_keys] = row_keys
    
    qui_sub = which(grepl("__SUB__", show_vec, fixed = TRUE))
    row_sub = show_vec[qui_sub]
    row_sub = gsub("[ ()]", "-", row_sub)
    substr(row_sub, 1, 8) = "Sub-Keys"
    show_vec[qui_sub] = row_sub
    
    show_vec = paste0("#> ", show_vec)
    
    extra = NULL
    
    if(!info_unik_na$x.is_unik || !info_unik_na$y.is_unik){
      
      who = c("first", "second")[c(!info_unik_na$x.is_unik, !info_unik_na$y.is_unik)]
      
      msg = dsb("some keys in the .[C ? who] data set.[*s, D ? who] are **not** unique")
      
      msg = c("unicity:", msg)
      extra = rbind(extra, msg)
    }
    
    if(info_unik_na$x.n_na > 0 || info_unik_na$y.n_na > 0){
      
      index = c(info_unik_na$x.n_na > 0, info_unik_na$y.n_na > 0)
      who = c("first", "second")[index]
      nb_na = fsignif(c(info_unik_na$x.n_na, info_unik_na$y.n_na)[index])
      
      msg = dsb("the keys in the .[C ? who] data set.[*s, D ? who] contain NA values (.[C ? nb_na] obs.)")
      
      msg = c("NA:", msg)
      extra = rbind(extra, msg)
    }
    
    if(legend){
      if(display == "extensive"){
        msg = c("key legend:", "number = # of unique keys, X = that are exclusive, C = that are common")
      } else {
        msg = c("legend:", "a (b): a = # of unique keys, b = # of keys that are exclusive")
      }
      
      extra = rbind(extra, msg)
    }
    
    if(overhead){
      msg = c("overhead:", format_difftime(time_start))
      extra = rbind(extra, msg)
    }
    
    if(!is.null(extra)){
      # formatted information
      extra[, 1] = format(extra[, 1], justify = "right")
      extra_vec = apply(extra, 1, paste, collapse = " ")
      extra_vec = paste0("#> ", extra_vec)
      
      # We also add a separator
      show_vec = c(show_vec, "#> -- ", extra_vec)
    }
    
    gt("text processing")
    
    
    merge_info = paste(show_vec, collapse = "\n")
  }
  
  if(IS_IMERGE){
    
    if(IS_DISPLAY) message(merge_info)
    
    if(is.null(info_unik_na)){
      info_main_key = compute_unik_exclusive(x.skey, y.skey, keys = by.x, 
                                             display = display, add_attr = TRUE, is_imerge = TRUE)
      
      info_unik_na = attr(info_main_key, "info")
    }
    
    # We return the matched and non-matched data sets
    # + duplicated and NA
    
    if(isTRUE(dots$flag)){
      res = list(x.match_flag = x_in_y, x.duplicated = info_unik_na$x$which_dup,
                 x.NA = info_unik_na$x$which_na,
                 y.match_flag = y_in_x, y.duplicated = info_unik_na$y$which_dup,
                 y.NA = info_unik_na$y$which_na)
      
    } else {
      
      x_info = extract_matched_unmatched_dup_na(x, x_in_y, by.x, info_unik_na$x)
      y_info = extract_matched_unmatched_dup_na(y, y_in_x, by.y, info_unik_na$y)
      
      res = list(x.matched = x_info$matched, x.unmatched = x_info$unmatched,
                 x.duplicated = x_info$dup, x.NA = x_info$na,
                 y.matched = y_info$matched, y.unmatched = y_info$unmatched,
                 y.duplicated = y_info$dup, y.NA = y_info$na)
    }
    
    return(res)
  }
  
  if(IS_ERROR){
    stop_up(ERROR_MSG, msg = c("\nFYI, here is what the merge would have looked like:\n", 
                               merge_info))
  }
  
  res = try(merge(x, y, by.x = by.x, by.y = by.y, all.x = all.x, all.y = all.y, 
                  sort = sort, suffixes = suffixes, no.dups = no.dups, 
                  allow.cartesian = allow.cartesian))
  
  gt("merging")
  
  if(inherits(res, "try-error")){
    stop_up("Error during the merge:\n", res, 
            msg = c("\nFYI here is what the outcome of the merge should have been:\n",
                    merge_info))
  }
  
  if(IS_DISPLAY){
    message(merge_info)
  }
  
  res
}

compute_unik_exclusive_sub = function(x_list, y_list, keys, display){
  
  n_x = length(x_list[[1]])
  n_y = length(y_list[[1]])
  
  n_keys = length(keys)
  res_all = vector("list", n_keys)
  for(i in 1:n_keys){
    xi = x_list[[i]]
    yi = y_list[[i]]
    
    xy_int = to_integer(c(xi, yi))
    xi_int = xy_int[1:n_x]
    yi_int = xy_int[(n_x + 1):(n_x + n_y)]
    
    res_all[[i]] = compute_unik_exclusive(xi_int, yi_int, keys[i], display)
  }
  
  res = do.call(rbind, res_all)
  
  info_row = if(display == "compact") c("__SUB__", "()", "()", "") else c("__SUB__", "", "", "")
  res = rbind(info_row, res)
  
  res
}



compute_unik_exclusive = function(x, y, keys, display = "extensive", add_attr = FALSE, is_imerge = FALSE){
  # how many value of x (y) are unique?
  # how many values of x (y) are in y (x)?
  # add_attr: information on whether the keys are unique + NA information
  
  x_int_all = to_integer(x, add_items = TRUE, items.list = TRUE)
  x_int = x_int_all$x
  x_items = x_int_all$items
  
  y_int_all = to_integer(y, add_items = TRUE, items.list = TRUE)
  y_int = y_int_all$x
  y_items = y_int_all$items
  
  n_unik.x = length(x_int_all$items)
  n_unik.y = length(y_int_all$items)
  
  x_excl = x_items[!x_items %in% y_items]
  n_excl.x = length(x_excl)
  
  y_excl = y_items[!y_items %in% x_items]
  n_excl.y = length(y_excl)
  
  n_common = n_unik.x - n_excl.x
  
  if(display == "extensive"){
    row_key_unik = c(paste0(keys, collapse = ":"), fsignif(n_unik.x), fsignif(n_unik.y), "")
    row_key_excl = c("", fsignif(n_excl.x), fsignif(n_excl.y), "-> X")
    row_key_com = c("", fsignif(n_common), fsignif(n_common), "-> C")
    
    res = rbind(row_key_unik, row_key_excl, row_key_com)
  } else {
    info_x = paste0(fsignif(n_unik.x), " (", fsignif(n_excl.x), ")")
    info_y = paste0(fsignif(n_unik.y), " (", fsignif(n_excl.y), ")")
    
    res = rbind(c(paste0(keys, collapse = "-"), info_x, info_y, fsignif(n_common)))
  }
  
  if(add_attr){
    info = list()
    
    x.which_na = integer(0)
    if(anyNA(x_int)){
      x.which_na = which(is.na(x_int))
      info$x.n_na = length(x.which_na)
    } else {
      info$x.n_na = 0
    }
    
    y.which_na = integer(0)
    if(anyNA(y_int)){
      y.which_na = which(is.na(y_int))
      info$y.n_na = length(y.which_na)
    } else {
      info$y.n_na = 0
    }
    
    # We only add the information on unicity, the message will be created in the function
    info$x.is_unik = (n_unik.x + info$x.n_na) == length(x)
    info$y.is_unik = (n_unik.y + info$y.n_na) == length(y)
    
    # We add extra information for imerge
    if(is_imerge){
      
      info_x = info_y = list()
      
      # Info on NAs
      info_x$which_na = x.which_na
      info_y$which_na = y.which_na
      
      # We include the observations that ARE duplicated (and non NA)
      
      if(!info$x.is_unik){
        dup = unique(x_int[duplicated(x_int)])
        if(info$x.n_na > 0){
          dup = dup[!is.na(dup)]
        }
        
        which_dup = x_int %in% dup
        dup_order = order(x_int[which_dup])
        
        info_x$which_dup = which_dup
        info_x$dup_order = dup_order
      } else {
        info_x$which_dup = info_x$dup_order = integer(0)
      }
      
      
      if(!info$y.is_unik){
        dup = unique(y_int[duplicated(y_int)])
        if(info$y.n_na > 0){
          dup = dup[!is.na(dup)]
        }
        
        which_dup = y_int %in% dup
        dup_order = order(y_int[which_dup])
        
        info_y$which_dup = which_dup
        info_y$dup_order = dup_order
      } else {
        info_y$which_dup = info_y$dup_order = integer(0)
      }
      
      info$x = info_x
      info$y = info_y
      
    }
    
    attr(res, "info") = info
  }
  
  res
}

extract_matched_unmatched_dup_na = function(x, x_in_y, by.x, info){
  # x is the original data set
  # by.x the keys used to match
  # info: list of information on duplicated and NAs
  
  x_nm = names(x)
  n_x = nrow(x)
  
  x_clean = unclass(x)
  x_clean = c(list(1:n_x), x_clean[by.x], x_clean[setdiff(x_nm, by.x)])
  
  x_nm_new = c("obs_", by.x, setdiff(x_nm, by.x))
  
  #
  # Matched and unmatched
  
  qui_x.matched = which(x_in_y)
  x_matched = lapply(x_clean, function(v) v[qui_x.matched])
  if(length(qui_x.matched) == 0){
    x_unmatched = x_clean
  } else {
    x_unmatched = lapply(x_clean, function(v) v[-qui_x.matched])
  }
  
  # 
  # duplicated & NA
  
  x_dup = lapply(x_clean, function(v) v[info$which_dup][info$dup_order])
  x_NA = lapply(x_clean, function(v) v[info$which_na])
  
  names(x_matched) = names(x_unmatched) = x_nm_new
  names(x_dup) = names(x_NA) = x_nm_new
  
  # as.data.frame always exist, so we know we'll fall back onto it
  for(my_class in class(x)){
    as_fun = paste0("as.", my_class)
    if(exists(as_fun, mode = "function")){
      x_matched = eval(str2lang(paste0(as_fun, "(x_matched)")))
      x_unmatched = eval(str2lang(paste0(as_fun, "(x_unmatched)")))
      x_dup = eval(str2lang(paste0(as_fun, "(x_dup)")))
      x_NA = eval(str2lang(paste0(as_fun, "(x_NA)")))
      break
    }
  }
  
  if(!inherits(x, "data.table")){
    # We add the appropriate row names
    row_nm = row.names(x)
    row.names(x_matched) = row_nm[qui_x.matched]
    
    if(length(qui_x.matched) == 0){
      row.names(x_unmatched) = row_nm
    } else {
      row.names(x_unmatched) = row_nm[-qui_x.matched]
    }
    
    row.names(x_dup) = row_nm[info$which_dup][info$dup_order]
    row.names(x_NA) = row_nm[info$which_na]
  } 
  
  list(matched = x_matched, unmatched = x_unmatched, dup = x_dup, na = x_NA)
}



format_difftime = function(x){
  # x: number of seconds or difftime or time
  
  if(inherits(x, "POSIXt")){
    x = Sys.time() - x
  }
  
  if(inherits(x, "difftime")){
    x = as.double(x, units = "secs")
  }
  
  if(x > 60){
    n_min = x %/% 60
    rest_s = floor(x %% 60)
    res = paste0(fsignif(n_min), " min ", sprintf("%02i", rest_s), " sec")
  } else if(x < 0.1){
    res = "< 0.1s"
  } else {
    res = paste0(fsignif(x, 2, 1), "s")
  }
  
  res
}


####
#### Name manipulation ####
####

# Hard values:
PARTICLES = "de|du|le|la|st|mac|mc|opde|van|von|vom|zur|im|dem|vande|vanden|vander|ten|ter|af|di|da|dal|della|delos|delas|dela|del|das|dos|el|al|ben"

concat_large_particle = function(x){
  # van den => vanden, etc
  # Note that I do not use \\b for word limitation because of accentuated characters 
  
  from_to = c("(^| )van de(r|n)?( |$)" = "vande\\1",                 
              "(^| )op de( |$)" = "opde",
              "(^| )de l(o|a)(s)?( |$)" = "del\\1\\2")
  
  res = x
  for(i in seq_along(from_to)){
    from = names(from_to)[i] 
    to = from_to[i]
    res = gsub(from, to, res, perl = TRUE)
  }
  
  res
}

merge_particle = function(x, concat = TRUE, ben = TRUE){
  # concat: whether to concatenate multi-word particles
  # ben can be a valid first name, in some contexts, we may not want to merge it
  
  if(!ben){
    PARTICLES = gsub("|ben", "", PARTICLES, fixed = TRUE)
  }
  
  if(concat){
    res = concat_large_particle(x)
  } else {
    res = x
  }
  
  gsub(paste0("(^| )(", PARTICLES, ") "), "\\1\\2_", res, perl = TRUE)
}


is_valid_swedish_name = function(name_vec){
  # This functions looks at a character string and erases it if it does not correspond to a swedish name
  
  # valid first names, will be used to detect the first names in the last comma section
  swedish_first_names = readfst("_DATA/swedish_first_names.fst")
  ok_name = swedish_first_names[freq >= 10, name_ascii]
  
  # We try to find the real first names
  base_split = strsplit2df(name_vec, split = " |-", addPos = TRUE)
  setDT(base_split)
  base_split[, is_name := cleanCharVector(x, forceASCII = TRUE) %in% ok_name]
  base_split[, is_abbrev := grepl("^[[:alpha:]]\\.?$", x)]
  base_split[, is_ok := is_name | is_abbrev]
  
  base_info = base_split[, .(x = paste(x, collapse = " "), 
                             n_ok = sum(is_ok), n = .N, first_ok = sum(is_ok & pos == 1)), keyby = id]
  
  # sort(table(base_info[n == 1 & n_ok == 0, x]))
  # sort(table(base_info[n == 2 & n_ok == 1, x]))
  # sort(table(base_info[n == 3 & n_ok == 2, x]))
  not_a_name = dsb("/jr, ii, iii, dr, dr.")
  
  id_ok = base_info[(n == 1 & n_ok == 1 & !x %in% not_a_name) | 
                      (n == 2 & n_ok == 1 & first_ok) | 
                      (n == 3 & n_ok == 2 & first_ok), id]
  
  seq_along(name_vec) %in% id_ok
}

freq_swedish_name = function(name_vec){
  swedish_first_names = readfst("_DATA/swedish_first_names.fst")
  name_vec_ascii = cleanCharVector(name_vec, forceASCII = TRUE)
  
  freq_vec = setNames(swedish_first_names$freq, swedish_first_names$name)
  res = freq_vec[name_vec_ascii]
  res[is.na(res)] = 0
  as.vector(res)
}


name_str_to_name_DB = function(x, prefix, sep = " ", nmax = 3){
  # "julien pierre lesieur", prefix = "name_"
  #
  # id | name_1 | name_2 |  name_3
  # ---|--------|--------|-------- #
  #  1 | julien | pierre | lesieur
  #
  
  check_arg(x, "character vector mbt")
  check_arg(prefix, "character scalar mbt")
  check_arg(sep, "character scalar")
  check_arg(nmax, "integer scalar ge{2}")
  
  base_names_raw = strsplit2df(x, split = sep, addPos = TRUE)
  
  base_names = base_names_raw[pos == 1, .(id, x)]
  setnames(base_names, "x", paste0(prefix, 1))
  
  if(nmax >= 2){
    for(i in 2:nmax){
      bae_names_i = base_names_raw[pos == i, .(id, x)]
      base_names = merge(base_names, bae_names_i, by = "id", all.x = TRUE)
      base_names[is.na(base_names$x), x := ""]
      setnames(base_names, "x", paste0(prefix, i))
    }
  }
  
  base_names
}


dup_data_change_letter = function(data, letter_dict, vars){
  # we duplicate the data set for each value in vars that contain the letter
  # we make the replacement and turn everything into ascii
  
  check_arg(data, "data.frame mbt")
  check_arg(letter_dict, "named character vector len(1) mbt")
  check_arg(vars, "character scalar")
  
  name_vars = str_get(names(data), vars)
  if(length(name_vars) == 0){
    stop(dsb("The argument `vars` (equal to `.[vars]` which must be a regular ",
             "expression) did not match any variable from `data`."))
  }
  
  letter = names(letter_dict)
  replacement = letter_dict
  
  qui = logical(nrow(data))
  
  # that's pretty slow
  for(v in name_vars){
    qui = qui | grepl(letter, data[[v]], fixed = TRUE)
  }
  
  data_sub = data[qui == TRUE]
  
  # now the replacement
  for(v in name_vars){
    x = gsub(letter, replacement, data_sub[[v]], fixed = TRUE)
    x = cleanCharVector(x, forceASCII = TRUE)
    set(data_sub, j = v, value = x)
  }
  
  data_sub
}

complete_with_next = function(id, x){
  
  check_arg(id, "vector no na")
  check_arg(x, "character vector no na")
  
  id_next = c(id[-1], -1)
  
  while(TRUE){
    
    nc = nchar(x)
    fl = substr(x, 1, 1)
    
    nc_next = c(nc[-1], -1)
    fl_next = c(fl[-1], "")
    
    qui = which(id == id_next & nc %in% c(0, 1) & nc_next > 1 & fl == fl_next)
    
    if(length(qui) == 0){
      break
    } else {
      # message(length(qui))
      x[qui] = x[qui + 1]
    }
    
  }
  
  x
}

extract_name_patents = function(all_names_raw, id_name = "obs", silent = FALSE){
  
  check_arg(all_names_raw, "character vector no na")
  check_arg(id_name, "character scalar")
  
  
  start = Sys.time()
  
  # Minor treatment
  from_to = c(";" = ", ",                                        # regular separator
              "\\b([[:alpha:]])\\.([[:alpha:]])\\b" = "\\1 \\2", # "a.k." => "a k."
              "@.*" = "",                                        # always at the end (ex: "whitaker, derek@tms sweden ab")
              "c/o.*" = "",                                      # always at the end (ex: eriksson, ulf, c/o the ludwig inst.cancer research)
              "'" = "",                                          # we "attach" single quotes: o'conor => oconor
              "\\ba\\.b\\.\\b" = "ab",                           # we'll later use "ab" to detect companies
              "aktiebolag(et)?" = "ab",
              "(hewlett-packard|astrazeneca).*" = "",            # a couple of companies directly in the name
              # particles
              "\\bvan de(r|n)?\\b" = "vande\\1",                 # van der, van den, van de => vande_
              "\\bop de\\b" = "opde",
              "\\bde l(o|a)(s)?\\b" = "del\\1\\2"                # de los, de las, de la => del__
  )
  
  all_names = all_names_raw
  for(i in seq_along(from_to)){
    from = names(from_to)[i] ; to = from_to[i]
    all_names = gsub(from, to, all_names, perl = TRUE)
  }
  
  
  # We first extract the  family names which are (almost always) the first element
  family_names_raw = gsub(",.+", "", all_names, perl = TRUE)
  names_rest = gsub("^[^,]+, ?", "", all_names, perl = TRUE)
  
  #
  # Special case for the names without any comma: we have to find out which is the family name (424...)
  #
  
  qui_no_comma = !grepl(",", all_names, fixed = TRUE)
  if(any(qui_no_comma)){
    name_NC = all_names[qui_no_comma]
    base_NC = strsplit2df(name_NC, split = " |-", addPos = TRUE, id = which(qui_no_comma))
    base_NC[, freq := freq_swedish_name(x)]
    base_NC[, is_valid := +(freq > 10)]
    base_NC[, n_valid := sum(is_valid), by = id]
    base_NC[, n := .N, by = id]
    base_NC[, min_freq := min(freq), by = id]
    
    # case with a clear last name
    base_NC_partial = base_NC[n_valid < n]
    base_NC_partial_firstname = base_NC_partial[is_valid == 1, .(first = paste(x, collapse = " ")), by = id]
    base_NC_partial_lastname = base_NC_partial[is_valid == 0, .(last = paste(x, collapse = " ")), by = id]
    tmp = merge(base_NC_partial_firstname, base_NC_partial_lastname, by = "id", all = TRUE)
    family_names_raw[tmp$id] = tmp$last
    names_rest[tmp$id] = tmp$first
    
    # case with most likely last name
    base_NC_all_valid = base_NC[n_valid == n]
    base_NC_all_valid_firstname = base_NC_all_valid[freq > min_freq, .(first = paste(x, collapse = " ")), by = id]
    base_NC_all_valid_lastname = base_NC_all_valid[freq == min_freq, .(last = paste(x, collapse = " ")), by = id]
    tmp = merge(base_NC_all_valid_firstname, base_NC_all_valid_lastname, by = "id", all = TRUE)
    family_names_raw[tmp$id] = tmp$last
    names_rest[tmp$id] = tmp$first
  }
  
  # Objective: get all the first names (and first names only) space separated
  
  # We split the first names into a first and a second item
  names_rest = gsub(";", ",", names_rest, fixed = TRUE)
  first_item = gsub(",.+", "", names_rest)
  
  # the rest of the first names after we've dropped the 1st element before the first comma
  firstname_rest = character(length(names_rest))
  qui_comma = grepl(",", names_rest)
  firstname_rest[qui_comma] = gsub("^[^,]+, ?", "", names_rest[qui_comma])
  
  # we split by parts, where a part is a comma separated item
  if(any(firstname_rest != "")){
    base_parts = strsplit2df(firstname_rest, split = ", ?", addPos = TRUE)
    base_parts[, is_valid := is_valid_swedish_name(x)]
    
    # for each full_name, we keep the parts that are valid. We stop at the first non valid
    base_parts[, cum_pos := cumsum(pos), keyby = id]
    base_parts[, cum_pos_valid := cumsum(pos * is_valid), keyby = id]
    
    base_parts = base_parts[cum_pos_valid == cum_pos]
    
    value_item = base_parts[, .(x = paste(x, collapse = " ")), by = id]
    
    second_item = character(length(names_rest))
    second_item[value_item$id] = value_item$x
  } else {
    second_item = ""
  }
  
  
  # The vector of all space separated first names
  first_names_all = trimws(paste(first_item, second_item))
  
  # we clean a few remaining artefacts
  first_names_all = str_clean(first_names_all, 
			      paste0("\\b", c("mr\\.?", "ms\\.?", "dr\\.?", "dipl\\.-ing\\.", "\\+"), "\\b"))
  
  # we clean the '.' in the initials
  first_names_all = gsub("(?<=[[:alpha:]])\\.", "", first_names_all, perl = TRUE)
  
  # particles (note that ben, in the name, is a particle. I don't put it here)
  part = gsub("|ben", "", PARTICLES, fixed = TRUE)
  qui_particle = grepl(paste0("[^\\p{L}-](", PARTICLES, ")$"), 
                       first_names_all, perl = TRUE)
  if(any(qui_particle)){
    FN_particle = first_names_all[qui_particle]
    part = str_clean(FN_particle, ".+ ")
    FN_particle = str_clean(FN_particle, " [^ ]+$")
    first_names_all[qui_particle] = FN_particle
    # we add this to the family name
    family_names_raw[qui_particle] = paste(part, family_names_raw[qui_particle])
  }
  
  # we remove the "-" in names (too unstable)
  first_names_all = gsub("-", " ", first_names_all, fixed = TRUE)
  
  
  # we take care of the companies
  qui_company = grepl("\\bab\\b", first_names_all, perl = TRUE)
  FN_comapny = first_names_all[qui_company]
  # we keep the first first name for them
  FN_comapny = gsub(" .+", "", FN_comapny, perl = TRUE)
  first_names_all[qui_company] = FN_comapny
  
  #
  # data base
  #
  
  #
  # first names (all ASCII)
  
  FN_clean = cleanCharVector(first_names_all, forceASCII = TRUE, cleanPunct = TRUE)
  
  base_first_names_raw = strsplit2df(FN_clean, split = " ", addPos = TRUE)
  
  base_first_names = base_first_names_raw[pos == 1, .(id, first_name_1 = x)]
  
  FN_2 = base_first_names_raw[pos == 2, .(id, first_name_2 = x)]
  FN_3 = base_first_names_raw[pos == 3, .(id, first_name_3 = x)]
  
  tmp = merge(base_first_names, FN_2, by = "id", all.x = TRUE)
  tmp2 = merge(tmp, FN_3, by = "id", all.x = TRUE)
  base_first_names = tmp2
  
  base_first_names[is.na(first_name_2), first_name_2 := ""]
  base_first_names[is.na(first_name_3), first_name_3 := ""]
  
  
  #
  # family names (all ASCII)
  
  family_names = family_names_raw
  
  # Actions:
  # - remove the "-" from multiple names
  # - tie the particles to the name
  
  # the "-" + other non letter + single letters
  family_names = gsub("-", " ", family_names, fixed = TRUE)
  family_names = cleanCharVector(family_names, forceASCII = TRUE, 
                                 cleanPunct = TRUE, cleanOneLetter = TRUE)
  
  family_names = merge_particle(family_names, concat = FALSE)
  
  #
  # the data base
  
  base_family_names_raw = strsplit2df(family_names, split = " ", addPos = TRUE)
  
  base_family_names = base_family_names_raw[pos == 1, .(id, fam_name_1 = x)]
  
  LN_2 = base_family_names_raw[pos == 2, .(id, fam_name_2 = x)]
  LN_3 = base_family_names_raw[pos == 3, .(id, fam_name_3 = x)]
  
  tmp = merge(base_family_names, LN_2, by = "id", all.x = TRUE)
  tmp2 = merge(tmp, LN_3, by = "id", all.x = TRUE)
  base_family_names = tmp2
  
  base_family_names[is.na(fam_name_2), fam_name_2 := ""]
  base_family_names[is.na(fam_name_3), fam_name_3 := ""]
  
  #
  # FULL NAMES
  
  base2merge = data.table(id = seq_along(all_names), name_raw = all_names)
  tmp  = merge(base2merge, base_first_names, by = "id", all = TRUE)
  tmp2 = merge(tmp, base_family_names, by = "id", all = TRUE)
  stopifnot(nrow(tmp2) == length(all_names))
  base_inv_names = tmp2
  
  setnames(base_inv_names, "id", id_name)
  
  # sorting out the last problems
  qui_no_FN = is.na(base_inv_names$first_name_1)
  if(any(qui_no_FN)){
    base_inv_names[qui_no_FN, first_name_1 := fam_name_2]
    base_inv_names[qui_no_FN, fam_name_2 := ""]
  }
  
  setNA(base_inv_names, "^first, ^fam", "")
  
  if(!silent) message(fsignif(nrow(base_inv_names)), " names parsed in ", format_difftime(start), ".")
  
  base_inv_names
}

is_abbrev = function(x, y){
  # Whether one string is the abbreviation of the other
  # ex: "n west" is the abbreviation of "north west" => will return 1
  # Only one abbreviation is tolerated and it requires at least two words
  
  check_arg(x, "character vector")
  check_arg(y, "character vector len(data)", .data = x)
  
  qui = grepl("\\b[[:alpha:]]\\b", x, perl = TRUE) | grepl("\\b[[:alpha:]]\\b", y, perl = TRUE)
  
  x_init = gsub("\\b([[:alpha:]])[[:alpha:]]*", "\\1", x)
  y_init = gsub("\\b([[:alpha:]])[[:alpha:]]*", "\\1", y)
  
  qui_same_init = x_init == y_init & qui
  
  if(!any(qui_same_init)){
    return(+qui_same_init)
  }
  
  x_same_init = x[qui_same_init]
  y_same_init = y[qui_same_init]
  
  x_split = strsplit(x_same_init, " ")
  y_split = strsplit(y_same_init, " ")
  
  id = rep(1:length(x_same_init), lengths(x_split))
  x_words = unlist(x_split)
  y_words = unlist(y_split)
  
  is_different_word = x_words != y_words
  # Max 1 different word (the abbreviation)
  dist = tapply(is_different_word, id, sum)
  
  res = rep(0, length(x))
  res[qui_same_init] = +(dist == 1)
  
  res
}


clean_employer_name_se = function(x, do_unik = NULL){
  # we clean non-character things
  
  check_arg(x, "character vector")
  check_arg(do_unik, "logical scalar")
  
  n = length(x)
  if(is.null(do_unik)) do_unik = n > 1e6
  
  if(do_unik){
    x_int = cpp_to_integer(x)
    x_small = x[!duplicated(x_int)]
    
    res_small = clean_employer_name_se(x_small, do_unik = FALSE)
    res = res_small[x_int]
    return(res)
  } 
  
  # we transform aktiebolag into ab and place it in the end
  x_clean = str_op(x, "'\\([^)]*\\)'R, '/'r, 'aktiebolag(et)? => ab'R, W, w")
  qui_ab = str_is(x_clean, "\\bab\\b")
  x_ab = x_clean[qui_ab]
  x_ab = str_op(x_ab, "'\\bab\\b'R, '| ab'a, w", do_unik = FALSE)
  # new string.ops: 
  #      str_op(x_ab, "'\\bab\\b'R, ' ab'ar, w", do_unik = FALSE)
  x_clean[qui_ab] = x_ab
  
  # we join single letters
  x_clean = gsub("\\b(\\w) \\b(?=\\w\\b)", "\\1", x_clean, perl = TRUE)
  
  x_clean
}


make_pattern = function(x, dict = NULL, accent = FALSE){
  # . is one letter
  # _ is 2+ letters
  # d is 2+ digits
  #
  # \\p{L} matches any unicode letter
  
  if(is.null(dict)){
    if(accent){
      dict = c("[\\p{L}]{2,}" = "_",
               "[\\p{L}]" = ".",
               "[[:digit:]]+" = "d")
    } else {
      dict = c("[[:alpha:]]{2,}" = "_",
               "[[:alpha:]]" = ".",
               "[[:digit:]]+" = "d")
    }
  }
  
  res = x
  for(i in seq_along(dict)){
    pat = names(dict)[i]
    res = gsub(pat, dict[i], res, perl = TRUE)
  }
  
  res
}



####
#### bilateral variables ####
####

tech_sim = function(x, y, cuts = c(1, 3, 4, 7)){
  # compares technology codes:
  # H03M007 & H04L012 => 1
  # H04H060 & H04L012 => 3
  check_arg(x, "character vector no na")
  check_arg(y, "character vector no na len(data)", .data = x)
  check_arg(cuts, "integer vector no na")
  
  n = length(x)
  res = numeric(n)
  
  for(k in cuts){
    qui = substr(x, 1, k) == substr(y, 1, k)
    res[qui] = k
  }
  
  res
}


####
#### Address extraction ####
####

extract_city = function(x){
  str_op(x, "'[[:digit:]]=> 'R, W, w")
}

extract_street_name = function(x){
  str_op(x, "' ?[[:digit:]].*'R, W, w")
}

extract_street_nb = function(x){
  # The first digit
  str_op(x, "'[[:digit:]]+'x, '^0+'R")
}

extract_building = function(x){
  # Only works if the pattern is "street_name street_nber building"
  # "anything non numeric 55 a 32 lgh" will lead to "a"
  # "anything non numeric 55a 32 lgh" will also lead to "a"
  str_op(x, "'^[^[:digit:]]+[[:digit:]]+ ?[[:alpha:]]\\b'x, '^[^[:digit:]]+[[:digit:]]+ ?'R")
}

street_clean_companies = function(x){
  # ex: "volvo ab goranstgatan" => "goranstgatan"
  # healthcare => ge healthcare
  gsub("^.+(\\bab\\b|s:ta?|r ?& ?d|healthcare) ?", "", x)
}


extract_se_address_patents = function(address_raw, box_valid = FALSE){
  # address_raw: the raw address string
  # It works only for SE addresses!!! 
  # I should have one function like that for each country
  # box: whether to consider box as a valid address
  
  check_arg(address_raw, "character vector")
  check_arg(box_valid, "logical scalar")
  
  time_start = Sys.time()
  
  address_raw_origin = address_raw
  
  address_int = cpp_to_integer(address_raw)
  
  qui_dup = duplicated(address_int)
  address_raw = address_raw[!qui_dup]
  address_raw = str_clean(address_raw, ", *$")
  
  last_item = str_op(address_raw, "',[^,]+$'x")
  last_item = str_clean(last_item, c("^, *", "^s( |-)", "^se?(?=[^[:alpha:]])", "^[ -]+"))
  
  # normalization
  qui_hyphen = which(grepl("-", last_item, fixed = TRUE))
  if(length(qui_hyphen) > 0){
    last_no_hyphen = str_clean(last_item[qui_hyphen], "\\b[[:alpha:]]{1,2}-")
    last_no_hyphen = str_op(last_no_hyphen, "W, w")
    last_item[qui_hyphen] = last_no_hyphen
    
    # we need to add a last comma, will be used later on (it is assumed the last comma is the default)
    tmp = address_raw[qui_hyphen]
    tmp = substr(tmp, 1, nchar(tmp) - nchar(last_no_hyphen))
    tmp = str_clean(tmp, "( s?e?|)-$")
    address_raw[qui_hyphen] = paste0(tmp, ",")
  }
  
  qui_no_last_item = which(nchar(last_item) == 0)
  if(length(qui_no_last_item) > 0){
    pblm = address_raw[qui_no_last_item]
    sol = dsb("' se?-[[:digit:] ]+[[:alpha:]]+$'x ? pblm")
    last_item[qui_no_last_item] = sol
    qui_pblm_solved = nchar(sol) > 0
    if(any(qui_pblm_solved)){
      tmp = address_raw[qui_no_last_item][qui_pblm_solved]
      address_raw[qui_no_last_item][qui_pblm_solved] = substr(tmp, 1, nchar(tmp) - nchar(sol[qui_pblm_solved]))
    }
  }
  
  
  postcode = gsub("[^[:digit:]]", "", last_item)
  city = gsub("^[[:digit:] ]+", "", last_item)
  
  # We tackle the missing postcodes
  qui_postcode0 = which(nchar(postcode) == 0)
  if(length(qui_postcode0) > 0){
    last_item0 = str_op(address_raw[qui_postcode0], "'[[:digit:] ]{5,}.+$'x")
    postcode0 = str_op(last_item0, "'[[:alpha:] ,-]+$'R, '[[:digit:] ]+$'x, ' 'R")
    city0 = str_op(last_item0, "', ?se$'R, '[[:alpha:] -]+$'x")
    
    last_item[qui_postcode0] = last_item0
    postcode[qui_postcode0] = postcode0
    qui_city_ok = nchar(city0) > nchar(city[qui_postcode0])
    city[qui_postcode0][qui_city_ok] = city0[qui_city_ok]
  }
  
  
  # we tackle missing cities
  qui_city0 = which(nchar(city) == 0)
  if(length(qui_city0) > 0){
    ante_last_item = str_clean(address_raw[qui_city0], c(",[^,]*$", ".+,"))
    city0 = str_op(ante_last_item, "'[[:alpha:] -]+$'x")
    
    qui_city_ok = nchar(city0) > 0
    city[qui_city0][qui_city_ok] = city0[qui_city_ok]
  }
  
  # Now we extract the full street
  street_full = gsub(",[^,]*$", "", address_raw, perl = TRUE)
  street_full[qui_no_last_item] = ""
  
  # input: street_full: character vector of the full street information
  # output: data.table with 4 columns: street name, street number, building, company flag
  
  n = length(street_full)
  street_name = street_nb = building = character(n)
  
  is_company = grepl("\\b(ab|c/o|r ?& ?d|university|univ\\.)\\b", street_full)
  
  is_comma = grepl(",", street_full, fixed = TRUE)
  
  #
  # without comma
  #
  
  # NC = no comma
  street_NC = street_full[!is_comma]
  
  # By default, we take all the words coming before the first number
  street_NC_name = extract_street_name(street_NC)
  street_NC_nb = extract_street_nb(street_NC)
  building_NC = extract_building(street_NC)
  
  # We clean the company names
  street_NC_name = street_clean_companies(street_NC_name)
  
  
  #
  # with comma
  #
  
  # C = comma
  street_C = street_full[is_comma]
  n_C = length(street_C)
  if(n_C > 0){
    street_C_split = strsplit(street_C, ",", fixed = TRUE)
    
    n_all = lengths(street_C_split)
    id = rep(1:n_C, n_all)
    
    # sflat : split flat
    street_C_sflat = trimws(unlist(street_C_split))
    
    info_C = data.table(id, street_part = street_C_sflat)
    
    # We first re assemble street name / street number that are comma separated
    info_C[, id_prev := shift(id, fill = -1)]
    info_C[, street_part_prev := shift(street_part, fill = "")]
    info_C[, street_part_new := street_part]
    info_C[, is_nb := grepl("^[[:digit:]]", street_part)]
    info_C[is_nb == TRUE, street_part_new := paste(street_part_prev, street_part)]
    
    # NOTA: I consider the 'box' as extraneous information => this is not street!!!
    
    if(box_valid){
      invalid_pattern = "^(box|floor|building|lgh|igh|lg|lgn|lag|tel|tr|rum|entre|entrance|po|p\\.o|pl|apt|apartment|plan)\\b"
    } else {
      invalid_pattern = "^(floor|building|lgh|igh|lg|lgn|lag|tel|tr|rum|entre|entrance|po|p\\.o|pl|apt|apartment|plan)\\b"
    }
    
    # we detect what are the candidates for valid street name / street number
    info_C[, is_pattern := grepl("[^[:digit:]] [[:digit:]]", street_part_new)]
    info_C[, is_extraneous := grepl(invalid_pattern, street_part_new)]
    info_C[is_extraneous == TRUE, is_pattern := FALSE]
    info_C[, is_company := grepl("\\b(ab|c/o|r ?& ?d)\\b", street_part_new)]
    info_C[is_company == TRUE, is_pattern := FALSE]
    info_C[, n_pattern := sum(is_pattern), by = id]
    
    # we pick the first candidate for each id
    info_C_valid = info_C[is_pattern == TRUE]
    valid_add = info_C_valid[, .(street_full = first(street_part_new)), by = id]
    stopifnot(nrow(valid_add) == length(unique(valid_add$id)))
    
    # invalid => the address is complicated to extract and this is no individuals
    info_C_invalid = info_C[n_pattern == 0]
    
    # LATER: add addresses without numbers: "harbour, lodge" => "lodge"
    
    street_C_name = street_C_nb = building_C = character(n_C)
    
    street_valid = valid_add$street_full
    id_valid = valid_add$id
    street_C_name[id_valid] = extract_street_name(street_valid)
    street_C_nb[id_valid] = extract_street_nb(street_valid)
    building_C[id_valid] = extract_building(street_valid)
    
  } else {
    street_C_name = street_C_nb = building_C = NULL
  }
  
  
  #
  # the resulting information
  #
  
  street_name[is_comma] = street_C_name
  street_name[!is_comma] = street_NC_name
  
  street_nb[is_comma] = street_C_nb
  street_nb[!is_comma] = street_NC_nb
  
  building[is_comma] = building_C
  building[!is_comma] = building_NC
  
  # We clean the punctuation from the remainder
  city = cleanCharVector(city, cleanPunct = TRUE, cleanDigits = TRUE)
  street_name = cleanCharVector(street_name, cleanPunct = TRUE, cleanDigits = TRUE)
  
  # we extra clean
  street_name = str_op(street_name, "'aa => a'r, 'ae => a'r, 'oe => o'r, 'sankta => sankt'r, '\\bs t\\b => sankt'R, '\\bs ta\\b => sankt'R")
  # "(aa => a)r, (ae => a)r, (oe => o)r, (sankta => sankt)r, (\\bs t\\b => sankt)R, (\\bs ta\\b => sankt)R"
  
  base_add_small = data.table(raw = address_raw, city, postcode, 
                              street_name, street_nb, building)
  
  if(!any(qui_dup)){
    message(fsignif(length(address_raw_origin)), " addresses parsed in ", format_difftime(time_start))
    
    return(base_add_small)
  }
  
  order_add_int = order(address_int[!qui_dup])
  base_add_small = base_add_small[order_add_int]
  
  base_add = base_add_small[address_int]
  base_add[, raw := address_raw_origin]
  
  message(fsignif(length(address_raw_origin)), " addresses parsed in ", format_difftime(time_start))
  
  base_add
}


extract_se_address_statse = function(postcode_raw, city_raw, street_full_raw, silent = FALSE){
  
  # We don't add the raw address, it is too costly to compute
  
  time_start = Sys.time()
  
  base_add = data.table(postcode = postcode_raw, 
                        city = extract_city(city_raw),
                        street_name = extract_street_name(street_full_raw), 
                        street_nb = extract_street_nb(street_full_raw),
                        building = extract_building(street_full_raw))
  
  # we extra clean
  base_add[, street_name := str_op(street_name, "'aa => a'r, 'ae => a'r, 'oe => o'r, 'sankta => sankt'r, '\\bs t\\b => sankt'R, '\\bs ta\\b => sankt'R")]
  
  if(!silent) message(fsignif(nrow(base_add)), " addresses extracted in ", format_difftime(time_start))
  
  base_add
}

