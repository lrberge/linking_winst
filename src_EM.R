#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Jan 09 22:30:09 2023
# ~: 
#----------------------------------------------#

# INFO:
# - R part of the EM algorithm
# - contains code to explore the results (plot/summary)
# - a substantial and critical part of the code is in src/EM.cpp


# Required packages
library(fixest)
library(data.table)
library(dreamerr)

####
#### main algorithm ####
####


#' EM algorithm to match persons from two data sets
#' 
#' Flexible EM algorithm that can be used to find coherent groups based on a set of variables. One of its purposes is to identify the probability that two records refer to the same person when merging administrative data.
#'
#' @param base Data frame containing the variables.
#' @param vars Character vector of variables used for the classification. A likelihood family is automatically attributed to each variable based on their type. You can assign the variable to the family you want with the following syntax: \code{vars = c("var1" = "beta", "var2", "var3" = "normal")}. Here \code{var1} will be assigned a beta distribution and \code{var3} a Normal distribution while \code{var2} will be automatically set. The available likelihoods are \code{normal}, \code{multinomial} and \code{beta}.
#' @param init A vector used for initialization. Must be of the same length as the number of observations in the data base. Must have the same number of values as the number of classes (defined by argument \code{M}). It provides a guess of the initial partition. Can be missing.
#' @param M The number of classes, default is 2.
#' @param em.tol Stopping criterion: default is \code{1e-5}.
#' @param em.iter Maximum number of iterations, default is 500.
#' @param silent Logical scalar, default is `FALSE`. Whether to display the evolution of the likelihood and inform on the automatic types of the variables.
#' 
#' @details
#' 
#' The "influence" of a variable corresponds to the number of observations that would switch classes if that variable was to be removed.
#'
#' @return
#' It returns an \code{em_match} object with the results of the classification. Of interest: tau: the final probabilities to belong to each class for each observation. prms: the parameters associated to each variable and class.
#' 
#' `influence` is the number of observations that would switch class 
#'
#' @examples
#' 
#' res = em_matching(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), M = 3)
#' 
#' table(max.col(res$tau), iris$Species)
#' 
#' summary(res)
#' 
#' plot(res)
#' 
#' 
em_matching = function(base, vars, init, M = 2, em.tol = 1e-8, em.iter = 100, 
                       cpp.clean = TRUE, silent = FALSE){
  # vars => variables to use + which family to use
  # M: nber of categories
  
  check_arg(base, "data.frame mbt")
  check_arg(vars, "character vector mbt")
  check_arg(M, "integer scalar GE{2}")
  check_arg(em.tol, "numeric scalar GT{0} LT{1}")
  check_arg(cpp.clean, "logical scalar")
  
  K = length(vars)
  is_log = rep(FALSE, K)
  variables = c()
  if(!is.null(names(vars))){
    method = c()
    var_name = names(vars)
    for(k in 1:K){
      if(var_name[k] == ""){
        # We find it automatically
        method[k] = "auto"
        v = vars[k]
        check_value_plus(v, "match", .choices = names(base), 
                         .message = paste0("The ", n_th(k), " element of argument 'vars' is not valid. It should be one variable of the data. But currently it is equal to '", v, "'."))
        variables[k] = v
      } else {
        m = vars[k]
        
        if(grepl("\\blog\\b", m)){
          is_log[k] = TRUE
          m = gsub("\\blog\\b", "", m)
          m = gsub("[^[:alpha:]]", "", m)
        }
        
        check_value_plus(m, "match", .choices = c("normal", "beta", "multinomial"), 
                         .message = paste0("The ", n_th(k), " element of argument 'vars' is not valid. It should be one of 'normal', 'beta' or 'multinomial'. But currently it is equal to '", m, "'"))
        method[k] = m
        v = var_name[k]
        check_value_plus(v, "match", .choices = names(base), 
                         .message = paste0("The ", n_th(k), " element of the names of argument 'vars' is not valid. It should be one variable of the data. But currently it is equal to '", v, "'."))
        variables[k] = v
      }
    }
  } else {
    check_value_plus(vars, "multi match", .choices = names(base))
    variables = vars
    method = rep("auto", K)
  }
  
  check_arg(init, "vector(logical, integer) len(data)", .data = base)
  if(missing(init)){
    init = sample(M, n, TRUE)
  }
  
  # We clean the data set => handling NAs
  ANY_NA = obs_na = obs_na_init = FALSE
  if(anyNA(base)){
    obs_na = !complete.cases(base)  
  }
  
  if(anyNA(init)){
    obs_na_init = is.na(init)
    obs_na = obs_na | obs_na_init
  }
  
  if(!isFALSE(obs_na)){
    ANY_NA = TRUE
    n_rm = sum(obs_na)
    extra = ""
    if(!isFALSE(obs_na_init)){
      extra = dsb(" (including .[fsignif(sum(obs_na_init))] in the `init`)")
    }
    message(dsb(".[fsignif(n_rm)] observation.[plural(n_rm)] removed because of NA values.[extra]."))
    base = base[!obs_na, ]
    init = init[!obs_na]
  }
  
  
  X = all_items = list()
  info_vars = list()
  for(k in 1:K){
    
    x = base[[variables[k]]]
    prefix = paste0("The ", n_th(k), " variable")
    info_tmp = list()
    info_tmp$is_log = is_log[k]
    
    if(method[k] == "auto"){
      check_value(x, "vector", .prefix = prefix)
      n_cases = length(unique(x))
      if(!is.numeric(x) || n_cases < 10){
        method[k] = "multinomial"
      } else if(all(x >= 0 & x <= 1)){
        method[k] = "beta"
      } else {
        method[k] = "normal"
      }
      
      message("Variable '", variables[k], "' has been set to ", method[k], ".")
    }
    
    if(is_log[k]){
      check_value(x, "numeric vector GE{0}", .prefix = prefix)
      qui_0 = x == 0
      if(any(qui_0)){
        minValue = min(x[!qui_0]) / 10
        x[qui_0] = minValue
      }
      x = log(x)
    }
    
    if(method[k] == "normal"){
      check_value(x, "numeric vector", .prefix = prefix)
      X[[k]] = x
      
    } else if(method[k] == "beta"){
      check_value(x, "numeric vector", .prefix = prefix)
      
      if(any(x < 0 | x > 1)){
        message("Variable '", variables[k], "' is outside [0, 1] and is then normalized to fit the beta distribution.")
        min_x = min(x)
        max_x = max(x)
        info_tmp$x_range = c(min_x, max_x)
        x = (x - min_x) / (max_x - min_x)
      }
      
      qui_0 = x == 0
      if(any(qui_0)){
        minVal = min(min(x[!qui_0]), 1e-6)
        x[qui_0] = minVal
      }
      
      qui_1 = x == 1
      if(any(qui_1)){
        maxVal = max(max(x[!qui_1]), 1 - 1e-6)
        x[qui_1] = maxVal
      }
      
      X[[k]] = x
      
    } else if(method[k] == "multinomial"){
      check_value(x, "vector", .prefix = prefix)
      quf = to_integer(x, add_items = TRUE, items.list = TRUE, sorted = TRUE)
      # quf = fixest:::quickUnclassFactor(x, addItem = TRUE)
      all_items[[k]] = quf$items
      X[[k]] = quf$x - 1
    } else {
      stop("Internal error: method ", method[k], " is not implemented.")
    }
    
    info_vars[[k]] = info_tmp
  }
  
  # XX: numeric matrix of all the covariates
  XX = do.call("cbind", X) * 1
  
  # EM algorithm
  n = nrow(base)
  
  tau = matrix(0.2, n, M)
  
  # Initialization
  if(is.logical(init)){
    if(M != 2) stop("Argument 'init' cannot be logical when M>2. It should then be an integer with values ranging from 1 to M.")
    init = 1 + init
  } else if(min(init) == 0){
    init = 1 + init
  }
  
  if(!all(unique(init) %in% 1:M)){
    stop("Argument 'init' must take values ranging from 1 to M, and each case must have several values. That's not the case at the moment (eg ", enumerate_items(head(setdiff(1:M, unique(init)), 3), "is"), " missing).")
  }
  
  tau[cbind(1:n, init)] = 0.8
  tau = tau / rowSums(tau)
  
  # setting up
  cpp_setup_X_parameters(XX, method)
  cpp_setup_tau_parameters(tau)
  
  ll_old = -Inf
  ll_all = c()
  iter = 0
  delta = Inf
  while(delta > em.tol && iter < em.iter){
    iter = iter + 1
    
    # M-step
    ll_obs = cpp_m_step(method)
    
    # # LL before max on tau
    # tau = cpp_get_tau()
    # tau_ok = as.vector(tau)[as.vector(tau) > 0]
    # ll = sum(tau * ll_obs) + sum(tau*log(tau_ok))
    # cat("LL after prms max: ", signif_plus(ll), "\n")
    
    # E-step
    ll = cpp_map(ll_obs)
    # we add the tau*ln(tau) value to get the value of the lower bound
    
    # # LL after max on tau
    # tau = cpp_get_tau()
    # tau_ok = as.vector(tau)[as.vector(tau) > 0]
    # ll = ll + sum(tau*log(tau_ok))
    # 
    # cat("LL after tau  max: ", signif_plus(ll), "\n")
    
    # LL
    d = ll - ll_old
    if(iter > 1){
      delta = abs(ll - ll_old)/(0.1 + abs(ll))
    } else {
      w = nchar(signif_plus(ll, r = 1))
    }
    
    ll_all = c(ll_all, ll)
    ll_old = ll
    
    cat("iter ", sfill(iter, 3, "."), " -- ll = ", 
        sfill(signif_plus(ll, r = 1), w, right = TRUE), 
        " -- delta = ", d, "\n", sep = "")
    # if(d < 0) break
  }
  
  prms = cpp_get_params(method)
  
  names(prms) = c(variables, "delta")
  for(k in 1:K){
    p = prms[[k]]
    
    if(method[k] == "normal"){
      row.names(p) = c("mu", "sigma")
    } else if(method[k] == "beta"){
      row.names(p) = c("alpha", "beta")
    } else if(method[k] == "multinomial"){
      row.names(p) = all_items[[k]]
    }
    prms[[k]] = p
  }
  
  tau = cpp_get_tau()
  
  # The influence of each variable
  infl = cpp_em_influence(ll_obs, method)
  
  # Cleaning up the static data in cpp
  if(cpp.clean) cpp_cleanup()
  
  if(ANY_NA){
    # We need to rebuild tau so that it exactly matches the original data set
    tau_NA = matrix(NA_real_, length(obs_na), M)
    tau_NA[!obs_na, ] = tau
    tau = tau_NA
  }
  
  res = list(n = n, tau = tau, prms = prms, ll_all = ll_all, method = method, influence = infl, info_vars = info_vars)
  class(res) = "em_match"
  res
}


####
#### methods ####
####


#' Summary for em_match objects
#' 
#' Provides the estimated parameters for each variable as well as the influence of each variable.
#'
#' @param object An object of class \code{em_match}.
#' @param ... Not currently used.
#'
#' @return
#' Doesn't return anything, only prints the results on the console.
#'
#' @examples
#' 
#' res = em_matching(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), M = 3)
#' 
#' table(max.col(res$tau), iris$Species)
#' 
#' summary(res)
#' 
#' 
summary.em_match = function(object, ...){
  cat("EM match: LL: ", signif_plus(tail(object$ll_all, 1)), "\n", sep = "")
  
  prms = object$prms
  K = length(prms) - 1
  M = length(prms$delta)
  
  cat("\nFraction per class: ", paste0(1:M, ": ", signif_plus(prms$delta * 100), "%", collapse = ", "), "\n\n", sep = "")
  
  cat("Estimated parameters: \n")
  
  vars = names(prms)
  method = object$method
  
  influence = object$influence / object$n * 100
  
  col_name = paste0("class ", 1:M)
  
  for(k in 1:K){
    cat(vars[k], " (", method[k], ") -- Influence: ", influence[k], "%\n", sep = "")
    x = prms[[k]]
    
    if(method[k] == "multinomial"){
      n = nrow(x)
      if(n > 12){
        r = colSums(x[-(1:9), ])
        xx = rbind(x[1:9, ], r)
        rownames(xx)[10] = paste0("... ", n - 9, " remaining")
        x = xx
      }
    }
    
    colnames(x) = col_name
    
    print(x)
    
    if(k != K) cat("\n")
  }
}





#' Plot method for em_match objects
#' 
#' Plots the estimated parameter distribution for each variable and class.
#'
#' @param x An object of class \code{em_match}.
#' @param y Which variable to display? By default all of them. You can use the special argument \code{.K} to refer to the last variable.
#' @param mfrow If several variables are to be displayed then \code{par(mfrow = mfrow)} will be used. Use this argument if you're not happy with the default.
#' @param dict Dictionary for the variables. By default uses the \code{getFixest_dict} dictionary.
#' @param ... Not currently used.
#'
#'
#' @examples
#' 
#' res = em_matching(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), M = 3)
#' 
#' table(max.col(res$tau), iris$Species)
#' 
#' summary(res)
#' 
#' plot(res)
#' 
#' 
plot.em_match = function(x, y, mfrow, dict = getFixest_dict(), dict.class, ylab, ...){
  
  if(!requireNamespace("fplot", quietly = TRUE)){
    message("The package `fplot` is not installed. Trying to install it with:",
            "\n`install.packages(\"fplot\")`")
    install.packages("fplot")
  }
  suppressPackageStartupMessages(library(fplot))
  
  
  K = length(x$prms) - 1
  
  check_arg_plus(y, "evalset integer vector GT{0} | match(proba)", .data = list(.K = K))
  if(missing(ylab)) ylab = "Classification result"
  
  if(missing(y)) y = 1:K
  
  my_cols = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
  
  if(missnull(dict.class)){
    dict.class = NULL
  } else {
    check_arg(dict.class, "character vector no na")
    if(is.null(names(dict.class))){
      dict.class = setNames(dict.class, seq_along(dict.class))
    }
  }
  
  if(identical(y, "proba")){
    
    # The distributions oof probabilities
    
    tau = x$tau
    
    classes = max.col(tau)
    
    M = max(classes)
    
    my_cols_alpha = c()
    for(m in 1:M){
      current_col = as.vector(col2rgb(my_cols[m], alpha = TRUE)) / 255
      my_cols_alpha[m] = rgb(current_col[1], current_col[2], current_col[3], 0.8)
    }
    
    class_fmt = dict_apply(classes, dict.class)
    
    base = data.table(probability = tau[cbind(1:nrow(tau), classes)], class = class_fmt)
    
    fplot::plot_distr(~ probability | class, base, mod.method = "side", col = my_cols_alpha, top = "frac", ylab = ylab)
    title(main = "Probability to belong to each class")
    
  } else {
    # With K numerix
    
    
    
    if(any(y > K)) stop("Argument 'y' must contain values from 1 to ", K, ".")
    
    all_k = y
    
    method = x$method
    
    M = length(x$prms$delta)
    dots = list(...)
    
    class_default = setNames(paste0("Class ", 1:M), 1:M)
    qui = intersect(names(class_default), names(dict.class))
    class_default[qui] = dict.class[qui]
    class_fmt = dict_apply(1:M, class_default)
    
    n_k = length(all_k)
    if(n_k > 1){
      do_par = TRUE
      
      if(missing(mfrow)){
        if(!identical(par("mfrow"), c(1L, 1L))){
          # means the user did set mfrow => we don't redo it
          do_par = FALSE
        } else {
          mfrow = switch(as.character(n_k), "2" = c(2, 1), "3" = c(3, 1), "4" = c(2, 2), "5" = c(3, 2), "6" = c(3, 2), c(3, 3))
        }
      } else {
        check_arg(mfrow, "integer vector len(2) GE{1}")
      }
      
      if(do_par){
        # we also change the title space
        mar = par("mar")
        mar = c(4.1, 4.1, 2.8, 1.5)
        
        op = par(mfrow = mfrow, mar = mar)
        on.exit(par(op))
      }
      
    }
    
    for(k in all_k){
      
      xx = x$prms[[k]]
      info_x = x$info_vars[[k]]
      
      my_alpha = ifelse(method[k] == "multinomial", 0.8, 0.5)
      my_cols_alpha = c()
      for(m in 1:M){
        current_col = as.vector(col2rgb(my_cols[m], alpha = TRUE)) / 255
        my_cols_alpha[m] = rgb(current_col[1], current_col[2], current_col[3], my_alpha)
      }
      
      xlab = dict_apply(names(x$prms)[k], dict)
      
      if(method[k] == "multinomial"){
        
        base = data.table(x = rep(rownames(xx), M))
        dict_var = c(x = names(x$prms)[k])
        base$probability = as.numeric(xx)
        base$Class = dict_apply(rep(1:M, each = nrow(xx)), dict.class)
        
        mod.method = ifelse(nrow(xx) <= 12, "side", "split")
        
        base$x = factor(base$x, levels = rownames(xx))
        
        fplot::plot_distr(probability ~ x | Class, base, dict = dict_var, mod.method = mod.method, 
                          col = my_cols_alpha, xlab = xlab, main = xlab, ylab = ylab, sorted = FALSE)
        
      } else if(method[k] == "normal"){
        
        # first: finding the xlim
        all_min = all_max = c()
        for(m in 1:M){
          all_min[m] = xx[1, m] - 3*xx[2, m]
          all_max[m] = xx[1, m] + 3*xx[2, m]
        }
        xlim = c(min(all_min), max(all_max))
        
        # Now the ylim
        x_1k = seq(xlim[1], xlim[2], length.out = 500)
        y_all = list()
        
        for(m in 1:M){
          y_all[[m]] = dnorm(x_1k, xx[1, m], xx[2, m])
        }
        ylim = c(0, max(unlist(y_all)))
        
        info = legendFit(legend = class_fmt, plot = FALSE)
        add_usr = info$total_height / par("pin")[2] * diff(ylim)
        ylim[2] = ylim[2] + add_usr
        
        # Empty plot
        my_ylab = if(missing(ylab)) "Density" else ylab
        plot(0, 0, xlim = xlim, ylim = ylim, type = "n", ylab = my_ylab, xlab = xlab, main = xlab, axes = FALSE)
        
        box()
        axis(2, lwd = 0, lwd.ticks = 1)
        if(info_x$is_log){
          log_axis()
        } else {
          axis(1, lwd = 0, lwd.ticks = 1)
        }
        
        for(m in 1:M){
          y = y_all[[m]]
          
          shade_area(0, y, x = x_1k, border = NA, col = my_cols_alpha[m])
          lines(x_1k, y, col = my_cols[m], lwd = 2)
        }
        
        legendFit(legend = class_fmt, fill = my_cols_alpha, border = my_cols[1:M])
        
        
        
      } else if(method[k] == "beta"){
        # first: finding the xlim
        xlim = c(0, 1)
        
        # Now the ylim
        x_1k = seq(1e-2, 1-1e-2, length.out = 500)
        y_all = list()
        
        for(m in 1:M){
          y_all[[m]] = dbeta(x_1k, xx[1, m], xx[2, m])
        }
        ylim = c(0, max(unlist(y_all)))
        
        info = legendFit(legend = class_fmt, plot = FALSE)
        add_usr = info$total_height / par("pin")[2] * diff(ylim)
        ylim[2] = ylim[2] + add_usr
        
        # Empty plot
        my_ylab = if(missing(ylab)) "Density" else ylab
        plot(0, 0, xlim = xlim, ylim = ylim, type = "n", ylab = my_ylab, xlab = xlab, main = xlab, axes = FALSE)
        
        box()
        axis(2, lwd = 0, lwd.ticks = 1)
        if(info_x$is_log){
          log_axis(info_x$x_range)
        } else {
          m = value = axis(1, labels = NA, lwd = 0)
          
          if(!is.null(info_x$x_range)){
            value = m * diff(info_x$x_range) + info_x$x_range[1]
          }
          
          axis(1, m, labels = value, lwd = 0, lwd.ticks = 1)
        }
        
        for(m in 1:M){
          y = y_all[[m]]
          
          shade_area(0, y, x = x_1k, border = NA, col = my_cols_alpha[m])
          lines(x_1k, y, col = my_cols[m], lwd = 2)
        }
        
        legendFit(legend = class_fmt, fill = my_cols_alpha, border = my_cols[1:M])
      }
    }
  }
  
}

####
#### utilities ####
####

# INFO:
# - sub functions used in the functions previously defined


log_axis = function(new_range = NULL){
  
  m = value = axis(1, labels = NA, lwd = 0)
  
  if(!is.null(new_range)){
    value = m * diff(new_range) + new_range[1]
  }
  
  mm = round(log10(exp(value)))
  
  if(min(mm) < 4 || max(mm) > 7){
    at = mm * log(10)
    for(i in 1:length(at)){
      axis(1, at[i], substitute(10^p, list(p = mm[i])))
    }
  } else {
    axis(1, at, signif_plus(10**mm), lwd = 0, lwd.ticks = 1)
  }
}

col_alpha = function(x, alpha){
  
  M = length(x)
  
  alpha = rep(alpha, M)
  
  res = c()
  for(m in 1:M){
    current_col = as.vector(col2rgb(x[m], alpha = TRUE)) / 255
    res[m] = rgb(current_col[1], current_col[2], current_col[3], alpha[m])
  }
  
  res
}

dict_apply = function(x, dict){
  
  # getting the dict
  if (is.logical(dict)) {
    if(dict == FALSE){
      return(x)
    }
    dict = getFixest_dict()
    
  } else {
    dict_origin = getFixest_dict()
    if (!is.null(dict_origin)) {
      if (!is.null(dict)) {
        dict_origin[names(dict)] = as.vector(dict)
      }
      dict = dict_origin
    }
  }
  
  if (is.null(dict)) {
    return(x)
  }
  
  # applying the dict
  dict_names = gsub(" ", "", names(dict), fixed = TRUE)
  x_clean = gsub(" ", "", x, fixed = TRUE)
  res = x
  who_in = x_clean %in% dict_names
  
  if (any(who_in)) {
    res[who_in] = dict[x_clean[who_in]]
  }
  
  res
}

get_y_lim = function(){
  myRawLim = par("usr")
  xrange = myRawLim[2] - myRawLim[1]
  x_ext = 0.04 * (xrange/1.08)
  yrange = myRawLim[4] - myRawLim[3]
  y_ext = 0.04 * (yrange/1.08)
  myLim = myRawLim + c(x_ext, -x_ext, y_ext, -y_ext)
  y_min = myLim[3]
  y_max = myLim[4]
  isLog = par("ylog")
  if (isLog) {
    res = 10^c(y_min, y_max)
  }
  else {
    res = c(y_min, y_max)
  }
  return(res)
}

listDefault = function(x, variable, value){
  x_name = deparse(substitute(x))
  if (is.null(x[[variable]])) {
    x[[variable]] = value
    assign(x_name, x, envir = parent.frame(n = 1))
  }
}


legendFit = function (where = "top", legend, minCex = 0.7, trunc, trunc.method = "auto", 
                      plot = TRUE, title = NULL, title_out = FALSE, ...){
  # fits the legend with autosizing of the text
  
  check_arg_plus(title, "null character scalar conv")
  check_arg(title_out, "logical scalar")
  
  ADD_TITLE = FALSE
  if (length(title) == 1 && nchar(title) > 0 && grepl("[^ ]", title)) {
    ADD_TITLE = TRUE
  }
  
  do_adj = 0
  if (!ADD_TITLE) {
    do_adj = -1
  } else if (!title_out) {
    do_adj = 1.9
  }
  
  n = length(legend)
  AUTO_TRUNC = TRUE
  if (!missing(trunc)) {
    AUTO_TRUNC = FALSE
    if (is.logical(legend)){
      legend = as.character(legend)
    }
    myLabels = truncate_string(legend, trunc, trunc.method)
  } else {
    myLabels = legend
    trunc = 100
  }
  
  largeur_totale = par("pin")[1]
  myCex = 1
  fsize = function(x, cex) max(strwidth(x, units = "in", cex = cex)) + 
    strwidth("WWWl", units = "in", cex = cex)
  unit_size = fsize(myLabels, 1)
  
  while (myCex >= minCex && n * unit_size > largeur_totale) {
    myCex = myCex * 0.95
    unit_size = fsize(myLabels, myCex)
  }
  
  nlines = 1
  if (n * unit_size > largeur_totale) {
    nlines = 2
    n_top = ceiling(n/2)
    myCex = 1
    unit_size = fsize(myLabels, myCex)
    while (myCex >= minCex && n_top * unit_size > largeur_totale) {
      myCex = myCex * 0.95
      unit_size = fsize(myLabels, myCex)
    }
  }
  
  res = list(cex = myCex)
  hauteur_caractere = strheight("W", units = "in", cex = myCex)
  
  if (nlines == 2) {
    res$total_height = (4 + do_adj/2 + 0.5) * hauteur_caractere
  } else {
    res$total_height = (2.5 + do_adj/2 + 0.5) * hauteur_caractere
  }
  
  if (AUTO_TRUNC && myCex <= minCex) {
    n_relevant = ifelse(nlines == 2, n_top, n)
    minTrunc = 5
    trunc = max(minTrunc, min(25, max(nchar(legend))) - 3)
    myLabels = truncate_string(legend, trunc, trunc.method)
    unit_size = fsize(myLabels, myCex)
    while (n_relevant * unit_size > largeur_totale && trunc > 
           minTrunc) {
      trunc = max(minTrunc, trunc - 3)
      myLabels = truncate_string(myLabels, trunc, trunc.method)
      unit_size = fsize(myLabels, myCex)
    }
  }
  
  res$trunc = trunc
  if (plot == FALSE) {
    return(res)
  }
  
  if (do_adj != 0) {
    h = strheight("W")/diff(get_y_lim())
    adj_title = do_adj * h/2.1
  } else {
    adj_title = 0
  }
  
  if (nlines == 2) {
    leg1 = legend(where, myLabels[1:n_top], fill = "black", 
                  cex = myCex, horiz = TRUE, plot = FALSE, inset = adj_title)
    leg2 = legend(where, myLabels[(n_top + 1):n], fill = "black", 
                  cex = myCex, horiz = TRUE, plot = FALSE)
    leg2_x = leg2$rect$left
    if (grepl("top", where)) {
      leg2_y = leg1$rect$top - 1.5 * strheight("W", units = "user", 
                                               cex = myCex)
    }
    else {
      leg2_y = leg1$rect$top + 1.5 * strheight("W", units = "user", 
                                               cex = myCex)
    }
  }
  
  legend_options = list(...)
  legend_options$legend = as.character(myLabels)
  listDefault(legend_options, "bty", "n")
  listDefault(legend_options, "horiz", TRUE)
  listDefault(legend_options, "cex", myCex)
  listDefault(legend_options, "x", where)
  if (nlines == 1) {
    if (do_adj != 0) {
      legend_options$inset = adj_title
    }
    do.call("legend", legend_options)
  } else {
    
    legend_options_1 = legend_options
    for (var in intersect(names(legend_options_1), 
                          c("fill", "lwd", "lty", "col", "pch", "angle", "density"))){
      
      val = legend_options_1[[var]]
      n_val = length(val)
      if (n_val > 1) {
        if (n_val < n) {
          val = rep(val, ceiling(n/n_val))
        }
        legend_options_1[[var]] = val[1:n_top]
      }
    }
    
    legend_options_1$legend = legend_options_1$legend[1:n_top]
    if (do_adj != 0) {
      legend_options_1$inset = adj_title
    }
    
    do.call("legend", legend_options_1)
    legend_options_2 = legend_options
    for (var in intersect(names(legend_options_2), 
                          c("fill", "lwd", "lty", "col", "pch", "angle", "density"))) {
      val = legend_options_2[[var]]
      n_val = length(val)
      if (n_val > 1) {
        if (n_val < n) {
          val = rep(val, ceiling(n/n_val))
        }
        legend_options_2[[var]] = val[(n_top + 1):n]
      }
    }
    legend_options_2$x = leg2_x
    legend_options_2$y = leg2_y
    legend_options_2$legend = legend_options_2$legend[(n_top + 
                                                         1):n]
    do.call("legend", legend_options_2)
  }
  
  if (ADD_TITLE) {
    if (title_out) {
      legend("top", legend = title, adj = c(0, -1), bty = "n", 
             text.font = 3, xpd = TRUE)
    } else {
      legend("top", legend = title, adj = c(0, -0.3), bty = "n", 
             text.font = 3)
    }
  }
  
  return(invisible(res))
}


shade_area = function(y1, y2, x, xmin, xmax, col="grey", ...){
  # fonction plus pratique que polygon
  # elle permet de griser une partie delimitee par
  # y1 et y2 pour chacune des valeurs de x
  # on doit avoir la meme longueur de y1,y2 et x
  # exemple:
  # a=curve(x**2,-5,5)
  # shade_area(a$y+1,a$y-1,a$x)
  # qqes parametres graphiques:
  # lwd / border (couleur du bord, peut etre NA) / lty
  
  n = length(x)
  stopifnot(length(y1) == n | length(y1) == 1)
  stopifnot(length(y2) == n | length(y2) == 1)
  
  if(length(y1) == 1) y1 = rep(y1,n)
  if(length(y2) == 1) y2 = rep(y2,n)
  
  if(missing(xmin)) xmin = min(x)
  if(missing(xmax)) xmax = max(x)
  
  ind = which(x >= xmin & x <= xmax)
  x1 = x[ind]
  x2 = x[rev(ind)]
  polygon(c(x1,x2), c(y1[ind], y2[rev(ind)]), col=col, ...)
}








