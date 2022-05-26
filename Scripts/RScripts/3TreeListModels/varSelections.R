varSelection1<-function (x, y, method = "addVars", yaiMethod = "msn", wts = NULL, 
    nboot = 20, trace = FALSE, useParallel = if (.Platform$OS.type == 
        "windows") FALSE else TRUE, ...) {
    if (missing(x)) 
        stop("x must be specified.")
    if (missing(y)) 
        stop("y must be specified.")
    okMethods <- c("addVars", "delVars")
    if (!(method %in% okMethods)) 
        stop("method=\"", method, "\" must be one of: \"", paste0(okMethods, 
            collapse = "\", \""), "\"")
    if (is.null(wts)) 
        wts <- rep(1, ncol(y))
    if (useParallel && .Platform$OS.type != "Windows" && requireNamespace("parallel")) {
        myapply <- parallel::mclapply
    }
    else {
        if (useParallel) 
            warning("package parallel was not loaded and is not being used")
        myapply <- lapply
    }
    cl <- match.call()
    bootstrap <- nboot > 0
    if (yaiMethod == "gnn") {
        if (!requireNamespace("vegan")) 
            stop("install vegan and try again")
    }
    if (yaiMethod == "ica") {
        if (!requireNamespace("fastICA")) 
            stop("install fastICA and try again")
    }
    if (yaiMethod == "randomForest") {
        if (!requireNamespace("randomForest")) 
            stop("install randomForest and try again")
    }
    if (yaiMethod == "msnPP") {
        if (!requireNamespace("ccaPP")) 
            stop("install ccaPP and try again")
    }
    if (method == "delVars") {
        allErr <- unlist(myapply(1:max(1, nboot), function(i, 
            x, y, wts, yaiMethod, ...) suppressWarnings(grmsd1(one = suppressWarnings(yai(x = x, 
            y = y, method = yaiMethod, bootstrap = bootstrap, 
            bootstrap, ...)), ancillaryData = y, wts = wts)), 
            x, y, wts, yaiMethod, bootstrap, ...))
        if (trace) 
            cat("With all vars, mean grmsd (over boostraps) = ", 
                mean(allErr), "; stddev=", sd(allErr), "; Num cols = ", 
                ncol(x), "\n", sep = "")
        xa <- x
        selvars <- list(None = allErr)
        while (ncol(xa) > 1) {
            err <- list()
            for (var in 1:ncol(xa)) err[[var]] <- unlist(myapply(1:max(1, 
                nboot), function(i, xa, y, wts, var, yaiMethod, 
                bootstrap, ...) suppressWarnings(grmsd1(one = suppressWarnings(yai(x = xa[, 
                -var, drop = FALSE], y = y, method = yaiMethod, 
                bootstrap = bootstrap, ...)), ancillaryData = y, 
                wts = wts)), xa, y, wts, var, yaiMethod, bootstrap, 
                ...))
            names(err) <- names(xa)
            del <- which.min(unlist(lapply(err, mean)))
            selvars[[names(del)]] <- as.vector(unlist(err[del]))
            xa <- xa[, -del, drop = FALSE]
            remVars <- colnames(xa)
            if (trace) 
                cat("Delete var= ", names(del), "; mean grmsd (over boostraps) = ", 
                  mean(err[[del]]), "; stddev=", sd(err[[del]]), 
                  "; Num cols remaining= ", ncol(xa), "\n", sep = "")
        }
    }
    else if (method == "addVars") {
        remVars <- NULL
        selvars <- list()
        keep <- NULL
        while (length(keep) < ncol(x)) {
            err <- list()
            for (var in setdiff(names(x), keep)) {
                xa <- x[, c(keep, var), drop = FALSE]
                err[[var]] <- unlist(myapply(1:max(1, nboot), 
                  function(i, xa, y, wts, yaiMethod, bootstrap, 
                    ...) suppressWarnings(grmsd1(one = suppressWarnings(yai(x = xa, 
                    y = y, method = yaiMethod, bootstrap = bootstrap, 
                    ...)), ancillaryData = y, wts = wts)), xa, 
                  y, wts, yaiMethod, bootstrap, ...))
            }
            add <- names(which.min(unlist(lapply(err, mean))))
            selvars[[add]] <- as.vector(unlist(err[add]))
            keep <- c(keep, add)
            if (trace) 
                cat("Added var= ", add, "; mean grmsd (over boostraps) = ", 
                  mean(err[[add]]), "; stddev=", sd(err[[add]]), 
                  "; Num cols being used= ", ncol(xa), "\n", 
                  sep = "")
        }
    }
    err <- lapply(selvars, function(x) c(mean(x), sd(x)))
    rn <- names(err)
    err <- matrix(unlist(err), ncol = 2, byrow = TRUE)
    rownames(err) <- rn
    colnames(err) <- c("mean", "sd")
    rtn <- list(call = cl, grmsd = err, allgrmsd = selvars, method = method)
    if (!is.null(remVars)) 
        rtn$remVars <- remVars
    class(rtn) <- c("varSel", "list")
    rtn
}

grmsd1<-function (..., ancillaryData = NULL, vars = NULL, wts = NULL, 
		rtnVectors = FALSE) 
{
	if (missing(...)) 
		stop("... required")
	args <- list(...)
	argLabs <- as.list(substitute(list(...)))[-1]
	names(args) <- if (is.null(names(argLabs))) 
				unlist(argLabs)
			else {
				fixNames <- names(argLabs) == ""
				names(argLabs)[fixNames] <- argLabs[fixNames]
				names(argLabs)
			}
	okClasses <- c("yai", "impute.yai", "data.frame", "matrix", 
			"lm")
	if (!is.null(wts)) {
		if (any(wts < 0) || sum(wts) <= 0) 
			stop("wts must be positive and sum > 0")
	}
	mgd <- list()
	for (objName in names(args)) {
		object <- args[[objName]]
		if (!inherits(object, okClasses)) {
			warning("object ", objName, " class is not one of ", 
					paste(okClasses, collapse = ", "))
			next
		}
		if (inherits(object, "yai")) 
			object <- impute.yai(object, ancillaryData = ancillaryData, 
					vars = vars, observed = TRUE,method="dstWeighted")
		if (inherits(object, "lm")) {
			pr <- predict(object)
			ob <- pr + resid(object)
			if (is.null(dim(pr))) {
				object <- cbind(pr, ob)
				colnames(object) = c(objName, paste0(objName, 
								".o"))
			}
			else {
				colnames(ob) = paste0(colnames(ob), ".o")
				object <- cbind(pr, ob)
			}
		}
		object <- na.omit(object)
		if (nrow(object) == 0) {
			warning("argument ", objName, " has no rows.")
			next
		}
		if (inherits(object, "matrix") & mode(object) != "numeric") {
			warning("argument ", objName, " must be numeric.")
			next
		}
		facts = if (inherits(object, "matrix")) 
					FALSE
				else unlist(lapply(object, is.factor))
		if (any(facts)) {
			if (all(facts)) {
				warning("all variables are factors in ", objName)
				next
			}
			else {
				nams <- names(facts)[facts]
				nams <- nams[-grep("[.]o$", nams)]
				warning("factor(s) have been removed from ", 
						objName, ": ", paste0(nams, collapse = ", "))
				object <- object[, !facts, drop = FALSE]
			}
		}
		useVars <- if (is.null(vars)) 
					colnames(object)
				else {
					ov <- grep("[.]o$", vars)
					ov <- if (length(ov) == 0) 
								unique(c(vars, paste0(vars, ".o")))
							else vars
					intersect(ov, colnames(object))
				}
		if (length(useVars) == 0) {
			warning("needed variables not found in ", objName)
			next
		}
		ov = useVars[grep("[.]o$", useVars)]
		if (length(ov) == 0) {
			warning("no observed variables found in ", objName)
			next
		}
		pv <- unique(sub("[.]o$", "", ov))
		pv <- intersect(pv, useVars)
		if (length(pv) == 0) {
			warning("nothing to compute in ", objName)
			next
		}
		ob <- as.matrix(object[, ov, drop = FALSE])
		pr <- as.matrix(object[, pv, drop = FALSE])
		qr <- qr(ob)
		uvars <- qr$pivot[1:qr$rank]
		if (length(uvars) < length(ov)) 
			warning("rank deficiency in ", objName, " was addressed by removing: ", 
					paste0(c(colnames(ob)[qr$pivot[(qr$rank + 1):length(qr$pivot)]], 
									colnames(pr)[qr$pivot[(qr$rank + 1):length(qr$pivot)]]), 
							collapse = ", "))
		p <- solve(chol(cov(ob[, uvars, drop = FALSE])))
		ob <- as.matrix(ob[, uvars]) %*% p
		pr <- as.matrix(pr[, uvars]) %*% p
		wt <- wts
		wt <- if (is.null(wt)) 
					rep(1, ncol(pr))
				else {
					if (length(names(wt)) > 0) {
						names(wt) <- sub("[.]o$", "", names(wt))
						wt <- na.omit(wt[names(pr)])
					}
					if (length(wt) != ncol(pr)) {
						warning("weights do not match variables in ", 
								objName, " and were ignored.")
						wt <- rep(1, ncol(pr))
					}
					wt
				}
		wt <- wt/sum(wt)
		md <- apply((pr - ob), 1, function(x, wt) sum((x^2) * 
									wt), wt)
		mgd[[objName]] <- if (rtnVectors) 
					sqrt(md)
				else sqrt(mean(md))
	}
	if (rtnVectors) {
		idx <- sort(unlist(lapply(mgd, function(x) sqrt(mean(x)))), 
				index.return = TRUE)$ix
		mgd[idx]
	}
	else sort(unlist(mgd))
}

varSelection2<-function (x, y, method = "addVars", yaiMethod = "msn", wts = NULL, 
    nboot = 20, trace = FALSE, useParallel = if (.Platform$OS.type == 
        "windows") FALSE else TRUE, ...) {

    if (missing(x)) 
        stop("x must be specified.")
    if (missing(y)) 
        stop("y must be specified.")
    okMethods <- c("addVars", "delVars")
    if (!(method %in% okMethods)) 
        stop("method=\"", method, "\" must be one of: \"", paste0(okMethods, 
            collapse = "\", \""), "\"")
    if (is.null(wts)) 
        wts <- rep(1, ncol(y))
    if (useParallel && .Platform$OS.type != "Windows" && requireNamespace("parallel")) {
        myapply <- parallel::mclapply
    }
    else {
        if (useParallel) 
            warning("package parallel was not loaded and is not being used")
        myapply <- lapply
    }
    cl <- match.call()
    bootstrap <- nboot > 0
    if (yaiMethod == "gnn") {
        if (!requireNamespace("vegan")) 
            stop("install vegan and try again")
    }
    if (yaiMethod == "ica") {
        if (!requireNamespace("fastICA")) 
            stop("install fastICA and try again")
    }
    if (yaiMethod == "randomForest") {
        if (!requireNamespace("randomForest")) 
            stop("install randomForest and try again")
    }
    if (yaiMethod == "msnPP") {
        if (!requireNamespace("ccaPP")) 
            stop("install ccaPP and try again")
    }
	if (method == "delVars") {
		allErr <- unlist(myapply(1:max(1, nboot), function(i, 
								x, y, wts, yaiMethod, ...) suppressWarnings(grmsd2(one = suppressWarnings(yai(x = x, 
															y = y, method = yaiMethod, bootstrap = bootstrap, 
															bootstrap, ...)), ancillaryData = y, wts = wts)), 
						x, y, wts, yaiMethod, bootstrap, ...))
		if (trace) 
			cat("With all vars, mean grmsd (over boostraps) = ", 
					mean(allErr), "; stddev=", sd(allErr), "; Num cols = ", 
					ncol(x), "\n", sep = "")
		xa <- x
		selvars <- list(None = allErr)
		while (ncol(xa) > 1) {
			err <- list()
			for (var in 1:ncol(xa)) err[[var]] <- unlist(myapply(1:max(1, 
										nboot), function(i, xa, y, wts, var, yaiMethod, 
										bootstrap, ...) suppressWarnings(grmsd2(one = suppressWarnings(yai(x = xa[, 
																			-var, drop = FALSE], y = y, method = yaiMethod, 
																	bootstrap = bootstrap, ...)), ancillaryData = y, 
													wts = wts)), xa, y, wts, var, yaiMethod, bootstrap,...))
			names(err) <- names(xa)
			del <- which.min(unlist(lapply(err, mean)))
			selvars[[names(del)]] <- as.vector(unlist(err[del]))
			xa <- xa[, -del, drop = FALSE]
			remVars <- colnames(xa)
			if (trace) 
				cat("Delete var= ", names(del), "; mean grmsd (over boostraps) = ", 
						mean(err[[del]]), "; stddev=", sd(err[[del]]), 
						"; Num cols remaining= ", ncol(xa), "\n", sep = "")
		}
	}
	else if (method == "addVars") {
		remVars <- NULL
		selvars <- list()
		keep <- NULL
		while (length(keep) < ncol(x)) {
			err <- list()
			for (var in setdiff(names(x), keep)) {
				xa <- x[, c(keep, var), drop = FALSE]
				err[[var]] <- unlist(myapply(1:max(1, nboot), 
								function(i, xa, y, wts, yaiMethod, bootstrap, 
										...) suppressWarnings(grmsd2(one = suppressWarnings(yai(x = xa, 
																	y = y, method = yaiMethod, bootstrap = bootstrap, 
																	...)), ancillaryData = y, wts = wts)), xa, 
								y, wts, yaiMethod, bootstrap, ...))
			}
			add <- names(which.min(unlist(lapply(err, mean))))
			selvars[[add]] <- as.vector(unlist(err[add]))
			keep <- c(keep, add)
			if (trace) 
				cat("Added var= ", add, "; mean grmsd (over boostraps) = ", 
						mean(err[[add]]), "; stddev=", sd(err[[add]]), 
						"; Num cols being used= ", ncol(xa), "\n", 
						sep = "")
		}
	}
   
 err <- lapply(selvars, function(x) c(mean(x), sd(x)))
    rn <- names(err)
    err <- matrix(unlist(err), ncol = 2, byrow = TRUE)
    rownames(err) <- rn
    colnames(err) <- c("mean", "sd")
    rtn <- list(call = cl, grmsd = err, allgrmsd = selvars, method = method)
    if (!is.null(remVars)) 
        rtn$remVars <- remVars
    class(rtn) <- c("varSel", "list")
    rtn
}


Jaccard<-function(ob,pr){

	mins<-rowSums(pmin(ob,pr))
	maxs<-rowSums(pmax(ob,pr))
	ifelse(mins==0&maxs==0,0,1-mins/maxs)
}

grmsd2<-function (..., ancillaryData = NULL, vars = NULL, wts = NULL, 
		rtnVectors = FALSE) 
{
	if (missing(...)) 
		stop("... required")
	args <- list(...)
	argLabs <- as.list(substitute(list(...)))[-1]
	names(args) <- if (is.null(names(argLabs))) 
				unlist(argLabs)
			else {
				fixNames <- names(argLabs) == ""
				names(argLabs)[fixNames] <- argLabs[fixNames]
				names(argLabs)
			}
	okClasses <- c("yai", "impute.yai", "data.frame", "matrix", 
			"lm")
	if (!is.null(wts)) {
		if (any(wts < 0) || sum(wts) <= 0) 
			stop("wts must be positive and sum > 0")
	}
	mgd <- list()
	for (objName in names(args)) {
		object <- args[[objName]]
		if (!inherits(object, okClasses)) {
			warning("object ", objName, " class is not one of ", 
					paste(okClasses, collapse = ", "))
			next
		}
		if (inherits(object, "yai")) 
			object <- impute.yai(object, ancillaryData = ancillaryData, 
					vars = vars, observed = TRUE,method="dstWeighted")
		if (inherits(object, "lm")) {
			pr <- predict(object)
			ob <- pr + resid(object)
			if (is.null(dim(pr))) {
				object <- cbind(pr, ob)
				colnames(object) = c(objName, paste0(objName, 
								".o"))
			}
			else {
				colnames(ob) = paste0(colnames(ob), ".o")
				object <- cbind(pr, ob)
			}
		}
		object <- na.omit(object)
		if (nrow(object) == 0) {
			warning("argument ", objName, " has no rows.")
			next
		}
		if (inherits(object, "matrix") & mode(object) != "numeric") {
			warning("argument ", objName, " must be numeric.")
			next
		}
		facts = if (inherits(object, "matrix")) 
					FALSE
				else unlist(lapply(object, is.factor))
		if (any(facts)) {
			if (all(facts)) {
				warning("all variables are factors in ", objName)
				next
			}
			else {
				nams <- names(facts)[facts]
				nams <- nams[-grep("[.]o$", nams)]
				warning("factor(s) have been removed from ", 
						objName, ": ", paste0(nams, collapse = ", "))
				object <- object[, !facts, drop = FALSE]
			}
		}
		useVars <- if (is.null(vars)) 
					colnames(object)
				else {
					ov <- grep("[.]o$", vars)
					ov <- if (length(ov) == 0) 
								unique(c(vars, paste0(vars, ".o")))
							else vars
					intersect(ov, colnames(object))
				}
		if (length(useVars) == 0) {
			warning("needed variables not found in ", objName)
			next
		}
		ov = useVars[grep("[.]o$", useVars)]
		if (length(ov) == 0) {
			warning("no observed variables found in ", objName)
			next
		}
		pv <- unique(sub("[.]o$", "", ov))
		pv <- intersect(pv, useVars)
		if (length(pv) == 0) {
			warning("nothing to compute in ", objName)
			next
		}
		ob <- as.matrix(object[, ov, drop = FALSE])
		pr <- as.matrix(object[, pv, drop = FALSE])
		
#		Start here
#		qr <- qr(ob)
#		uvars <- qr$pivot[1:qr$rank]
#		if (length(uvars) < length(ov)) 
#			warning("rank deficiency in ", objName, " was addressed by removing: ", 
#					paste0(c(colnames(ob)[qr$pivot[(qr$rank + 1):length(qr$pivot)]], 
#									colnames(pr)[qr$pivot[(qr$rank + 1):length(qr$pivot)]]), 
#							collapse = ", "))
#		p <- solve(chol(cov(ob[, uvars, drop = FALSE])))
#		ob <- as.matrix(ob[, uvars]) %*% p
#		pr <- as.matrix(pr[, uvars]) %*% p
#		wt <- wts
#		wt <- if (is.null(wt)) 
#					rep(1, ncol(pr))
#				else {
#					if (length(names(wt)) > 0) {
#						
#						names(wt) <- sub("[.]o$", "", names(wt))
#						wt <- na.omit(wt[names(pr)])
#					}
#					if (length(wt) != ncol(pr)) {
#						warning("weights do not match variables in ", 
#								objName, " and were ignored.")
#						wt <- rep(1, ncol(pr))
#					}
#					wt
#				}	
#		Finish here

		mgd[[objName]] <- if (rtnVectors) 
			Jaccard(ob,pr)
		else mean(Jaccard(ob,pr))
	}
	if (rtnVectors) {
		idx <- sort(unlist(lapply(mgd, function(x) mean(x))), 
				index.return = TRUE)$ix
		mgd[idx]
	}
	else sort(unlist(mgd))
	
}


#Reynolds' error index

varSelection3<-function (x, y, method = "addVars", yaiMethod = "msn", wts = NULL, 
		nboot = 20, trace = FALSE, useParallel = if (.Platform$OS.type == 
						"windows") FALSE else TRUE, ...) {
	
	if (missing(x)) 
		stop("x must be specified.")
	if (missing(y)) 
		stop("y must be specified.")
	okMethods <- c("addVars", "delVars")
	if (!(method %in% okMethods)) 
		stop("method=\"", method, "\" must be one of: \"", paste0(okMethods, 
						collapse = "\", \""), "\"")
	if (is.null(wts)) 
		wts <- rep(1, ncol(y))
	if (useParallel && .Platform$OS.type != "Windows" && requireNamespace("parallel")) {
		myapply <- parallel::mclapply
	}
	else {
		if (useParallel) 
			warning("package parallel was not loaded and is not being used")
		myapply <- lapply
	}
	cl <- match.call()
	bootstrap <- nboot > 0
	if (yaiMethod == "gnn") {
		if (!requireNamespace("vegan")) 
			stop("install vegan and try again")
	}
	if (yaiMethod == "ica") {
		if (!requireNamespace("fastICA")) 
			stop("install fastICA and try again")
	}
	if (yaiMethod == "randomForest") {
		if (!requireNamespace("randomForest")) 
			stop("install randomForest and try again")
	}
	if (yaiMethod == "msnPP") {
		if (!requireNamespace("ccaPP")) 
			stop("install ccaPP and try again")
	}
	if (method == "delVars") {
		allErr <- unlist(myapply(1:max(1, nboot), function(i, 
								x, y, wts, yaiMethod, ...) suppressWarnings(grmsd3(one = suppressWarnings(yai(x = x, 
															y = y, method = yaiMethod, bootstrap = bootstrap, 
															bootstrap, ...)), ancillaryData = y, wts = wts)), 
						x, y, wts, yaiMethod, bootstrap, ...))
		if (trace) 
			cat("With all vars, mean grmsd (over boostraps) = ", 
					mean(allErr), "; stddev=", sd(allErr), "; Num cols = ", 
					ncol(x), "\n", sep = "")
		xa <- x
		selvars <- list(None = allErr)
		while (ncol(xa) > 1) {
			err <- list()
			for (var in 1:ncol(xa)) err[[var]] <- unlist(myapply(1:max(1, 
										nboot), function(i, xa, y, wts, var, yaiMethod, 
										bootstrap, ...) suppressWarnings(grmsd3(one = suppressWarnings(yai(x = xa[, 
																			-var, drop = FALSE], y = y, method = yaiMethod, 
																	bootstrap = bootstrap, ...)), ancillaryData = y, 
													wts = wts)), xa, y, wts, var, yaiMethod, bootstrap,...))
			names(err) <- names(xa)
			del <- which.min(unlist(lapply(err, mean)))
			selvars[[names(del)]] <- as.vector(unlist(err[del]))
			xa <- xa[, -del, drop = FALSE]
			remVars <- colnames(xa)
			if (trace) 
				cat("Delete var= ", names(del), "; mean grmsd (over boostraps) = ", 
						mean(err[[del]]), "; stddev=", sd(err[[del]]), 
						"; Num cols remaining= ", ncol(xa), "\n", sep = "")
		}
	}
	else if (method == "addVars") {
		remVars <- NULL
		selvars <- list()
		keep <- NULL
		while (length(keep) < ncol(x)) {
			err <- list()
			for (var in setdiff(names(x), keep)) {
				xa <- x[, c(keep, var), drop = FALSE]
				err[[var]] <- unlist(myapply(1:max(1, nboot), 
								function(i, xa, y, wts, yaiMethod, bootstrap, 
										...) suppressWarnings(grmsd3(one = suppressWarnings(yai(x = xa, 
																	y = y, method = yaiMethod, bootstrap = bootstrap, 
																	...)), ancillaryData = y, wts = wts)), xa, 
								y, wts, yaiMethod, bootstrap, ...))
			}
			add <- names(which.min(unlist(lapply(err, mean))))
			selvars[[add]] <- as.vector(unlist(err[add]))
			keep <- c(keep, add)
			if (trace) 
				cat("Added var= ", add, "; mean grmsd (over boostraps) = ", 
						mean(err[[add]]), "; stddev=", sd(err[[add]]), 
						"; Num cols being used= ", ncol(xa), "\n", 
						sep = "")
		}
	}
	
	err <- lapply(selvars, function(x) c(mean(x), sd(x)))
	rn <- names(err)
	err <- matrix(unlist(err), ncol = 2, byrow = TRUE)
	rownames(err) <- rn
	colnames(err) <- c("mean", "sd")
	rtn <- list(call = cl, grmsd = err, allgrmsd = selvars, method = method)
	if (!is.null(remVars)) 
		rtn$remVars <- remVars
	class(rtn) <- c("varSel", "list")
	rtn
}

Reynolds<-function(ob,pr){
	
	rowSums(abs(ob-pr))
#	maxs<-rowSums(pmax(ob,pr))
#	ifelse(mins==0&maxs==0,0,1-mins/maxs)
}

grmsd3<-function (..., ancillaryData = NULL, vars = NULL, wts = NULL, 
		rtnVectors = FALSE) 
{
	if (missing(...)) 
		stop("... required")
	args <- list(...)
	argLabs <- as.list(substitute(list(...)))[-1]
	names(args) <- if (is.null(names(argLabs))) 
				unlist(argLabs)
			else {
				fixNames <- names(argLabs) == ""
				names(argLabs)[fixNames] <- argLabs[fixNames]
				names(argLabs)
			}
	okClasses <- c("yai", "impute.yai", "data.frame", "matrix", 
			"lm")
	if (!is.null(wts)) {
		if (any(wts < 0) || sum(wts) <= 0) 
			stop("wts must be positive and sum > 0")
	}
	mgd <- list()
	for (objName in names(args)) {
		object <- args[[objName]]
		if (!inherits(object, okClasses)) {
			warning("object ", objName, " class is not one of ", 
					paste(okClasses, collapse = ", "))
			next
		}
		if (inherits(object, "yai")) 
			object <- impute.yai(object, ancillaryData = ancillaryData, 
					vars = vars, observed = TRUE,method="dstWeighted")
		if (inherits(object, "lm")) {
			pr <- predict(object)
			ob <- pr + resid(object)
			if (is.null(dim(pr))) {
				object <- cbind(pr, ob)
				colnames(object) = c(objName, paste0(objName, 
								".o"))
			}
			else {
				colnames(ob) = paste0(colnames(ob), ".o")
				object <- cbind(pr, ob)
			}
		}
		object <- na.omit(object)
		if (nrow(object) == 0) {
			warning("argument ", objName, " has no rows.")
			next
		}
		if (inherits(object, "matrix") & mode(object) != "numeric") {
			warning("argument ", objName, " must be numeric.")
			next
		}
		facts = if (inherits(object, "matrix")) 
					FALSE
				else unlist(lapply(object, is.factor))
		if (any(facts)) {
			if (all(facts)) {
				warning("all variables are factors in ", objName)
				next
			}
			else {
				nams <- names(facts)[facts]
				nams <- nams[-grep("[.]o$", nams)]
				warning("factor(s) have been removed from ", 
						objName, ": ", paste0(nams, collapse = ", "))
				object <- object[, !facts, drop = FALSE]
			}
		}
		useVars <- if (is.null(vars)) 
					colnames(object)
				else {
					ov <- grep("[.]o$", vars)
					ov <- if (length(ov) == 0) 
								unique(c(vars, paste0(vars, ".o")))
							else vars
					intersect(ov, colnames(object))
				}
		if (length(useVars) == 0) {
			warning("needed variables not found in ", objName)
			next
		}
		ov = useVars[grep("[.]o$", useVars)]
		if (length(ov) == 0) {
			warning("no observed variables found in ", objName)
			next
		}
		pv <- unique(sub("[.]o$", "", ov))
		pv <- intersect(pv, useVars)
		if (length(pv) == 0) {
			warning("nothing to compute in ", objName)
			next
		}
		ob <- as.matrix(object[, ov, drop = FALSE])
		pr <- as.matrix(object[, pv, drop = FALSE])
		
#		Start here
#		qr <- qr(ob)
#		uvars <- qr$pivot[1:qr$rank]
#		if (length(uvars) < length(ov)) 
#			warning("rank deficiency in ", objName, " was addressed by removing: ", 
#					paste0(c(colnames(ob)[qr$pivot[(qr$rank + 1):length(qr$pivot)]], 
#									colnames(pr)[qr$pivot[(qr$rank + 1):length(qr$pivot)]]), 
#							collapse = ", "))
#		p <- solve(chol(cov(ob[, uvars, drop = FALSE])))
#		ob <- as.matrix(ob[, uvars]) %*% p
#		pr <- as.matrix(pr[, uvars]) %*% p
#		wt <- wts
#		wt <- if (is.null(wt)) 
#					rep(1, ncol(pr))
#				else {
#					if (length(names(wt)) > 0) {
#						
#						names(wt) <- sub("[.]o$", "", names(wt))
#						wt <- na.omit(wt[names(pr)])
#					}
#					if (length(wt) != ncol(pr)) {
#						warning("weights do not match variables in ", 
#								objName, " and were ignored.")
#						wt <- rep(1, ncol(pr))
#					}
#					wt
#				}	
#		Finish here
		
		mgd[[objName]] <- if (rtnVectors) 
					Reynolds(ob,pr)
				else mean(Reynolds(ob,pr))
	}
	if (rtnVectors) {
		idx <- sort(unlist(lapply(mgd, function(x) mean(x))), 
				index.return = TRUE)$ix
		mgd[idx]
	}
	else sort(unlist(mgd))
	
}

bestVarsabs<-function (obj, nbest = NULL,abs_min=FALSE) 
{
	if (missing(obj)) 
		stop("obj must be present")
	if (!inherits(obj, "varSel")) 
		stop("class of obj must be varSel")
	grmsd <- switch(obj$method, addVars = obj$grmsd[, 1], delVars = rev(obj$grmsd[2:nrow(obj$grmsd), 
							1]), stop("method '", obj$method, "' not found in obj"))
	
	vars <- if (!is.null(obj$remVars)) 
				c(obj$remVars, names(grmsd))
			else names(grmsd)
	
	if (is.null(nbest)) {
		
		if(abs_min==FALSE){
			
			le <- length(grmsd)
			nbest <- if (le > 2) {
						
						s <- (grmsd[le] - grmsd[1])/le
						ss <- unlist(lapply(2:(le - 1), function(i, ss)
											(ss[i -1] + ss[i])/2, diff(grmsd)))
						sb <- abs(s) > abs(ss)
						if (any(sb)) 
							min(le, which.max(sb) + 1)
						else le
					}
					else le
			
		}else{
		
			nbest<-min(which(grmsd==min(grmsd)))
		
		}
	}

	vars[1:min(nbest, length(vars))]
}

grmsd_obs_pred<-function(ob,pr,wts=NULL,rtnVectors = FALSE){

	ov<-dim(ob)[2]
	if(ov==1){return(sqrt(mean((ob-pr)^2,na.rm=TRUE)))}
	qr <- qr(ob)
	uvars <- qr$pivot[1:qr$rank]
	if (length(uvars) < ov) 
		warning("rank deficiency in was addressed by removing: ", 
				paste0(c(colnames(ob)[qr$pivot[(qr$rank + 1):length(qr$pivot)]], 
								colnames(pr)[qr$pivot[(qr$rank + 1):length(qr$pivot)]]), 
						collapse = ", "))
	p <- solve(chol(cov(ob[, uvars, drop = FALSE])))
	ob <- as.matrix(ob[, uvars]) %*% p
	pr <- as.matrix(pr[, uvars]) %*% p
	wt <- wts
	wt <- if (is.null(wt)) 
				rep(1, ncol(pr))
			else {
				if (length(names(wt)) > 0) {
					names(wt) <- sub("[.]o$", "", names(wt))
					wt <- na.omit(wt[names(pr)])
				}
				if (length(wt) != ncol(pr)) {
					warning("weights do not match variables and were ignored.")
					wt <- rep(1, ncol(pr))
				}
				wt
			}
	wt <- wt/sum(wt)
	md <- apply((pr - ob), 1, function(x, wt) sum((x^2) * 
								wt), wt)
	
	if (rtnVectors) {
		
		return(sqrt(md))
	}
	else return(sqrt(mean(md)))
	
	
}

Iindex<-function(ob,pr){
	
	rowSums((ob-pr)^2)
	
}

© 2022 GitHub, Inc.
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
