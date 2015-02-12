
create.factor <- function(x, y, type = "categorical", cuts, q, useNA = TRUE, range = c(0, Inf), subset = 1:length(x), plot = TRUE) {
	# Create a factor variable.
	#
	#
	# Parameters:
	# 		x - variable to be converted to factor
	# 		y - response variable (optional)
  #     type - "categorical", "nominal", "numeric".
	#			q - quantiles to split numeric (and optionally nominal) variables, instead of given cuts.
	#			cuts - used for discretizing numerical and nominal variables. Given as a vector of values.
	#     useNA - whether to make NA values a separate category. Defaults to TRUE.
	#		  subset - susbet of the variables to calculate weight of evidence on. Returns entire variable as factor.
	#			plot - whether a histogram and a woe plot should be plotted.
	#
	# Output:
	#		A factor variable.
	
	#   if (is.factor(x)) {
	#     print ("Variable is already a factor variable. Perhaps you need to regroup the levels?")
	#     return
	#   }
  
  if (!(type %in% c("categorical", "nominal", "numeric"))) {
  	print(paste("Unknown variable type: ", type, ". Please specify one of \'categorical\', \'nominal\', \'numeric\'.", sep = ""))
  	return
  }
  
  if (missing(cuts) & missing(q) & (type %in% c("numeric"))) {
  	q = 10
  }
  
  if (!missing(cuts) & (type %in% c("numeric", "nominal"))) {
  	x.factor <- factor(cut2(x = x, cuts = cuts))
  	
  } else if (!missing(q) & (type %in% c("numeric", "nominal"))) {
  	cuts = quantile(x[subset], probs = seq(from = 0, to = 1, length.out = q + 1), type = 8, na.rm = TRUE)
  	cuts[1] <- range[1]
  	cuts[length(cuts)] <- range[2]
  	cuts = unique(cuts)					# dodano naknadno i netestirano
  	x.factor <- factor(cut(x = x, breaks = cuts, right = FALSE, include.lowest = TRUE))
  
  }	else if (type == "numeric") {
  	x.factor <- factor(cut2(x = x, g = q))
  	
  } else if (type == "nominal") {
  	x.factor <- factor(x, levels = sort(unique(x)))
  	
  } else if (missing(y) & (type %in% c("character", "categorical"))) {
		# if no response variable is given than just wrap a factor variable around
		factor.levels <- sort(unique(x))
		x.factor <- factor(x, levels = factor.levels)
		
	} else if (!missing(y) & (type %in% c("character", "categorical"))) { 
		# if response variable is given then order the factors by weight of evidence
		woe <- woe.calculate(x = x, y = y, subset = subset)$woe
		factor.levels <- woe$Category[order(woe$woe)]
		x.factor <- factor(x, levels = factor.levels)
	}
	
  
	# 	if (exists("factor.levels")) {
	# 		if (type != "nominal") {
	# 				x.factor <- factor(x, levels = factor.levels)
	# 			} else {
	# 				x.factor <- factor(x, levels = factor.levels, ordered = TRUE)
	# 			}
	# 	}
	
	# make NA values a new category if exist
  if (anyNA(x)) {
    levels(x.factor) <- c(levels(x.factor), "NA")
    x.factor[is.na(x.factor)] <- "NA"
  }
	
	# plot if enabled
	if (plot & !missing(y)) {
		hist.woe.plot(x = x.factor, y = y, subset=subset, angle.x = ifelse(nchar(paste(levels(x.factor), collapse="")) > 50, 90, 0))
	}
	
	return (x.factor)
}


woe.calculate <- function(x, y, subset = 1:length(x)) {
	# Calculate weight of evidence (woe).
	#
	# Parameters:
	#		x - factor variable to calculate woe of.
	#		y - response variable (binary).
	#		subset - susbet of the variables to calculate weight of evidence on. 
	#
	# Output:
	#		A list of two members: woe data frame and information value (IV)

	# woe calculcation
	tab <- table(x[subset], y[subset], useNA = "ifany")
	distr <- table(y[subset])
	woe.values <- log(tab[,1] / distr[1] / (tab[,2] / distr[2]))
	#woe.values <- ifelse(is.infinite(woe.values) | is.na(woe.values), 0, woe.values) # if some values are nan or inf set woe to zero
	woe.values <- sapply(woe.values, function(x) {
		if (is.nan(x)) return(0)
		if (is.infinite(x)) {
			if (sign(x) < 0) return (min(woe.values[!is.infinite(woe.values)], na.rm = T) * 1.25)
			if (sign(x) > 0) return (max(woe.values[!is.infinite(woe.values)], na.rm = T) * 1.25)
		}
		return (x)
	})
	woe <- data.frame("Category" = factor(names(woe.values), levels = names(woe.values)), "woe" = woe.values, row.names=NULL) # wrap to data frame
	
	# IV calulcation
	IV <- sum((tab[,1]/distr[1] - tab[,2]/distr[2]) * woe$woe)
	
	return (list(woe=woe, IV=IV))
}



refactor <- function(x, y = NULL, type = "alphabet", subset = 1:length(x), plot = TRUE) {
	# Change the level order in a factor variable to alphabetical, woe or nominal order.
	#
	# Parameters:
	# 		x - factor variable to be refactored
	# 		y - response variable used in the "woe" type refactor
	#     type - "alphabet" DEFAULT, arranges factor levels by alphabet. Other choice is "woe" which sorts levels
	#           by increasing weight of evidence. Option "woe" requires y, "nominal" sorts integer factors.
	#     subset - subset of the variables to calculate weight of evidence on. Returns entire variable as factor.
	#
	# Output:
	#		A factor variable.
	#		Optionally produces a histogram and a woe plot.
	
	if (type == "alphabet") {
		levels <- levels(x)[order(levels(x))]
	} else if (type == "woe") {
		levels <- levels(x)[order(woe.calculate(x, y, subset=subset)$woe$woe)]
	} else if (type == "nominal") {
	  levels <- levels(x)[order(as.integer(levels(x)))]
	}

	x.factor <- factor(x, levels=levels)
	
	if (plot == TRUE) {
		hist.woe.plot(x = x.factor, y = y, subset=subset, angle.x = ifelse(nchar(paste(levels(x.factor), collapse="")) > 50, 90, 0))
	}
	
	return (x.factor)
}


regroup.factor <- function(x, y, groups, mergeNA, type = "alphabet", name = "Group", factor.levels = NULL, subset = 1:length(x), plot = TRUE) {
  # Change factor levels in a factor variable.
  #
  # Parameters:
  #     x - factor variable. If ordered, the new variable will also be ordered.
  #     y - response variable used to calculate weight of evidence
  #     groups - a list of vectors giving new factor levels by indices of old. For ordered, smallest is group[[1]].
  #     mergeNA - how to merge the NA category. Can be 'first', 'last' or the exact number of group.
	#			type - "alphabet" default, takes levels in the alphabetical order.
  #     name - names of new factor groups. If no name given groups will be named "Group_i"
  #     factor.levels - new names of factor levels (instead of default Group_i).
	#			subset - subset of variables to use as reference (usually training dataset).
  
  
	if (!missing(mergeNA)) {
		if (!any(levels(x) == "NA")) {
			print ("No 'NA' category.")
			return (NULL)
		}
		
		x.new <- x
		levels <- levels(x.new)

		if (mergeNA == "first") {
			index <- min(which(levels != "NA"))
			
		} else if (mergeNA == "last") {
			index <- max(which(levels != "NA"))

		} else if (is.numeric(mergeNA)) {
			if (as.integer(mergeNA) != mergeNA) {
				print ("Index must be integer. Please input an integer index.")
				return (NULL)
			}
			if (mergeNA == which(levels(x.new) == "NA")) {
				print ("Cannot merge NA with itself - invalid group index.")
				return (NULL)
			} else {
				index = mergeNA
			}
		}
				
		NA_category <- paste(levels[index], ", NA", sep = "")
		levels(x.new)[index] <- NA_category
		levels(x.new)[levels == "NA"] <- NA_category

	} else {
	
	  n.groups <- length(groups)
	  
	  if (is.null(factor.levels)) {
	    factor.levels <- paste(name, 1:n.groups, sep="_")
	  }
	  
	  x.new <- factor(x=rep(factor.levels[1], length(x)), levels=factor.levels, ordered=is.ordered(x))
	  
	  for (i in 2:n.groups) {
	    x.new[x %in% levels(x)[groups[[i]]]] <- factor.levels[i]
	  }
	}
  
	# plot
  if (!missing(y) & plot) {
  	hist.woe.plot(x = x.new, y = y, subset=subset, angle.x = ifelse(nchar(paste(levels(x.new), collapse="")) > 50, 90, 0))
  }
  
  return (x.new)
}






hist.plot <- function(x, name="", angle.x=0, hjust.x=0.5, subset = 1:length(x)) {
	# Plots a histogram (bar plot) of a discrete variable.
	#
	# Parameters:
	#			x - a factor variable
	#			name - (optional) variable name to appear in the plot
	#			angle.x, hjust.x - formatting parameters
	#
	# Outputs:
	#			ggplot handle to the plot.

	if (!is.data.frame(x)) {
		df <- data.frame(x=x)
	}

	
	df$miss <- factor(ifelse(x!="NA", "isnotNA", "NA"))
	
	
	p <- ggplot(df[subset,]) + 
		geom_bar(aes(x = x, fill = miss)) +
		scale_fill_manual(values = c("grey20", "#CD5C5C")) +
		guides(fill = FALSE) +		# guide removes legend
		labs(title = "Histogram") + 
	  theme(axis.text.x = element_text(hjust = hjust.x, size = 12, angle = angle.x)) + 
	  theme(axis.text.y = element_text(hjust = hjust.x, size = 12)) +
	  theme(axis.title.x = element_text(angle = 0, hjust = .5, size = 12)) + 
	  theme(axis.title.y = element_text(angle = 90, vjust = 1, size = 12)) +
    ylab("Percentage")
		
	return (p)
}


woe.plot <- function(x, y, name="", angle.x = 0, hjust.x = 0.5, subset = 1:length(x)) {
	# Plots a weight of evidence graph of a factor variable. Information value is plotted as a title.
	#
	# Parameters:
	#			x - a factor variable
	#			name - (optional) variable name to appear in the plot
	#			angle.x, hjust.x - formatting parameters
	#			subset - 
	#
	# Outputs:
	#			ggplot handle to the plot.
	
	woe.IV <- woe.calculate(x[subset], y[subset])
	
	df <- woe.IV$woe
	IV <- woe.IV$IV
	
	df$miss <- factor(ifelse(df$Category != "NA", "isnotNA", "NA"))
	
	p <- ggplot(df, aes(x = Category, y = woe)) + 
		geom_point(aes(group = miss, color = miss), size = 4) + 
		scale_color_manual(values = c("grey20", "#CD5C5C")) +
		guides(color = FALSE) + 
		geom_line(size = .8, aes(group = miss), color = "grey20") + 
		labs(title = paste("Information Value = ", sprintf("%3.4f", IV), sep = " ")) + 
		xlab(name) + 
		ylab("Weight of evidence") + 
		theme(axis.text.x = element_text(hjust = hjust.x, size = 12, angle = angle.x)) + 
		theme(axis.text.y = element_text(size = 12)) +
		geom_hline(color="grey20")
	
	return (p)
}



hist.woe.plot <- function(x, y, name="", angle.x=0, hjust.x=0.5, subset=rep(TRUE, length(x))) {
	#	Combines and plots the histogram and woe plots of a discretized variable.
	
	grid.arrange(hist.plot(x[subset], name, angle.x, hjust.x), 
					 woe.plot(x[subset], y[subset], name, angle.x, hjust.x), ncol=2)
}



split.data <- function(x, split.ratio = c(.7, 0, .3), ratio = 0.1, oversample = TRUE, seed = NULL) {
	# FSplitting data into modeling, validation and testing datasets. Does oversampling if specified.
	# oversamoling included
	# oversample - postotak na koji treba napraviti oversample, pretpostavlja se  nebalansiran dataset
	
	# 	if ( (!(goal %in% names(data)) ) {
	# 		print("Ne postoji ciljna varijabla. Terminate.")
	# 		break
	# 	}
	
	#set seed
	if (!is.null(seed)) {
		set.seed(seed)
	}
	
	if (any(split.ratio < 0)) {
		print("split ratios cannot be < 0")
		return
	}
	
	names = c("train", "valid", "test")[which(split.ratio > 0)]
	
	s <- sample(x = 1:length(x), length(x), replace = FALSE)
	d <- data.frame(x = x, sample = s)
	
	def.rate <- mean(x)
	def.count <- sum(x)
	nondef.count <- sum(1 - x)
	
	split.ratio <- split.ratio[split.ratio>0]
	
	wh <- dlply(d, .(x),
							function(d) {
								
								TOT = nrow(d)
								
								split.count <- round(split.ratio * TOT)
								split.count[length(split.count)] <- TOT - sum(split.count[-length(split.count)])
								
								spl <- cumsum(c(1, (split.count)))
								
								
								aaply(1:(length(spl) - 1), 1, 
											function(i) {
												#list(sample(d$s[(spl[i]):(spl[i+1]-1)], split.count[i], replace = FALSE))
												#list(d$s[(spl[i]):(spl[i+1]-1)])
												if (oversample & (d$x[1] == 1)) {
													x <- (d$sample[(spl[i]):(spl[i+1]-1)])
													x <- list(sample(x, length(x) / ratio, replace = T))
													return (x)
												
												} else if (oversample & (d$x[1] == 0)) {
													x <- (d$sample[(spl[i]):(spl[i+1]-1)])
													x <- list(sample(x, round(split.ratio[i] * def.count / ratio) * (1-ratio) / ratio, replace = T))
													return (x)
												
												} else if (!oversample & (d$x[1] == 1)) {
													return (list(d$sample[(spl[i]):(spl[i+1]-1)]))
													
												} else if (!oversample & (d$x[1] == 0)) {
													x <- (d$sample[(spl[i]):(spl[i+1]-1)])
													x <- list(sample(x, round(split.ratio[i] * def.count) * (1-ratio) / ratio, replace = T))
													return (x) 
												
												} else {
													return (list(d$sample[(spl[i]):(spl[i+1]-1)]))
												}
											})
							})
	
	wh <- llply(dlply(melt(wh), .(L2), plyr::summarize, x = value), function(x) {x[,1]})
	names(wh) <- names
	
	return (wh)
}


gini <- function(y, y.hat, partition) {
	#	Calculate the Gini index.
	#
	# Parameters:
	#			y - actual response variable.
	#			yhat - predicted probabilities of events.
	#     partition - variable with partitions of the data.
	#
	# Outputs:
	#			Gini - calculated Gini index.
	
	if (missing(partition)) {
		f.ply <- laply
		partition <- list(1:length(y))
	} else {
		f.ply <- ldply
	}
	
	
	Gini <- f.ply(partition, function(part) {
									o = order(y.hat[part])
									y.part = y[part][o]
									
									N <- sum(y.part)
									Tot <- length(o)
									
									B <- cumsum(y.part) / N
									G <- cumsum(1 - y.part) / (Tot - N)
									
									return (c(Gini = sum((G[-1] + G[-Tot]) * (B[2:(Tot)] - B[1:(Tot-1)])) - 1))
								}, .id = "partition")
	
	return (Gini)
}



ROC.calculate <- function (y, y.hat, n) {
	# Calculates ROC curve. FPR (x axis) is proportion of negatives, TPR (y axis) is proportion of positives.
	#
	#	Parameters:
	#			y - actual binary response.
	#			y.hat - predicted probabilities of events.
	#			n - number of points to calculate ROC curve at.
	#
	#	Outputs:
	#			A data.frame containing ROC curve values (FPR and TPR).
	
	o = order(y.hat)
	y = y[o]
	
	N <- sum(y)
	Tot <- length(y)
	
	x.plot <- cumsum(y) / N
	y.plot <- cumsum(1 - y) / (Tot - N)
	
	subset <- if(!missing(n)) round(seq(1, length(y), length.out = n)) else 1:length(y)

	return (data.frame(x = x.plot, y = y.plot)[subset,])
}


ROC.curve <- function(y, y.hat, partition, n = 50, return.plot = FALSE) {
	#	Plot a ROC curve.  
	#
	#	Parameters:
	#			y - actual binary response.
	#			yhat - predicted probabilities of events.
	#			partition - variable with partitions of the data.
	#			n - number of points to plot ROC curve at.
	#
	#	Outputs:
	#			Plot handle.
	
	df.plot <- ldply(partition,
									 function(part) {
									 	return (data.frame(ROC.calculate(y = y[part], y.hat = y.hat[part], n = n)))
									 }, .id = "partition")
	


	p <- ggplot(df.plot, aes(x = x, y = y, color = partition)) +
	geom_line(size = 1) + 
	ylim(c(0,1)) + 
	xlab("False Positive Rate") + 
	ylab("True Positive Rate") + 
	labs(title = "ROC Curve") + 
	guides(fill = guide_legend(title = "Legend")) # no title
	
	plot(p)

	if (return.plot) return (p)
	
}


stab.bins <- function(y, y.hat, n = 10) {
	#	Calculate bins for population stability plots.
	#
	#	Parameters:
	#			y - actual binary response.
	#			yhat - predicted probabilities of events.
	#			n - number of stability bins.
	#
	#	Outputs:
	#			A summary array with data divided into n equally distributed bins: number of cases in each bin,
	#			number of each response category by bins, proportions of each of the calculated categories and cuts.
	
	q <- quantile(x = y.hat, probs = seq(0, 1, length.out = n + 1))
	q[1] <- 0
	q[length(q)] = 1
	# names(q)[laply(by(1:length(q), q, I), min)]
	bin <- rep(0, n)
	def_bin <- rep(0, n)
	
	for (i in 1:(length(q))) {
		if (i == 1) {
			bin[i] <- length(y[y.hat <= q[i]])
			def_bin[i] <- sum(y[y.hat <= q[i]])
		} else {
			bin[i] <- length(y[(y.hat > q[i-1]) & (y.hat <= q[i])])
			def_bin[i] <- sum(y[(y.hat > q[i-1]) & (y.hat <= q[i])])
		}
	}
	
	ret <- daply(data.frame(y = y, y.hat = y.hat), .(y), 
							 function(d) {
							 	table(cut(d$y.hat, breaks = q, labels = names(q)[-1]))
							 	})
	
	ret <- rbind(total = colSums(ret), 
							 ret, 
							 aaply(ret, 1, function(x) { x/sum(x) }), 
							 proportion.bin = colSums(ret) / sum(ret),
							 cuts = q[-1])
	ret <- t(ret)
	
	colnames(ret)[2:3] <- paste("count", colnames(ret)[2:3], sep=".")
	colnames(ret)[4:5] <- paste("proportion", colnames(ret)[4:5], sep=".")
	
# 	ret <- cbind(count = bin, default_count = def_bin, percentile = bin_percentiles, 
# 										cuts = q, def_rate = def_bin / bin)
	
	return(ret)
}


stability.plot <- function(y, y.hat, partition, n = 10, plot = TRUE, output = FALSE, train = "train", test = "test") {
	#	Calculate and plot population stability plots.
	#
	#	Parameters:
	#			y - actual binary response.
	#			yhat - predicted probabilities of events.
	#			partition - variable with partitions of the data.
	#			n - number of bins.
	#
	#	Outputs:
	#			(optional) stability bins
	
# 	part = which(partition %in% c(train, test))
	
# 	stab <- ddply(data.frame(y = y, y.hat = y.hat, partition = partition)[part,], .(partition), 
# 								function(d) {
# 									stab.bins(y = d$y, y.hat = d$y.hat, n)[,"proportion.bin"]
# 								})

	partition = partition[which(names(partition) %in% c(train, test))]
	
	stab <- ldply(partition, 
								function(part) {
									stab.bins(y = y[part], y.hat = y.hat[part], n)[,"proportion.bin"]
								}, .id = "partition")
	
	df.plot <- melt(stab, id.vars = "partition")
	
	stab.index <- sum((df.plot[df.plot$partition == test, "value"] - df.plot[df.plot$partition == train, "value"]) * 
								(log(df.plot[df.plot$partition == test, "value"] / df.plot[df.plot$partition == train, "value"])))
		
	if (plot) {
		p <- ggplot(df.plot, aes(x = variable, y = value, fill = partition, group = partition)) +
			geom_bar(stat = "Identity", position = "dodge") +
			ggtitle(label = paste("Stability index = ", format(stab.index, digits = 4), sep="")) +
			xlab("Bins") + 
			ylab("Percentile") + 
			theme(axis.text.x = element_text(hjust = .5)) + 
			scale_y_continuous(labels = percent)
		plot(p)
	}
	
	if (output) {
		ret <- (t(stab[,-1]))
		colnames(ret) = stab[,1]
		return (ret)
	}
}
