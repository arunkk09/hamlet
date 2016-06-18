### discretize.R  (2013-07-15)
###
###    Discretize a continuous random variable
###
### Copyright 2013 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


discretize = function( x, numBins, r=range(x) )
{
  b = seq(from=r[1], to=r[2], length.out=numBins+1 )
  y = table( cut(x, breaks=b , include.lowest=TRUE) )

  return( y )
}

discretize2d = function( x1, x2, numBins1, numBins2, r1=range(x1), r2=range(x2) )
{
  b1 = seq(from=r1[1], to=r1[2], length.out=numBins1+1 )
  b2 = seq(from=r2[1], to=r2[2], length.out=numBins2+1 )

  y2d = table( cut(x1, breaks=b1, include.lowest=TRUE ), 
               cut(x2, breaks=b2, include.lowest=TRUE ) )

  return( y2d )
}



### entropy.ChaoShen.R  (2008-08-20)
###
###    Chao-Shen entropy estimator (2003)
###
### Copyright 2008 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# compute Chao-Shen (2003) entropy estimator

# y is a vector of counts (may include zeros)
entropy.ChaoShen = function(y, unit=c("log", "log2", "log10"))
{
  unit = match.arg(unit)

  yx = y[y > 0]           # remove bins with zero counts
  n = sum(yx)             # total number of counts
  p = yx/n                # empirical frequencies

  f1 = sum(yx == 1)       # number of singletons
  if (f1 == n) f1 = n-1   # avoid C=0

  C = 1 - f1/n            # estimated coverage
  pa = C*p                # coverage adjusted empirical frequencies
  la = (1-(1-pa)^n)       # probability to see a bin (species) in the sample

  H = -sum(pa*log(pa)/la) # Chao-Shen (2003) entropy estimator

  if (unit == "log2")       H = H/log(2)  # change from log to log2 scale
  if (unit == "log10")      H = H/log(10) # change from log to log10 scale

  return(H)
}

### entropy.Dirichlet.R  (2013-07-16)
###
###    Dirichlet prior Bayesian estimators of entropy, mutual
###     information and related quantities
###
### Copyright 2008-13 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# estimate entropy based on Dirichlet-multinomial pseudocount model 

# y:  a vector of counts (may include zeros)
# a:  pseudocount per bin

# some choices for a:
# a = 0          :   empirical estimate
# a = 1          :   Laplace
# a = 1/2        :   Jeffreys
# a = 1/m        :   Schurmann-Grassberger  (m: number of bins)
# a = sqrt(n)/m  :   minimax

entropy.Dirichlet = function(y, a, unit=c("log", "log2", "log10"))
{
  return( entropy.plugin(freqs.Dirichlet(y, a), unit=unit) )
}

freqs.Dirichlet = function(y, a)
{
  ya = y+a          # counts plus pseudocounts
  na = sum(ya)      # total number of counts plus pseudocounts
  pa = ya/na        # empirical frequencies adjusted with pseudocounts

  return(pa)
}


# mutual information
mi.Dirichlet = function(y2d, a, unit=c("log", "log2", "log10"))
{
  f2d = freqs.Dirichlet(y2d, a)
  mi = mi.plugin(f2d, unit=unit)
  return( mi )
}

# chi-squared of independence
chi2indep.Dirichlet = function(y2d, a, unit=c("log", "log2", "log10"))
{
  f2d = freqs.Dirichlet(y2d, a)
  chi2 = chi2indep.plugin(f2d, unit=unit)
  return( chi2 )
}


# chi-squared statistic
chi2.Dirichlet = function(y1, y2, a1, a2, unit=c("log", "log2", "log10"))
{
  f1 = freqs.Dirichlet(y1, a1)
  f2 = freqs.Dirichlet(y2, a2)
  chi2 = chi2.plugin(f1, f2, unit=unit)
  return( chi2 )
}

# KL divergence
KL.Dirichlet = function(y1, y2, a1, a2, unit=c("log", "log2", "log10"))
{
  f1 = freqs.Dirichlet(y1, a1)
  f2 = freqs.Dirichlet(y2, a2)
  KL = KL.plugin(f1, f2, unit=unit)
  return( KL )
}






### entropy.empirical.R  (2013-07-16)
###
###    Empirical estimators of entropy, mutual information and related
###    quantities.
###
### Copyright 2008-13 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# compute empirical entropy 
# y is a vector of counts (may include zeros)
entropy.empirical = function(y, unit=c("log", "log2", "log10"))
{
  return( entropy.plugin(freqs.empirical(y), unit=unit) )
}

# empirical frequencies
freqs.empirical = function(y)
{
  return( y/sum(y) )
}


# empirical mutual information
mi.empirical = function(y2d, unit=c("log", "log2", "log10"))
{
  return( mi.plugin(freqs.empirical(y2d), unit=unit) )
}

# empirical chi-squared of independence
chi2indep.empirical = function(y2d, unit=c("log", "log2", "log10"))
{
  return( chi2indep.plugin(freqs.empirical(y2d), unit=unit) )
}


# empirical chi-squared statistic
chi2.empirical = function(y1, y2, unit=c("log", "log2", "log10"))
{
  return( chi2.plugin(freqs.empirical(y1), freqs.empirical(y2), unit=unit) )
}

# empirical KL divergence
KL.empirical = function(y1, y2, unit=c("log", "log2", "log10"))
{
  return( KL.plugin(freqs.empirical(y1), freqs.empirical(y2), unit=unit) )
}


### entropy.MillerMadow.R  (2008-08-20)
###
###    Miller-Madow entropy estimator (1955)
###
### Copyright 2008 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# compute Miller-Madow (1955) entropy estimator

# y is a vector of counts (may include zeros)
entropy.MillerMadow = function(y, unit=c("log", "log2", "log10"))
{
  unit = match.arg(unit)
  
  n = sum(y)      # total number of counts
  m = sum(y>0)    # number of bins with non-zero counts 

  # bias-corrected empirical estimate
  H = entropy.empirical(y, unit="log") + (m-1)/(2*n)

  if (unit == "log2")  H = H/log(2)  # change from log to log2 scale
  if (unit == "log10") H = H/log(10) # change from log to log10 scale

  return(H)
}

### entropy.NSB.R  (2008-08-21)
###
###    R interface to the nsb-entropy estimator
###
### Copyright 2006-2008 Jean Hausser
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA




##################### public function ##########################################

entropy.NSB = function(y, unit=c("log", "log2", "log10"), CMD="nsb-entropy")
{
  unit = match.arg(unit)

  tmpfile = tempfile()
  tmpfile.txt = paste(tmpfile, "txt", sep=".")
  nsboutfile = nsbsave(filename=tmpfile.txt, y)
  system(paste(CMD, "-dnum -s1 -e1 -cY -v0", tmpfile))
  H = nsbload(filename=nsboutfile)
  unlink(nsboutfile)
  unlink(tmpfile.txt)

  if (unit == "log2")  H = H/log(2)  # change from log to log2 scale
  if (unit == "log10") H = H/log(10) # change from log to log10 scale

  return(H)
}
##################### private function #########################################

# saves a vector of counts (theta) such that it can be read by nsb-entropy
nsbsave = function(filename="samples.txt", theta) 
{
  datastr = character()
  for (i in 1:length(theta))
  {
    if ( theta[i] == 0 ) next
    for (j in 1:theta[i])
    {
      datastr = paste(datastr, i-1)
    }
  }
  #Removing leading space
  datastr = substr(datastr, 2, nchar(datastr))
  
  fileHandle = file(filename, "w")
  cat(
    paste(
      "# name: ASize",
      paste("# type: scalar", length(theta)),
      "# name: data",
      "# type: matrix",
      "# rows: 1",
      paste("# columns:", sum(theta)),
      datastr, 
      sep="\n"),
    file=fileHandle)
  close(fileHandle)

  # return the name nsb-entropy will store the results in
  nsboutfile = character()  
  splitname = strsplit(filename,'.',fixed=TRUE)[[1]]
  return(paste(splitname[1], "_uni_num", length(theta),
                 "_mf1f0_1_entr.", splitname[2], sep=''))
}

# read text file output by nsb-entropy
nsbload = function(filename) 
{
  nsbout = readLines(filename)
  # Seek to the Snsb section
  for (i in 1:length(nsbout)) {
    if ( nsbout[i] == "# name: Snsb" ) break
  }
  #Snsb is at i+4
  return(as.double(nsbout[i+4]))
}
### entropy.plugin.R  (2008-09-28)
###
###    Plug-in entropy estimator
###
### Copyright 2008 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


entropy.plugin = function(freqs, unit=c("log", "log2", "log10"))
{
   unit = match.arg(unit)

   freqs = freqs/sum(freqs) # just to make sure ...

   H = -sum( ifelse(freqs > 0, freqs*log(freqs), 0) )

   if (unit == "log2")  H = H/log(2)  # change from log to log2 scale
   if (unit == "log10") H = H/log(10) # change from log to log10 scale

   return(H)
}

### entropy.R  (2013-07-16)
###
###    Estimating entropy from observed counts
###
### Copyright 2008-13 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


entropy = function(y, lambda.freqs, method=c("ML", "MM", "Jeffreys", "Laplace", 
                   "SG", "minimax", "CS", "NSB", "shrink"),
                   unit=c("log", "log2", "log10"), verbose=TRUE, ...)
{
  method = match.arg(method)

  if (method == "ML")       H = entropy.empirical(y, unit=unit)
  if (method == "MM")       H = entropy.MillerMadow(y, unit=unit)
  if (method == "NSB")      H = entropy.NSB(y, unit=unit, ...)
  if (method == "CS")       H = entropy.ChaoShen(y, unit=unit)

  if (method == "Jeffreys") H = entropy.Dirichlet(y, a=1/2, unit=unit)
  if (method == "Laplace")  H = entropy.Dirichlet(y, a=1, unit=unit)
  if (method == "SG")       H = entropy.Dirichlet(y, a=1/length(y), unit=unit)
  if (method == "minimax")  H = entropy.Dirichlet(y, a=sqrt(sum(y))/length(y), unit=unit)

  if (method == "shrink")   H = entropy.shrink(y, lambda.freqs=lambda.freqs,
      unit=unit, verbose=verbose)

  return(H)
}

freqs = function(y, lambda.freqs, method=c("ML", "MM", "Jeffreys", "Laplace", 
                   "SG", "minimax", "CS", "NSB", "shrink"), verbose=TRUE)
{
  method = match.arg(method)

  if (method == "ML")       H = freqs.empirical(y)
  if (method == "MM")       H = rep(NA, length(y))
  if (method == "NSB")      H = rep(NA, length(y))
  if (method == "CS")       H = rep(NA, length(y))

  if (method == "Jeffreys") H = freqs.Dirichlet(y, a=1/2)
  if (method == "Laplace")  H = freqs.Dirichlet(y, a=1)
  if (method == "SG")       H = freqs.Dirichlet(y, a=1/length(y))
  if (method == "minimax")  H = freqs.Dirichlet(y, a=sqrt(sum(y))/length(y))

  if (method == "shrink")   H = freqs.shrink(y, lambda.freqs=lambda.freqs,
                               verbose=verbose)

  return(H)
}


### entropy.shrink.R  (2013-07-16)
###
###    Shrinkage estimators of entropy, mutual information
###    and related quantities
###
### Copyright 2008-13 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# shrinkage estimate of entropy 

# y:  a vector of counts (may include zeros)
entropy.shrink = function(y, lambda.freqs, unit=c("log", "log2", "log10"), verbose=TRUE)
{
  f = freqs.shrink(y, lambda.freqs=lambda.freqs, verbose=verbose)
  h = entropy.plugin(f, unit=unit)
  attr(h, "lambda.freqs") = attr(f, "lambda.freqs") # shrinkage intensity

  return( h )
}

freqs.shrink = function (y, lambda.freqs, verbose = TRUE) 
{
    target = 1/length(y) # uniform target (note length() works also for matrices)
    n = sum(y)
    u = y/n

    if (missing(lambda.freqs))
    {
      if (n==1 || n==0)
      {
        lambda.freqs = 1
      }
      else
      {
        lambda.freqs = get.lambda.shrink(n, u, target, verbose)
      }

    }
    else
    {
      if (verbose)
      {
        cat(paste("Specified shrinkage intensity lambda.freq (frequencies):", 
           round(lambda.freqs, 4)) , "\n")
      }
     
    }
    u.shrink = lambda.freqs * target + (1 - lambda.freqs) * u

    attr(u.shrink, "lambda.freqs") = lambda.freqs
    
    return(u.shrink)
}


# shrinkage estimation of mutual information
mi.shrink = function(y2d, lambda.freqs, unit=c("log", "log2", "log10"), verbose=TRUE)
{
  f2d = freqs.shrink(y2d, lambda.freqs=lambda.freqs, verbose=verbose)
  mi = mi.plugin(f2d, unit=unit)
  attr(mi, "lambda.freqs") = attr(f2d, "lambda.freqs") # shrinkage intensity

  return( mi )
}

# shrinkage estimation of chi-squared of independence
chi2indep.shrink = function(y2d, lambda.freqs, unit=c("log", "log2", "log10"), verbose=TRUE)
{
  f2d = freqs.shrink(y2d, lambda.freqs=lambda.freqs, verbose=verbose)
  chi2 = chi2indep.plugin(f2d, unit=unit)
  attr(chi2, "lambda.freqs") = attr(f2d, "lambda.freqs") # shrinkage intensity

  return( chi2 )
}


# shrinkage estimation of chi-squared statistic
chi2.shrink = function(y1, y2, lambda.freqs1, lambda.freqs2,
                       unit=c("log", "log2", "log10"), verbose=TRUE)
{
  f1 = freqs.shrink(y1, lambda.freqs=lambda.freqs1, verbose=verbose)
  f2 = freqs.shrink(y2, lambda.freqs=lambda.freqs2, verbose=verbose)
  chi2 = chi2.plugin(f1, f2, unit=unit)
  attr(chi2, "lambda.freqs1") = attr(f1, "lambda.freqs") # shrinkage intensity 1
  attr(chi2, "lambda.freqs2") = attr(f2, "lambda.freqs") # shrinkage intensity 2

  return( chi2 )
}

# shrinkage estimation of KL divergence
KL.shrink = function(y1, y2, lambda.freqs1, lambda.freqs2,
                     unit=c("log", "log2", "log10"), verbose=TRUE)
{
  f1 = freqs.shrink(y1, lambda.freqs=lambda.freqs1, verbose=verbose)
  f2 = freqs.shrink(y2, lambda.freqs=lambda.freqs2, verbose=verbose)
  KL = KL.plugin(f1, f2, unit=unit)
  attr(KL, "lambda.freqs1") = attr(f1, "lambda.freqs") # shrinkage intensity 1
  attr(KL, "lambda.freqs2") = attr(f2, "lambda.freqs") # shrinkage intensity 2

  return( KL )
}




## private function

get.lambda.shrink = function(n, u, t, verbose)
{
  # *unbiased* estimator of variance of u
  varu = u*(1-u)/(n-1)
  
  # misspecification
  msp = sum( (u-t)^2 )

  # estimate shrinkage intensity  
  if (msp == 0)
  {
    #warning("Overshrinkage")
    lambda = 1
  }
  else
    lambda = sum( varu ) / msp
  
  if (lambda > 1)
  {
    lambda = 1 # truncate at 1
    #warning("Overshrinkage")
  }
  
  if (lambda < 0)
  {
    lambda = 0
    #warning("Undershrinkage")
  }
  
  if (verbose)
  {
    cat(paste("Estimating optimal shrinkage intensity lambda.freq (frequencies):", 
      round(lambda, 4)) , "\n")
  }

  return(lambda)
}




as.simple.formula <- function(attributes, class) {
	return(as.formula(paste(class, paste(attributes, sep = "", collapse = " + "), sep = " ~ ")))
}

get.data.frame.from.formula <- function(formula, data) {
	d = model.frame(formula, data, na.action = NULL)
	for(i in 1:dim(d)[2]) {
		if(is.factor(d[[i]]) || is.logical(d[[i]]) || is.character(d[[i]]))
			d[[i]] = factor(d[[i]])
	}
	return(d)
}

entropyHelper <- function(x) {
    return(entropy(table(x, useNA="always")))
}
discretize.all <- function(formula, data) {
	new_data = get.data.frame.from.formula(formula, data)
	
	dest_column_name = dimnames(new_data)[[2]][1]
	if(!is.factor(new_data[[1]])) {
		new_data[[1]] = equal.frequency.binning.discretization(new_data[[1]], 5)
	}
	
	#new_data = supervised.discretization(formula, data = new_data)

	# reorder attributes
	new_data = get.data.frame.from.formula(formula, new_data)
	return(new_data)
}

# unupervised
equal.frequency.binning.discretization <- function(data, bins) {
	bins = as.integer(bins)
	if (!is.numeric(data)) 
		stop("Data must be numeric")
	if(bins < 1)
		stop("Number of bins too small")
	
	complete = complete.cases(data)
	ord = order(data)
	len = length(data[complete])
	blen = len / bins
	new_data = data

	p1 = p2 = 0

	for(i in 1:bins) {
		p1 = p2 + 1
		p2 = round(i * blen)
		new_data[ord[p1:min(p2, len)]] = i
	}

	return(factor(new_data))
}

# unupervised
equal.width.binning.discretization <- function(data, bins) {
	if (!is.numeric(data)) 
		stop("Data must be numeric")
	if(bins < 1)
		stop("Number of bins too small")
	return(cut(data, bins))
}
### INFORMATION GAIN BASED ALGORITHMS
# classification and regression
# continous and discrete data

information.gain <- function(formula, data) {
	information.gain.body(formula, data, type = "infogain")
}

gain.ratio <- function(formula, data) {
	information.gain.body(formula, data, type = "gainratio")	
}

symmetrical.uncertainty <- function(formula, data) {
	information.gain.body(formula, data, type = "symuncert")	
}

information.gain.body <- function(formula, data, type = c("infogain", "gainratio", "symuncert")) {
	type = match.arg(type)
	new_data = get.data.frame.from.formula(formula, data)
	new_data = discretize.all(formula, new_data)
	
	attr_entropies = sapply(new_data, entropyHelper)
	class_entropy = attr_entropies[1]
	attr_entropies = attr_entropies[-1]
	joint_entropies = sapply(new_data[-1], function(t) {
			entropyHelper(data.frame(cbind(new_data[[1]], t)))
		})
	results = class_entropy + attr_entropies - joint_entropies

	if(type == "gainratio") {
		results = results / attr_entropies
	} else if(type == "symuncert") {
		results = 2 * results / (attr_entropies	+ class_entropy)
	}

	attr_names = dimnames(new_data)[[2]][-1]
	return(data.frame(attr_importance = results, row.names = attr_names))
}
### KL.plugin.R  (2013-07-16)
###
###    Plug-in estimator of the Kullback-Leibler divergence 
###       and of the Chi-Squared Statistic
###
### Copyright 2013 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


KL.plugin = function(freqs1, freqs2, unit=c("log", "log2", "log10") )
{
  unit = match.arg(unit)
  freqs1 = freqs1/sum(freqs1) # just to make sure ...
  freqs2 = freqs2/sum(freqs2) # just to make sure ...

  if( any( !(freqs2 > 0) ) ) warning("Vanishing value(s) in argument freqs2!")

  LR = ifelse(freqs1 > 0, log(freqs1/freqs2), 0)
  KL = sum(freqs1*LR)

  if (unit == "log2")  KL = KL/log(2)  # change from log to log2 scale
  if (unit == "log10") KL = KL/log(10) # change from log to log10 scale

  return(KL)
}


chi2.plugin = function(freqs1, freqs2, unit=c("log", "log2", "log10") )
{
  unit = match.arg(unit)
  freqs1 = freqs1/sum(freqs1) # just to make sure ...
  freqs2 = freqs2/sum(freqs2) # just to make sure ...

  if( any( !(freqs2 > 0) ) ) warning("Vanishing value(s) in argument freqs2!")

  chi2 = sum( (freqs1-freqs2)^2/freqs2 )

  if (unit == "log2")  chi2 = chi2/log(2)  # change from log to log2 scale
  if (unit == "log10") chi2 = chi2/log(10) # change from log to log10 scale

  return(chi2)
}

### mi.plugin.R  (2013-07-16)
###
###    Plug-in estimator of mutual information and 
###    of the chi-squared statistic of independence
###
### Copyright 2008-13 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA



mi.plugin = function(freqs2d, unit=c("log", "log2", "log10"))
{
  unit = match.arg(unit)

  freqs2d = as.matrix(freqs2d/sum(freqs2d)) # just to make sure ...

  freqs.x = rowSums(freqs2d) # marginal frequencies
  freqs.y = colSums(freqs2d)
  freqs.null = freqs.x %o% freqs.y # independence null model

  MI = KL.plugin(freqs2d, freqs.null, unit=unit)
 
  return(MI)
}


chi2indep.plugin = function(freqs2d, unit=c("log", "log2", "log10"))
{
  unit = match.arg(unit)

  freqs2d = as.matrix(freqs2d/sum(freqs2d)) # just to make sure ...

  freqs.x = rowSums(freqs2d) # marginal frequencies
  freqs.y = colSums(freqs2d)
  freqs.null = freqs.x %o% freqs.y # independence null model

  chi2 = chi2.plugin(freqs2d, freqs.null, unit=unit)
 
  return(chi2)
}


