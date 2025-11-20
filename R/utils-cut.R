# these functions largely borrowed from ggplot2.
# ive copied rather than imported them for increased control
# this was decided after finding a bug or two

#' Discretise numeric data into equal ranges
#'
#' This function makes `n` groups with equal range
#'
#' @param x numeric vector
#' @param n number of intervals to create, OR
#' @param length length of each interval
#' @inheritDotParams base::cut.default -x -include.lowest
#' @export
cut_interval <- function(x, n = NULL, length = NULL, ...) {
  cut(x, breaks(x, "equalInterval", n, length), include.lowest = TRUE, ...)
}

#' Discretise numeric data into equal size groups
#'
#' This function makes `n` groups with (approximately) equal numbers of observations
#'
#' @param x numeric vector
#' @param n number of intervals to create
#' @inheritDotParams base::cut.default -x -include.lowest
#' @export
cut_number <- function(x, n = NULL, ...) {
  brk <- breaks(x, "quantile", n)
  if (anyDuplicated(brk))
    stop(glue("Insufficient data values to produce {n} bins."))
  cut(x, brk , include.lowest = TRUE, ...)
}

#' Discretise numeric data into equal ranges
#'
#' This function makes groups of width `width`.
#'
#' @param x numeric vector
#' @param width The bin width.
#' @param center,boundary Specify either the position of edge or the center of
#'   a bin. Since all bins are aligned, specifying the position of a single bin
#'   (which doesn't need to be in the range of the data) affects the location of
#'   all bins. If not specified, uses the "tile layers algorithm", and sets
#'   the boundary to half of the binwidth.
#'
#'   To center on integers, `width = 1` and `center = 0`.
#'   `boundary = 0.5`.
#' @param closed One of `"right"` or `"left"` indicating whether right
#'   or left edges of bins are included in the bin.
#' @inheritDotParams base::cut.default -x -include.lowest
#' @export
cut_width <- function(x, width, center = NULL, boundary = NULL, closed = c("right", "left"), ...) {
  x <- as.numeric(x)
  width <- as.numeric(width)

  closed <- mbioUtils::matchArg(closed)

  x_range <- range(x, na.rm = TRUE, finite = TRUE)
  if (length(x_range) == 0) {
    return(x)
  }

  # Determine boundary
  if (!is.null(boundary) && !is.null(center)) {
    stop("Only one of 'boundary' and 'center' may be specified.")
  }
  if (is.null(boundary)) {
    if (is.null(center)) {
      # If neither edge nor center given, compute both using tile layer's
      # algorithm. This puts min and max of data in outer half of their bins.
      boundary <- width / 2
    } else {
      # If center given but not boundary, compute boundary.
      boundary <- center - width / 2
    }
  }
  boundary <- as.numeric(boundary)

  # Determine bins

  # Find functional min and max which produces consistently sized bins
  # its possible we want to do this for ints regardless of the bin width??
  min_x <- find_origin(x_range, width, boundary)
  if (closed == "right") {
    # if x are integers and width is 1, make sure min_x is one smaller than the data
    # otherwise the first bin will be functionally twice the width
    if (all(x == round(x)) && width == 1) {
      min_x <- min_x - 1
    }
  }
  
  max_x <- max(x, na.rm = TRUE)
  if (closed == "left") {
    # if x are integers and width is 1, make sure max_x is one greater than the data
    # otherwise the last bin will be functionally twice the width
    if (all(x == round(x)) && width == 1) {
      max_x <- max_x + 1
    }
  }

  breaks <- c(seq(min_x, max_x, width))
  last_break = tail(breaks, 1)
  # add an extra break if the seq() didn't produce a break >= max_x
  # but first test to see if last break is within a (internal R) rounding error of max_x
  if (max_x - last_break < 1e-08) {
    # just make the last break equal the max
    breaks[length(breaks)] <- max_x   
  } else if (last_break < max_x) {
    # add a new break
    breaks <- c(breaks, last_break + width)
  }

  # safety checks
  if (tail(breaks, 1) < max_x) {
    stop("Problem with final bin in utils-cut.R")
  }
  if (length(breaks) < 2) {
    stop("Less than two breaks in utils-cut.R")
  }

  # Round breaks *before* they go into the cut function. This way the data (not rounded)
  # will be correctly divided into the rounded bins
  if (all(breaks %% 1 == 0)) {
    # if breaks are all integers, skip our own rounding
    # and use default formatting in cut() (6 significant digits)
    requiredDigits <- -1
  } else {
    # Determine the appropriate number of digits to round the bin start/ends to.
    # Start at 1 and work up!
    requiredDigits <- 0
    rawBreaks <- breaks
    repeat {
      requiredDigits <- requiredDigits + 1
      if (requiredDigits > 20) { # safety escape condition
        break
      }

      # round the breaks to requiredDigits significant digits
      # we use formatC (not signif) because the cut() below uses it too,
      # and we are aiming for consistency (though maybe we could pass our labels to it?)
      breaks <- as.numeric(formatC(0 + rawBreaks, digits = requiredDigits, width = 1L))

      # if any breaks are the same, try more significant digits
      if (anyDuplicated(breaks) > 0) {
        next
      }

      # calculate the inter-break differences (binWidths) from rounded values
      diffs <- diff(breaks)

      # if only one difference (e.g. one bin), we can't do the following part
      # which checks that the differences are not too different from each other
      if (length(diffs) < 2) {
        break
      }

      # now see if the normalised variance (index of dispersion) of the diffs is small enough to
      # stop increasing the number of significant digits
      dispersion <- var(diffs)/mean(diffs)
      if (dispersion < 1e-05) {
        break
      }
    }
  }

  # Now, we know we have enough bins to encompass min_x and max_x, but they could have been
  # rounded to slightly just above or below, respectively.
  # If so, we will adjust the first and last bins by subtracting/adding one "least significant digit"
  # from them.

  if (min_x < breaks[1]) {
    newStart <- breaks[1] - signifDigitEpsilon(breaks[1], requiredDigits)
    breaks[1] <- as.numeric(formatC(newStart, digits = requiredDigits, width = 1L))
  }

  lastBreak = breaks[length(breaks)]
  if (max_x > lastBreak) {
    newEnd <- lastBreak + signifDigitEpsilon(lastBreak, requiredDigits)
    breaks[length(breaks)] <- as.numeric(formatC(newEnd, digits = requiredDigits, width = 1L))
  }

  # safety checks
  if (min_x < breaks[1]) {
    stop("Fatal problem with cut_width start bin extension")
  }
  if (max_x > breaks[length(breaks)]) {
    stop("Fatal	problem	with cut_width end bin extension")
  }

  cut(x, breaks, include.lowest = TRUE, right = (closed == "right"), dig.lab = requiredDigits, ...)
}

# Find the left side of left-most bin
find_origin <- function(x_range, width, boundary) {
  shift <- floor((x_range[1] - boundary) / width)
  boundary + shift * width
}

breaks <- function(x, method = c('equalInterval', 'quantile', 'sd'), nbins = NULL, binwidth = NULL) {
  method <- mbioUtils::matchArg(method)
  if (method != 'sd') {
    if ((!is.null(nbins) && !is.null(binwidth)) || (is.null(nbins) && is.null(binwidth))) {
      stop("Specify exactly one of n and width for methods `equalInterval` and `quantile`.")
    }
  } else {
    if (!is.null(nbins) || !is.null(binwidth)) {
       warning("`breaks` method `sd` does NOT currently support specifying `nbins` or `binwidth`.")
    }
  }

  rng <- range(x, na.rm = TRUE, finite = TRUE)

  if (method == "equalInterval") {
    if (!is.null(binwidth)) {
      brks <- scales::fullseq(rng, binwidth)
    } else {
      brks <- seq(rng[1], rng[2], length.out = nbins + 1)
    }
  } else if (method == "quantile") {
    if (!is.null(binwidth)) {
      probs <- seq(0, 1, by = binwidth)
    } else {
      probs <- seq(0, 1, length.out = nbins + 1)
    }
    brks <- stats::quantile(x, probs, na.rm = TRUE)
  } else {
    med <- median(x, na.rm = TRUE)
    sd <- sd(x, na.rm = TRUE)
    brks <- c(min(x), med-(sd*2), med-sd, med, med+sd, med+(sd*2), max(x))
    #very small data might not produce valid sd
    brks <- brks[!is.na(brks)]
 
    valid <- all(unlist(lapply(2:length(brks), FUN = function(x) {brks[[x]] > brks[[x-1]]})))
    if (!valid) {
      # add the first false bc the min is known to be valid, start testing after that point
      invalid <- c(FALSE, unlist(lapply(2:length(brks), FUN = function(x) {brks[[x]] < brks[[1]] || brks[[x]] > brks[[length(brks)]]})))
      brks <- brks[!invalid]
    }
  }

  return(brks)
}
