# construct a dataset of implicit links from an affiliation dataset
implicit_pairs <- function(data, nodes, links) {
  # uniquify subdata
  sub <- unique(data[, c(nodes, links)])
  # merge with itself
  imp <- merge(sub, sub[, 2:1], by = names(sub)[2])[, c(2:3, 1)]
  names(imp) <- c("node1", "node2", "link")
  # return
  imp
}

# identify duplicate links
duplicated_links <- function(data, node1, node2) {
  data <- data[, c(node1, node2)]
  wh_rev <- which(data[, 1] > data[, 2])
  data[wh_rev, 1:2] <- data[wh_rev, 2:1]
  duplicated(data)
}

# locate a good cutoff value for a heavy-tailed distribution
find_elbow <- function(vals, mincount = 3, maxcount = 20) {
  # sort the values
  vals <- unname(sort(vals))
  
  v <- 1
  len <- length(which(vals >= v))
  if (len < mincount) return(unname(max(v)))
  
  # if too few or too many values are included,
  # then perform again on one side of the cutoff
  while (len < mincount | len > maxcount) {
    # restrict to appropriate range
    vals <- if (len < mincount) {
      vals[vals < v]
    } else if (len > maxcount) {
      vals[vals >= v]
    }

    # locate the highest discrete second derivative
    i <- which.max(diff(diff(vals)))
    old.v <- v
    v <- vals[i + 2]
    
    # adjust length
    len <- if (len < mincount) {
      len + length(which(vals >= v))
    } else if (len > maxcount) {
      len - length(which(vals < v))
    }
    # if no change to cutoff value, abort
    if (v == old.v) break
  }
  
  # return the value
  v
}