#' clean data imported from csv
cleandata <- function(readin,
                      votethresh = 1,
                      vote.colnames,
                      title.colname,
                      type.colname){
    if(!all(vote.colnames %in% colnames(readin))) stop("Error 41: vote.colnames are not valid columns in the data")
    if(!(title.colname %in% colnames(readin))) stop("Error 42: title.colname is not a valid column in the data")
    if(!(type.colname %in% colnames(readin))) {
        warning("type.colname not found in data. Creating a dummy column.")
        readin[[type.colname]] <- rep("NONE", nrow(readin))
    }

    readin[,vote.colnames][is.na(readin[,vote.colnames])] <- 0 # replace NA with zero
    totals <- readin[["Total"]] <- rowSums(readin[,vote.colnames]) # count total votes
    cleanrows <- !(totals <= votethresh) # remove zero total votes
    data <- readin[cleanrows,]
    return(droplevels(data))
}


#' convert cleaned data into long form
data2longlist <- function(data,
                      vote.colnames,
                      title.colname,
                      type.colname
                      ){
    if(!all(vote.colnames %in% colnames(data))) stop("Error 41: vote.colnames is not a valid column in the data")
    if(!(title.colname %in% colnames(data))) stop("Error 42: title.colname is not a valid column in the data")
    expandedvotes <- apply(data[,vote.colnames], 1, function(filmvotes) rep(seq_along(1:length(vote.colnames)), filmvotes))
    votes.list <- lapply(1:nrow(data), function(ii){
        this_filmvotes <- expandedvotes[[ii]]
        nvotes <- length(this_filmvotes)
        data.frame(FILM = rep(data[ii,title.colname], nvotes),
                   TYPE = rep(ifelse(type.colname %in% colnames(data), data[ii,type.colname], "NONE"), nvotes),
                   VOTE = this_filmvotes)
    })
    names(votes.list) <- data[[title.colname]]
    return(votes.list)
}

#' bootstrap mean scores
bootmeans <- function(votes.list,
                     FUN = mean,
                     nboot = 10000,
                     ncores = NULL
                     ){
    require(parallel, quietly = T)
    if(is.null(ncores)) ncores <- detectCores()-1
    meanvotes.boot.list <- mclapply(1:nboot, function(i){
        boot.votes.list <- lapply(votes.list, function(this_film){
            if(nboot > 1){
                sample(this_film$VOTE, replace = TRUE)
            } else {
                this_film$VOTE # skip the bootstrap if nboot = 1
            }
        })
        boot.meanvotes <- sapply(boot.votes.list, function(this_film){
            FUN(this_film)
        })

        return(boot.meanvotes)
    }, mc.cores = ncores)
    return(meanvotes.boot.list)
}


#' create matrix of probabilities i>j
bootmeans2adjacency <- function(meanvotes.boot.list,
                                names = NULL
                                ){
    meanvotes.boot <- do.call(rbind, meanvotes.boot.list)
    a <- matrix(0, ncol(meanvotes.boot), ncol(meanvotes.boot))
    nvoters <- nrow(meanvotes.boot)
    for(i in 1:(nrow(a)-1)){
        for(j in i:nrow(a)){
            # an edge from i->j denotes an endorsement of j
            a[i,j] <- sum(meanvotes.boot[,j] > meanvotes.boot[,i])/nvoters # i endorses j
            a[j,i] <- sum(meanvotes.boot[,i] > meanvotes.boot[,j])/nvoters # j endorses i
        }
    }
    if(!is.null(names)) rownames(a) <- colnames(a) <- names
    return(a)
}




