#' Compute LAGFF rankings
#'
#' \code{LAGFF} computes range voting rankings for the Los Angeles Greek Film Festival..
#'
#' @param csv dataframe; from \code{read.csv}
#' @param title.colname string; specifying the \code{csv} column name containing film titles
#' @param type.colname string; specifying the \code{csv} column name containing film types
#' @param vote.colnames string; specifying the \code{csv} column names containing the number of votes for each point value
#' @param score function; used to estimate the score
#' @param nboot integer; number of bootstrap resamples
#' @param minvote integer; minimum number of votes required to be in rankings
#' @param seed integer; random seed for reproducibility
#' @param ncores number of cores used for parallel computing
#'
#' @details The LAGFF ranking algorithm bootstraps the probability that the average score \eqn{μ_i} of film i is truly greater than any other film j, by resampling the votes to simulate the universe of all possible rankings. This matrix of probabilities \eqn{p_{ij}=p(μ_j>μ_i)} is then treated as a weighted directed graph, and the authority score \eqn{eig_1(p^Tp)} is used to rank the films, in a way similar to Google page-rank. This is computed using \code{\link[igraph]{authority.score}}.
#'
#' This ranking system was adopted by LAGFF to address a problem in estimating ranks introduced by the wide range of audience sizes at a film festival. Specifically, ranking based on naive ordering the mean scores can produce undesirable results. For example, a high scoring film that many people came to see (opening/closing night) can sometimes be beaten by a film that very few people came to see, but which also scored highly, only because of the increased sampling error in the small group. The LAGFF algorithm addresses this problem by using resampling to weigh the relative superiority of each film to every other, which can then produce more meaningful rankings using graph theory.
#'
#' This method is asymptotically equivalent to the naive ordering – in the limit where all audience sizes are large, they give the same results. However, when both large and small audiences are present, this method tends to favor scores from larger audiences, which is desirable. However, as a side effect, the relative ranking of two particular films becomes dependent on their relative rankings with all of the other films. For this reason, the ranking within a subgroup of films may end up slightly differing from their relative ranking in the context of all the films.
#'
#' @return an object of class \code{LAGFF}, which is a list with elements for
#' \itemize{
#'   \item \code{adj} : the adjacency matrix
#'   \item \code{g} : an igraph graph object
#'   \item \code{data} : a dataframe with vote counts, mean, 95% CI, and authority score
#'   \item elements that are passthrough copies of the inputs arguments
#' }
#'
#' @export
LAGFF <- function(csv,
                  title.colname = "FILM",
                  type.colname = "TYPE",
                  vote.colnames = make.names(c("1","2","3","4","5")),
                  score = function(x) mean(x, trim = 0),
                  nboot = 1000,
                  minvote = 2,
                  seed = 137,
                  ncores = NULL
                  ){
    require(igraph)
    if(!is.null(seed)) set.seed(seed)

    #source(file.path("~/Documents/LAGFF/R","functions.R"))
    data <- cleandata(csv,
                      votethresh = minvote-1,
                      vote.colnames = vote.colnames,
                      type.colname = type.colname,
                      title.colname = title.colname )

    votes.list <- data2longlist(data,
                                vote.colnames = vote.colnames,
                                title.colname = title.colname,
                                type.colname = type.colname)

    # bootstrap the mean for every film
    meanvotes.boot.list <- bootmeans(votes.list,
                                     FUN = score,
                                     nboot = nboot,
                                     ncores = ncores)

    # turn probability film i>j into a graph and compute centrality scores
    a <- bootmeans2adjacency(meanvotes.boot.list, names = data[[title.colname]])
    g <- graph_from_adjacency_matrix(a, mode = "directed", diag = FALSE, weighted = TRUE)
    V(g)$type <- as.character(data[[type.colname]])
    data[["hub"]] <- hub.score(g)$vector
    data[["authority"]] <- authority.score(g)$vector
    data[["degree"]] <- colSums(a)

    # compute means, unbiased means, and 95% confidence intervals
    meanvotes.boot.matrix <- do.call(rbind,meanvotes.boot.list)
    meanvotes.boot.CI <- apply(meanvotes.boot.matrix,2,function(this_boot) quantile(this_boot,c(.05/2,1-.05/2)))
    data[["mean"]] <- sapply(votes.list, function(this_film) score(this_film$VOTE))
    data[["mean.lower"]] <- meanvotes.boot.CI[1,]
    data[["mean.upper"]] <- meanvotes.boot.CI[2,]

    ret <- list(adj = a,
                graph = g,
                data = data,
                title.colname = title.colname,
                type.colname = type.colname,
                vote.colnames = vote.colnames,
                score = score,
                nboot = nboot,
                minvote = minvote,
                seed = seed,
                ncores = ncores)
    class(ret) <- "LAGFF"
    return(ret)
}

#'summary function for LAGFF results
#'
#'\code{summary.LAGFF} prints the highest ranked film from each category.
#'
#'@param lagff.obj an LAGFF object
#'@export
summary.LAGFF <- function(lagff.obj){
    ranking <- lagff.obj$data[order(lagff.obj$data$authority, decreasing = T),
                              c(lagff.obj$title.colname, lagff.obj$type.colname, lagff.obj$vote.colnames,
                                "Total", "authority", "degree", "mean")]
    rownames(ranking) <- 1:nrow(ranking)

    best <- lapply(split(ranking, ranking[[lagff.obj$type.colname]]), function(x) x[1,])
    cat("The highest ranked film from each category is:\n\n")

    bests <- do.call(rbind, best)
    print(bests[order(bests$authority, decreasing = T),])
}


#' Plot LAGFF rankings
#'
#' \code{plot.LAGFF} creates a forest plot of LAGFF rankings.
#'
#'@param lagff.obj a result returned by \code{LAGFF}
#'@param rankmetric string; metric by which to rank films. One of 'authority', 'hub', 'degree', 'mean'.
#'@param color.palette string of colors, or ggsci palette generator function
#'@param color.alpha number between 0 and 1 specifying the transparency of the colors
#'@param title string; title of the plot
#'@param cex.label number specifying the size of the row labels
#'@param xlim range specifying the x-axis limits
#'@param cex.xlab number specifying the size of the x-axis label
#'@param cex.ticks number specifying the size of the ticks
#'@param boxsize number specifying the size of the boxes
#'@param boxcolor string specifying the color of the boxes
#'@param new_page logical; whether to make figure from a new page
#'@param grid logical; whether to include grid lines
#'@param xticks sequence specifying the x-axis ticks
#'
#' @export
plot.LAGFF <- function(lagff.obj,
                       rankmetric = "authority",
                       color.palette = pal_igv,
                       color.alpha = 1,
                       title = NULL,
                       cex.label = 0.5,
                       xlim = c(1,5),
                       cex.xlab = 1,
                       cex.ticks = 0.5,
                       boxsize = 1/200,
                       boxcolor = "darkblue",
                       new_page = TRUE,
                       grid = TRUE,
                       xticks = seq(1,5,by=1)
){
    require(forestplot)
    data <- lagff.obj[["data"]]
    title.colname <- lagff.obj[["title.colname"]]
    type.colname <- lagff.obj[["type.colname"]]

    if(is.null(data[[rankmetric]])) stop("ERROR 17: the rankmetric specified doesn't exist in data.
                                         Must be one of: authority, hub, degree, mean. ")

    # choose colors
    colors = factor(data[[type.colname]])
    colors <- colors[order(data[[rankmetric]], decreasing = TRUE)]
    if(class(color.palette) == "function"){
        require(ggsci)
        levels(colors) <- color.palette(alpha = color.alpha)(nlevels(colors))
    } else if(class(color.palette) == "character"){
        levels(colors) <- color.palette[1:nlevels(colors)]
    }

    if(is.null(title)) title <- paste(rankmetric, "ranking")

    # rank
    rankeddata <- data[order(data[[rankmetric]], decreasing = TRUE),]
    tabletext <- cbind(
        c("Rank", 1:nrow(rankeddata)),
        c("Film", as.character(rankeddata[[title.colname]]))
        #c("Type", as.character(rankeddata[[type.colname]])),
        #c("N", as.character(rankeddata$Total)),
        #c("Avg", round(rankeddata[["mean"]],2)),
        #c("Graph", round(rankeddata[["hubscore"]],3))
        )
    struct <- structure(list(
        mean  = c(NA, rankeddata[["mean"]]),
        lower = c(NA, rankeddata[["mean.lower"]]),
        upper = c(NA, rankeddata[["mean.upper"]])),
        .Names = c("mean", "lower", "upper"),
        row.names = c(NA, as.character(rankeddata[[title.colname]])),
        class = "data.frame")

    forestplot(
        tabletext,
        struct,
        col=fpColors(text = c("black", as.character(colors)), box=boxcolor),
        is.summary=c(TRUE,rep(FALSE,nrow(struct))),
        xlab="average vote",
        title=title,
        clip=xlim,
        txt_gp =fpTxtGp(label = gpar(cex = cex.label), xlab = gpar(cex = cex.xlab), ticks = gpar(cex = cex.ticks)),
        zero=weighted.mean(rankeddata[["mean"]], rankeddata[["Total"]]),
        boxsize=c(NA,rankeddata$Total)*boxsize,
        grid = grid,
        xticks = xticks,
        new_page=new_page)
}
