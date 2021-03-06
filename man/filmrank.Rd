% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filmrank.R
\name{filmrank}
\alias{filmrank}
\title{Compute film rankings}
\usage{
filmrank(csv, title.colname = "FILM", type.colname = "TYPE",
  vote.colnames = make.names(c("1", "2", "3", "4", "5")),
  score = function(x) mean(x, trim = 0), nboot = 1000, minvote = 2,
  seed = 137, ncores = NULL)
}
\arguments{
\item{csv}{dataframe; from \code{read.csv}}

\item{title.colname}{string; specifying the \code{csv} column name containing film titles}

\item{type.colname}{string; specifying the \code{csv} column name containing film types}

\item{vote.colnames}{string; specifying the \code{csv} column names containing the number of votes for each point value}

\item{score}{function; used to estimate the score}

\item{nboot}{integer; number of bootstrap resamples}

\item{minvote}{integer; minimum number of votes required to be in rankings}

\item{seed}{integer; random seed for reproducibility}

\item{ncores}{number of cores used for parallel computing}
}
\value{
an object of class \code{FilmRank}, which is a list with elements:
\itemize{
  \item \code{adj} : the adjacency matrix
  \item \code{g} : an igraph graph object
  \item \code{data} : a dataframe with the data and the authority score
  \item other elements that are pass-through copies of the inputs arguments
}
}
\description{
\code{filmrank} computes range voting rankings.
}
\details{
The FilmRank ranking algorithm bootstraps the probability that the average score \eqn{μ_i} of film i is truly greater than any other film j, by resampling the votes to simulate the universe of all possible rankings. This matrix of probabilities \eqn{p_{ij}=p(μ_j>μ_i)} is then treated as a weighted directed graph, and the authority score \eqn{eig_1(p^Tp)} is used to rank the films, in a way similar to Google page-rank. This is computed using \code{\link[igraph]{authority.score}}.

This ranking system was designed to address a problem in estimating ranks introduced by the wide range of audience sizes at a film festival. Specifically, ranking based on naive ordering the mean scores can produce undesirable results. For example, a high scoring film that many people came to see (opening/closing night) can sometimes be beaten by a film that very few people came to see, but which also scored highly, only because of the increased sampling error in the small group. The FilmRank algorithm addresses this problem by using resampling to weigh the relative superiority of each film to every other, which can then produce more meaningful rankings using graph theory.

This method is asymptotically equivalent to the naive ordering – in the limit where all audience sizes are large, they give the same results. However, when both large and small audiences are present, this method tends to favor scores from larger audiences, which is desirable. However, as a side effect, the relative ranking of two particular films becomes dependent on their relative rankings with all of the other films. For this reason, the ranking within a subgroup of films may end up slightly differing from their relative ranking in the context of all the films.
}
