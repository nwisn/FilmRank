require(FilmRank, quietly = T)
require(ggplot2movies, quietly = T)
require(reshape2, quietly = T)
set.seed(137)

# get a subset of IMDb movies
movies.subgroups <- movies$year == 2003 & movies$votes < 300 & movies$votes > 10 # make some cuts
subsample.ix <- sample(1:sum(movies.subgroups), 1000, replace = F) # subsample 50 films
movies.selection <- movies[movies.subgroups,][subsample.ix,]

# reshape the genre columns
movies.melt0 <- melt(movies.selection,
                     id.vars = colnames(movies)[1:17],
                     variable.name = "type",
                     value.name = "type.yes")
movies.melt <- movies.melt0[movies.melt0$type.yes == 1,]
movies.unique <- do.call(rbind, lapply(split(movies.melt, movies.melt$title), function(x) x[1,]))

# transform percentile vote columns r1-r10 into counts
movies.unique[,c(7:16)] <- round(movies.unique[,c(7:16)]/rowSums(movies.unique[,c(7:16)]) * movies.unique$votes)

suppressMessages(run <- filmrank(movies.unique,
                             title.colname = "title",
                             type.colname = "type",
                             vote.colnames = paste0("r", 1:10),
                             score = function(x) mean(x, trim = 0),
                             nboot = 100)
)

summary(run)
suppressMessages(plot(run, rankmetric = "authority"))


df <- run$data
df$auth.rank <- rank(df$authority)
df$mean.rank <- rank(df$mean)
df$residuals.rank <- df$mean.rank - mean(df$mean.rank) # de-meaned rank
df$residuals.mean <- df$mean - mean(df$mean) # de-meaned mean
df$sign.mean <- sign(df$residuals.mean) # ranked above/below mean
df$sign.rank <- sign(df$residuals.rank) # ranked above/below median
df$stderr.group <- sign(df$stderror - median(df$stderror))


require(ggplot2)
require(ggrepel)

ggplot(df) + aes(x=mean) + geom_histogram(aes(y=..density..)) + geom_density(color = "red", fill = "red", adjust = 2, alpha = .1) + geom_vline(xintercept=mean(df$mean), lty="dotted") + theme_bw()
ggplot(df) + aes(x=authority) + geom_histogram(aes(y=..density..)) + geom_density(color = "red", fill = "red", adjust = .5, alpha = .1) + geom_vline(xintercept=mean(df$authority), lty="dotted")

ggplot(df) + aes(x = Total, y = stderror) + geom_point()  + geom_smooth(span=.5) + scale_color_continuous(low = "red", high = "green") + geom_hline(yintercept=0, lty="dotted") + theme_bw() + ggtitle("Standard Error vs Total Votes") + xlab("Total Votes") + ylab("Standard Error (of the Mean Vote)")

ggplot(df) + aes(x = mean, y = stderror) + geom_point()  + geom_smooth(method="lm") + scale_color_continuous(low = "red", high = "green") + geom_hline(yintercept=0, lty="dotted") + theme_bw() + ggtitle("Standard Error vs Mean")

ggplot(df) + aes(x = mean, y = authority, color = stderror) + geom_point()  + geom_smooth(span=.1) + scale_color_continuous(low = "red", high = "green") + geom_hline(yintercept=0, lty="dotted") + geom_vline(xintercept = mean(df$mean), lty="dotted") + theme_bw() + ggtitle("Authority vs Mean")

ggplot(df) + aes(x = mean.rank/nrow(df), y = auth.rank/nrow(df), color = stderror) + geom_point(alpha = .5)  + geom_smooth(method = "lm") + scale_color_continuous(low = "red", high = "green") + geom_vline(xintercept=mean(df$mean.rank)/nrow(df), lty="dotted") + geom_hline(yintercept = mean(df$auth.rank)/nrow(df), lty="dotted") + theme_bw() + ggtitle("FilmRank vs Mean Rank")+ xlab("Rank by Mean Vote") + ylab("FilmRank") + guides(fill=FALSE) + labs(color="Standard Error") + theme(legend.position="right")

ggplot(df) + aes(x = Total, y = (auth.rank-mean.rank)/nrow(df)*100, color=mean.rank/nrow(df)*100, group=factor(sign.rank), fill=factor(sign.rank)) + geom_point()  + geom_smooth(span=.7) + scale_color_continuous(low = "red", high = "green") + geom_hline(yintercept=0, lty="dotted") + theme_bw() + ggtitle("Change in Ranking due to Audience Size") + xlab("Total Number of Votes") + ylab("Change in Rank Percentile") + guides(fill=FALSE) + labs(color="Rank Percentile") + theme(legend.position="right")

cor(df$auth.rank, df$mean.rank)

ggplot(df) + aes(x = stderror, y = (auth.rank-mean.rank)/nrow(df)*100,
                 color=mean.rank/nrow(df)*100, group=factor(sign.rank), fill=factor(sign.rank)) +
    geom_point()  +
    geom_smooth(method = "loess", span = 3) +
    scale_color_continuous(low = "red", high = "green") +
    geom_hline(yintercept=0, lty="dotted") +
    theme_bw() +
    ggtitle("Change in Ranking due to Standard Error") +
    xlab("Standard Error (of the Mean Vote)") +
    ylab("Change in Rank Percentile") +
    guides(fill=FALSE) + labs(color="Rank Percentile") + theme(legend.position="right")

