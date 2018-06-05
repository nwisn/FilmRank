
Film festivals present an Audience Choice award to films based on the ranking of votes. The voting is typically a 5-star range system, where audience members can give a film an integer score between 1 and 5. An algorithm is then used to rank the films. But what's a good algorithm?

### The FilmRank algorithm
The FilmRank ranking algorithm bootstraps the probability that the average score $\mu_i$ of film $i$ is truly greater than any other film $j$, by resampling the votes to simulate the universe of all possible rankings. This matrix of probabilities $p_{ij}=p(\mu_j \gt \mu_i)$ is then treated as a weighted directed graph, and the [authority score](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.418.3908) $\text{eig}_1(p^Tp)$ is used to rank the films, in a way similar to Google *page-rank*.

### Why use this method?
This ranking system was designed to address a problem in estimating ranks introduced by the wide range of audience sizes at a film festival. Specifically, ranking based on naive ordering the mean scores can produce undesirable results. For example, a high scoring film that many people came to see (opening/closing night) can sometimes be beaten by a film that very few people came to see, but which also scored highly, only because of the increased sampling error in the small group. The FilmRank algorithm addresses this problem by using resampling to weigh the relative superiority of each film to every other, which can then produce more meaningful rankings using graph theory.

### How are the results different?
This method is asymptotically equivalent to the naive ordering -- in the limit where all audience sizes are large, they give the same results. However, when both large and small audiences are present, this method tends to favor scores from larger audiences, which is desirable. 

### How do I use the algorithm?
The algorithm currently only exists as an [R](https://www.r-project.org/) package, which can be installed from [github](https://github.com/nwisn/FilmRank) by typing the following commands into R:

`install.packages("devtools")`

`library(devtools)`

`install_github("nwisn/FilmRank", build_vignettes = TRUE)`

An [example usage](https://nwisn.github.io/FilmRank/results_IMDb.html) exists on publicly-available IMDb voting data, and more information can be found by typing:

`vignette("FilmRank")`

and further help can be found by typing:

`?filmrank`


### References
[[1](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.418.3908)] Kleinberg, J. M. (1999). Authoritative sources in a hyperlinked environment. *Journal of the ACM (JACM)*, 46(5), 604-632.



