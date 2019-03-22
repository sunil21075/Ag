

ci_copy <- Cj
ci_copy[, 'mean_escaped_Gen4'] <- ci_copy[, 'mean_escaped_Gen4'] + 
                                  rnorm(dim(ci_copy)[1], mean=0, sd=10^-8)
Cj_copy.sd <- apply(ci_copy[, 4:11], MARGIN=2, FUN=sd, na.rm=T)
A_copy <- sweep(A[, 4:11], MARGIN=2, STATS = Cj_copy.sd, FUN = `/`) # standardize
head(A_copy)