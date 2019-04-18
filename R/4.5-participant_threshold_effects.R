###Use to get list of participants who did not respond strongly to teh 
#bias manipulation

NBshift <- function(thetas) ((thetas[,"B.I.one.N",, drop=F]  -
                                thetas[,"B.U.one.N",, drop=F])
+ (thetas[,"B.I.two.N",, drop=F]  -thetas[,"B.U.two.N",, drop=F])) /2

WBshift <- function(thetas) ((thetas[,"B.U.one.W",, drop=F]  -
                                thetas[,"B.I.one.W",, drop=F])
+ (thetas[,"B.U.two.W",, drop=F]  -thetas[,"B.I.two.W",, drop=F])) /2

funs <- list(NBshift, WBshift)
eff.names <- c("NBshift", "WBshift")


multi_function_posterior <-
  function(fun_list, samples, eff.names = c()) {
    effects <-
      matrix(ncol = length(fun_list),
             nrow = length(samples$theta[, 1, ]))
    for (i in 1:length(fun_list))
      effects[, i] <- fun_list[[i]](samples$theta)
    colnames(effects) <- eff.names
    effects
  }

test <- lapply(samples_top, function(x)
  multi_function_onsamples(fun_list, x, eff.names))

N_bias_shift <- do.call("c", lapply(test, function(x) quantile(x[,1], probs=0.1)))
W_bias_shift <- do.call("c", lapply(test, function(x) quantile(x[,2], probs=0.1)))

N_bias_shift <- N_bias_shift[N_bias_shift<0]
W_bias_shift <- W_bias_shift[W_bias_shift<0]


not_very_biased <- unique(c(names(N_bias_shift), names(W_bias_shift)))

not_very_biased <- gsub(".10%", "", not_very_biased)




N_bias_shift <- do.call("c", lapply(test, function(x) mean(x[,1])))
W_bias_shift <- do.call("c", lapply(test, function(x) mean(x[,2])))

N_bias_shift <- N_bias_shift[N_bias_shift<0]
W_bias_shift <- W_bias_shift[W_bias_shift<0]


not_very_biased <- unique(c(names(N_bias_shift), names(W_bias_shift)))

not_very_biased <- gsub(".10%", "", not_very_biased)




