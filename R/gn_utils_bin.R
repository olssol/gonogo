## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
##
##                       SAMPLES
##
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

#' Beta samples
#'
#' Beta Binomial Single
#'
#' @export
#'
gn_beta_posts <- function(n, y, a = 1, b = 1, n_samples = 3000) {
    a <- a + y
    b <- b + n - y
    rbeta(n_samples, a, b)
}


#' Get posterior samples
#'
#' @export
#'
gn_beta_effect_posts <- function(n1, y1, n0 = NULL, y0 = NULL,
                                 phat_ctl = 0.2,
                                 pri_trt  = c(0.25, 0.25),
                                 pri_ctl  = c(0.25, 0.25),
                                 ...,
                                 seed  = NULL) {
    if (!is.null(seed))
        old_seed <- set.seed(seed)

    post_trt <- gn_beta_posts(n1, y1,
                              a = pri_trt[1],
                              b = pri_trt[2],
                              ...)

    if (is.null(n0)) {
        post_ctl <- phat_ctl
    } else {
        post_ctl <- gn_beta_posts(n0, y0,
                                  a = pri_ctl[1],
                                  b = pri_ctl[2],
                                  ...)
    }

    if (!is.null(seed))
        set.seed(old_seed)

    rst <- post_trt - post_ctl

    ## meta
    if (is.null(n0)) {
        delta <- y1 / n1 - phat_ctl
        data <- c(n1 = n1, y1 = y1, phat_ctl = phat_ctl, delta = delta)
    } else {
        delta <- y1 / n1 - y0 / n0
        data <- c(n1 = n1, y1 = y1, n0 = n0, y0 = y0, delta = delta)
    }


    rst <- list(data       = data,
                post_trt   = post_trt,
                post_ctl   = post_ctl,
                post_delta = rst)

    class(rst) <- "CLS_GONOGO_POSTS_SINGLE"

    rst
}

#' Get posterior samples all
#'
#' @export
#'
gn_beta_effect_posts_all <- function(n1, ..., n_cores = 1) {

    y1  <- seq(0, n1)
    rst <- parallel::mclapply(y1, function(x) {
                         cur_rst <- gn_beta_effect_posts(
                             n1   = n1,
                             y1   = x,
                             ...)

                     }, mc.cores = n_cores)


    rst_smps <- NULL
    rst_data <- NULL
    for (i in seq_len(length(rst))) {
        rst_smps <- rbind(rst_smps, rst[[i]]$post_delta)
        rst_data <- rbind(rst_data, rst[[i]]$data)
    }

    rst <- list(data        = rst_data,
                post_effect = rst_smps)

    class(rst) <- "CLS_GONOGO_POSTS_ALL"
    rst
}

#' Grid search
#'
#' Grid search
#'
#' @export
#'
gn_grid_based_method <- function(k, a10, b10, a11, b11, length_out = 101) {
    step      <- 1 / (length_out - 1)
    epsilon   <- step / 2
    cutpoints <- seq(0, 1, by = step)
    pp_ctrl   <- pbeta(cutpoints + epsilon, a10, b10) -
        pbeta(cutpoints - epsilon, a10, b10)
    pp_trt    <- pbeta(cutpoints + epsilon, a11, b11) -
        pbeta(cutpoints - epsilon, a11, b11)

    outer_prod <- pp_trt %o% pp_ctrl
    ind_mat    <- outer(cutpoints, cutpoints + k, ">")

    sum(outer_prod * ind_mat)
}


#' Resampling method
#'
#' resamping method
#'
#' @export
#'
#'
gn_resampling_method <- function(k, a10, b10, a11, b11, n_samples = 10001) {
    pp_ctrl <- rbeta(n_samples, a10, b10)
    pp_trt  <- rbeta(n_samples, a11, b11)

    mean(pp_trt - pp_ctrl > k)
}


#' Beta Binomial Single
#'
#' Beta Binomial Single
#'
#' @export
#'
gn_beta_single <- function(k_list, p0_hat, p1_hat, n1,
                           a01 = 1, b01 = 1) {
    a11 <- a01 + n1 * p1_hat
    b11 <- b01 + n1 * (1 - p1_hat)

    1 - pbeta(k_list + p0_hat, a11, b11)
}


## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
##
##                       SIMULATION
##
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

#' Simulate individual trial
#'
#'
#' @export
#'
gn_beta_simu_single <- function(n1, n0 = NULL, true_trt, true_ctl,
                                ...,
                                seed = NULL) {

    if (!is.null(seed))
        old_seed <- set.seed(seed)


    y1 <- rbinom(1, n1, prob = true_trt)

    if (is.null(n0)) {
        phat_ctl <- true_ctl
        y0       <- NULL
    } else {
        phat_ctl <- NULL
        y0       <- rbinom(1, n0, prob = true_ctl)
    }

    rst <- gn_beta_effect_posts(n1 = n1,
                                y1 = y1,
                                n0 = n0,
                                y0 = n0,
                                phat_ctl = phat_ctl,
                                ...)

    if (!is.null(seed))
        set.seed(old_seed)

    rst
}

#' Simulate all trials
#'
#' @param rand_ratio randomization ratio. 0 corresponds to single arm
#'
#' @export
#'
gn_beta_simu_all <- function(vec_n,
                             vec_true_trt,
                             vec_true_ctl,
                             rand_ratio = 0,
                             ...,
                             seed    = NULL,
                             n_cores = 4,
                             n_simu  = 500) {

    para_all <- expand.grid(n          = vec_n,
                            true_trt   = vec_true_trt,
                            true_ctl   = vec_true_ctl,
                            rand_ratio = rand_ratio,
                            rep        = seq_len(n_simu))

    if (!is.null(seed))
        old_seed <- set.seed(seed)

    rst <- parallel::mclapply(seq_len(nrow(para_all)),
                              function(j) {
                         n1       <- para_all[j, 1]
                         true_trt <- para_all[j, 2]
                         true_ctl <- para_all[j, 3]
                         n0       <- n1 * para_all[j, 4]
                         rep      <- para_all[j, 4]

                         if (0 == n0) {
                             n0 <- NULL
                         }

                         cur_rst <- gn_beta_simu_single(n1, n0, true_trt, true_ctl,
                                                        ...)

                         cur_rst[3, ]

                     })

    if (!is.null(seed))
        set.seed(old_seed)

    list(para_all    = para_all,
         post_effect = t(simplify2array(rst)))
}


#' Simulate all combinations
#'
#' @param rand_ratio randomization ratio. 0 corresponds to single arm
#'
#' @export
#'
gn_beta_simu_comb <- function(vec_n,
                              rand_ratio = 0,
                              phat_ctl   = NULL,
                              ...) {

    rst_data <- NULL
    rst_smps <- NULL
    for (n1 in vec_n) {
        n0 <- n1 * rand_ratio

        if (0 == n0) {
            stopifnot(!is.null(phat_ctl))

            cur_rst <- gn_beta_effect_posts_all(
                n1       = n1,
                phat_ctl = phat_ctl,
                ...)

            rst_data <- rbind(rst_data, cur_rst$data)
            rst_smps <- rbind(rst_smps, cur_rst$post_effect)
        } else {
            for (y0 in seq(0, n0)) {
                cur_rst <- gn_beta_effect_posts_all(
                    n1 = n1, n0 = n0, y0 = y0,
                    ...)

                rst_data <- rbind(rst_data, cur_rst$data)
                rst_smps <- rbind(rst_smps, cur_rst$post_effect)
            }
        }
    }

    rst <- list(data        = rst_data,
                post_effect = rst_smps)

    class(rst) <- "CLS_GONOGO_POSTS_ALL"
    rst
}



## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
##
##                       PLOT AND TABLE FUNCTIONS
##
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------


#' Plot prior
#'
#'
#' @export
#'
gn_beta_plt_prior <- function(p_trt, p_ctl = NULL,
                              x = seq(0.001, 0.999, by = 0.01)) {
    if (is.null(p_trt))
        return(NULL)

    dta <- data.frame(Arm     = "Treatment",
                      Rate    = x,
                      Density = dbeta(x, p_trt[1], p_trt[2]))

    if (!is.null(p_ctl)) {
        dta <- rbind(dta,
                     data.frame(Arm     = "Control",
                                Rate    = x,
                                Density = dbeta(x, p_ctl[1], p_ctl[2])))
    }

    rst <- ggplot(data = dta, aes(x = Rate, y = Density)) +
        geom_line(aes(group = Arm, color = Arm)) +
        theme_bw() +
        theme(legend.position = "bottom")

    rst
}

#' Plot posterior of observed data
#'
#' @method plot CLS_GONOGO_POSTS_SINGLE
#'
#' @export
#'
plot.CLS_GONOGO_POSTS_SINGLE <- function(x, option = c("effect", "rate"), ...) {
    post_smps <- x
    if (is.null(post_smps))
        return(NULL)


    option <- match.arg(option)

    if ("effect" == option) {
        dta <- data.frame(x = post_smps$post_delta)
    } else {
        dta <- data.frame(x   = post_smps$post_trt,
                          Arm = "Treatment")

        if (1 < length(post_smps$post_ctl)) {
            dta <- rbind(dta,
                         data.frame(x   = post_smps$post_trt,
                                    Arm = "Control"))
        }
    }

    rst <- ggplot(data = dta, aes(x = x)) +
        theme_bw() +
        labs(y = "Density") +
        theme(legend.position = "bottom")

    if ("effect" == option) {
        rst <- rst +
            labs(x = "Treatment Effect")
    } else {
        rst <- rst +
            xlim(0, 1) +
            labs(x = "Success Rate")
    }

    if ("effect" == option) {
        rst <- rst + geom_density(...)
    } else {
        rst <- rst + geom_density(aes(group = Arm, fill = Arm), ...)
    }

    rst
}
