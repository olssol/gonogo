
#' Meet a specific threshold
#'
#' Check if the probability of the treatment effect better than the cut value is
#' more than the probaility threshold.
#'
#' @export
#'
gn_decision_single <- function(post_effect,
                               cut,
                               prob_thresh,
                               type_better = c("bigger", "less")) {

    type_better <- match.arg(type_better)

    if ("bigger" == type_better) {
        prob_better <- mean(post_effect >= cut)
    } else {
        prob_better <- mean(post_effect <= cut)
    }

    c(prob_better, prob_better > prob_thresh)
}


#' Go no-go decision
#'
#' @param rule_g0 1: meet lrv, 2: meet tv, 3: meet either, 4: meet both
#' @param rule_nogo 1: fail to meet lrv, 2: fail to meet tv, 3: fail to meet
#'     either, 4: fail to meet neither
#'
#' @export
#'
gn_decision <- function(post_effect,
                        lrv = 0.2, tv = 0.05,
                        thresh_lrv = 0.8, thresh_tv = 0.2,
                        rule_go = 1, rule_nogo = 2,
                        option = c("gofirst",
                                   "nogofirst",
                                   "consider",
                                   "conflict")) {

    option  <- match.arg(option)
    dec_lrv <- gn_decision_single(post_effect,
                                  cut = lrv,
                                  prob_thresh = thresh_lrv)

    dec_tv <- gn_decision_single(post_effect,
                                 cut = tv,
                                 prob_thresh = thresh_tv)

    dec_go   <- 1 == c(dec_lrv[2],
                       dec_tv[2],
                       dec_lrv[2] | dec_tv[2],
                       dec_lrv[2] & dec_tv[2])[rule_go]

    dec_nogo <- 1 == c(!dec_lrv[2],
                       !dec_tv[2],
                       !dec_lrv[2] | !dec_tv[2],
                       !dec_lrv[2] & !dec_tv[2])[rule_nogo]

    if (1 == dec_go & 1 == dec_nogo) {
        rst <- switch(option,
                      "gofirst"   = 1,
                      "nogofirst" = -1,
                      "consider"  = 0,
                      "conflict"  = NA)
    } else if (1 == dec_go) {
        rst <- 1
    } else if (1 == dec_nogo) {
        rst <- -1
    } else {
        rst <- 0
    }

    ## return
    rst <- c(lrv_p      = dec_lrv[1],
             lrv_meet   = dec_lrv[2],
             tv_p       = dec_tv[1],
             tv_meet    = dec_tv[2],
             go         = dec_go,
             nogo       = dec_nogo,
             decision   = rst,
             lrv        = lrv,
             tv         = tv,
             thresh_lrv = thresh_lrv,
             thresh_tv  = thresh_tv,
             rule_go    = rule_go,
             rule_nogo  = rule_nogo,
             option     = switch(option,
                                 gofirst   = 1,
                                 nogofirst = 2,
                                 consider  = 3),
             mean       = mean(post_effect),
             q25        = as.numeric(quantile(post_effect, 0.025)),
             q975       = as.numeric(quantile(post_effect, 0.975))
             )

    class(rst) <- "CLS_GONOGO_DEC_SINGLE"
    rst
}

#' Go no-go decision
#'
#'
#' @export
#'
gn_decision_all <- function(post_effect,
                            ...) {

    stopifnot("CLS_GONOGO_POSTS_ALL" %in% class(post_effect))

    rst_dec <- apply(post_effect$post_effect,
                     1,
                     gn_decision, ...)
    rst_dec <- t(rst_dec)

    ## result table
    rst <- post_effect$data %>%
        cbind(rst_dec %>%
              data.frame() %>%
              mutate(decision = case_when(
                         decision == -1 ~ "No Go",
                         decision == 0  ~ "Consider",
                         decision == 1  ~ "Go"))
              ) %>%
        select(-lrv,        -tv,
               -thresh_lrv, -thresh_tv,
               -rule_go,    -rule_nogo,
               -option)

    ## boundary
    bou <- list(go       = which(1  == rst_dec[, "decision"]),
                nogo     = which(-1 == rst_dec[, "decision"]),
                consider = which(0  == rst_dec[, "decision"]))

    rst <- list(dec_table = rst,
                dec_bound = bou)

    class(rst) <- "CLS_GONOGO_DEC_ALL"
    rst

}


#' Go no-go decision operating characteristics
#'
#'
#' @export
#'
gn_decision_opc <- function(para_all,
                            post_effect,
                            ...) {
    n_rep    <- max(para_all[, "rep"])
    dec_all  <- gn_decision_all(post_effect, ...)

    rst <- data.frame(para_all) %>%
        mutate(Decision = dec_all[, "decision"]) %>%
        mutate(N_Trt    = n,
               N_Ctl    = n * rand_ratio,
               Rate_Trt = true_trt,
               Rate_Ctl = true_ctl,
               Decision = case_when(
                   Decision == -1 ~ "No Go",
                   Decision == 0  ~ "Consider",
                   Decision == 1  ~ "Go")) %>%
        group_by(N_Trt, N_Ctl, Rate_Trt,
                 Rate_Ctl, Decision) %>%
        summarize(Count = n()) %>%
        mutate(Probability = Count / n_rep)


    dta_dec <- rst %>%
        ungroup() %>%
        select(Decision) %>%
        unique()

    data.frame(rst) %>%
        select(N_Trt, N_Ctl, Rate_Trt, Rate_Ctl) %>%
        unique() %>%
        cross_join(dta_dec) %>%bj
        left_join(rst) %>%
        mutate(Probability = if_else(is.na(Probability),
                                     0,
                                     Probability)) %>%
        select(-Count)
}

#' Get Binary decision OPC
#'
#'
#'
#' @export
#'
gn_decision_binary_opc <- function(dec_tbl,
                                   vec_true_rate_trt,
                                   true_rate_ctl = 1) {

    rst    <- NULL
    for (ptrt in vec_true_rate_trt) {

        dec_tbl <- dec_tbl %>%
            mutate(prob = dbinom(y1, n1, ptrt))

        if (!is.null(dec_tbl$n0)) {
            dec_tbl <- dec_tbl %>%
                mutate(prob = prob + dbinom(y0, n0, true_rate_ctl))
        }

        cur_rst <- dec_tbl %>%
            group_by(n1, decision) %>%
            summarize(prob = sum(prob)) %>%
            mutate(ptrt = ptrt)

        rst <- rbind(rst, cur_rst)
    }

    class(rst) <- c("CLS_OPC_BIN", "data.frame")
    rst
}

#' Print decision result
#'
#'
#' @method print CLS_OPC_BIN
#'
#' @export
#'
print.CLS_OPC_BIN <- function(x,
                              option = c("long", "wide_n", "wide_rate"),
                              ...) {

    option <- match.arg(option)
    rst    <- data.frame(x)
    if ("long" == option) {
        rst <- rst %>%
            select(ptrt, n1, decision, prob) %>%
            rename("Treatment Arm Size" = n1,
                   "Decision"           = decision,
                   "Probability"        = prob,
                   "Treatment Arm Rate" = ptrt)
    } else if ("wide_n" == option) {
        rst <- rst %>%
            select(ptrt, n1, decision, prob) %>%
            mutate(n1 = paste("N = ", n1, sep = "")) %>%
            pivot_wider(names_from = n1, values_from = prob) %>%
            rename("Decision"           = decision,
                   "Treatment Arm Rate" = ptrt)
    } else if ("wide_rate" == option) {
        rst <- rst %>%
            select(ptrt, n1, decision, prob) %>%
            mutate(ptrt = paste("Treatment Rate = ", ptrt, sep = "")) %>%
            pivot_wider(names_from = ptrt, values_from = prob) %>%
            rename("Decision"           = decision,
                   "Treatment Arm Size" = n1)
    }

    rst
}


## -----------------------------------------------------------------------------
##
##                              PLOT FUNCTIONS
##
## -----------------------------------------------------------------------------

#' Print decision result
#'
#'
#' @method plot CLS_GONOGO_DEC_ALL
#'
#' @export
#'
plot.CLS_GONOGO_DEC_ALL <- function(x,
                                    x_axis    = c("y1", "delta"),
                                    hl_region = TRUE, ...) {

    f_ann <- function(plt, vec, ...) {
        plt + annotate("rect",
                       xmin = min(vec) - step / 2,
                       xmax = max(vec) + step / 2,
                       ymin = -Inf,
                       ymax = Inf,
                       ...)
    }

    x_axis  <- match.arg(x_axis)
    dec_rst <- x
    dec_tbl <- dec_rst$dec_table
    dec_b   <- dec_rst$dec_bound
    x       <- dec_tbl[[x_axis]]
    step    <- x[2] - x[1]

    dta     <- rbind(data.frame(x    = x,
                                y    = dec_tbl$tv_p,
                                Type = "Prob(Delta > TV)"),
                     data.frame(x    = x,
                                y    = dec_tbl$lrv_p,
                                Type = "Prob(Delta > LRV)")
                     )

    rst <- ggplot(data = dta, aes(x = x, y = y)) +
        geom_line(aes(group = Type, color = Type)) +
        geom_point() +
        theme_bw()

    if (hl_region) {
        rst <- f_ann(rst,
                     vec   = x[dec_b$go],
                     alpha = 0.1,
                     fill  = "blue")
        rst <- f_ann(rst,
                     vec   = x[dec_b$nogo],
                     alpha = 0.1,
                     fill  = "red")

        if (length(dec_b$consider) > 0) {
            rst <- f_ann(rst,
                         vec   = x[dec_b$consider],
                         alpha = 0.1,
                         fill  = "yellow")
        }
    }

    rst
}

#' Print decision result
#'
#'
#' @method print CLS_GONOGO_DEC_SINGLE
#'
#' @export
#'
print.CLS_GONOGO_DEC_SINGLE <- function(x, ...) {

    data.frame(Summary = c(
                   "Treatment Effect (Mean)",
                   "Treatment Effect (95% CI)",
                   "LRV",
                   "LRV Threshold",
                   "Prob Treatment Effect better than LRV",
                   "LRV Threshold Met",
                   "TV",
                   "TV Threshold",
                   "Prob Treatment Effect better than TV",
                   "TV Threshold Met",
                   "Go Criteria Met",
                   "No-Go Criteria Met",
                   "Decision"),

               Value = c(
                   round(x["mean"], 3),
                   paste("(",
                         round(x["q25"], 3),
                         ",",
                         round(x["q975"], 3),
                         ")", sep = ""),
                   x["lrv"],
                   x["thresh_lrv"],
                   round(x["lrv_p"], 3),
                   if_else(1 == x["lrv_meet"], "Yes", "No"),
                   x["tv"],
                   x["thresh_tv"],
                   round(x["tv_p"], 3),
                   if_else(1 == x["tv_meet"], "Yes", "No"),
                   if_else(1 == x["go"], "Yes", "No"),
                   if_else(1 == x["nogo"], "Yes", "No"),
                   c("No Go", "Consider", "Go")[x["decision"] + 2]
               ))
}


#' Print decision result
#'
#'
#' @method plot CLS_OPC_BIN
#'
#' @export
#'
plot.CLS_OPC_BIN <- function(x,
                             option = c("line", "area"),
                             ...) {
    option <- match.arg(option)
    x      <- data.frame(x)
    switch(option,
           line = gn_opc_bin_plot_line(x, ...),
           area = gn_opc_bin_plot_area(x, ...))
}

#' Plot decision rates vs. treatment rate
gn_opc_bin_plot_line <- function(rst_opc, ...) {

    rst_opc <- data.frame(rst_opc) %>%
        mutate(n1 = paste("Treatment Arm Size = ", n1, sep = ""))

    rst <- ggplot(data = rst_opc, aes(x = ptrt, y = prob)) +
        geom_line(aes(group = decision, col = decision)) +
        facet_wrap(~n1, ncol = 1) +
        theme_bw() +
        labs(y = "Probability", x = "Treatment Rate") +
        theme(legend.position = "bottom")

    rst
}

#' Plot decision rates vs. treatment rate
gn_opc_bin_plot_area <- function(rst_opc, ...) {

    rst_opc <- data.frame(rst_opc) %>%
        mutate(n1 = paste("Treatment Arm Size = ", n1, sep = "")) %>%
        pivot_wider(names_from = decision, values_from = prob)

    for (dec in c("Go", "No Go", "Consider")) {
        if (is.null(rst_opc[[dec]]))
            rst_opc[[dec]] <- 0
    }

    rst_opc[["Consider"]] <- rst_opc[["Consider"]] + rst_opc[["Go"]]

    rst <- ggplot(data = rst_opc, aes(x = ptrt)) +
        geom_line(aes(y = Go)) +
        geom_ribbon(aes(ymin = 0, ymax = Go), fill = "green", ...) +
        geom_line(aes(y = Consider)) +
        geom_ribbon(aes(ymin = Go, ymax = Consider), fill = "yellow", ...) +
        geom_ribbon(aes(ymin = Consider, ymax = 1), fill = "red", ...) +
        facet_wrap(~n1, ncol = 1) +
        theme_bw() +
        labs(y = "Probability", x = "Treatment Rate")

    rst
}
