
## -----------------------------------------------------------------
##
##             FUNCTIONS
##
## -----------------------------------------------------------------

## show different type of messages
msg_box <- function(contents, type = "info") {
  switch(type,
         info    = cls <- "cinfo",
         warning = cls <- "cwarning",
         success = cls <- "csuccess",
         error   = cls <- "cerror")
  rst <- '<div class="'
  rst <- paste(rst, cls, '">')
  rst <- paste(rst, contents)
  rst <- paste(rst, "</div>")

  withMathJax(HTML(rst))
}


func_tbl <- function() {

    tbl <- data.frame(round(t(res), 3),
                      row.names = paste0("n1=", n1_list))

    colnames(tbl) <- paste0("observed difference=", delta_list)

    datatable(tbl,
              extensions = 'Buttons',
              options = list(
                  paging = TRUE,
                  searching = TRUE,
                  fixedColumns = TRUE,
                  autoWidth = TRUE,
                  ordering = TRUE,
                  dom = 'tB',
                  buttons = c('copy', 'csv', 'excel')
              ),
              rownames = TRUE) %>%
        formatStyle(colnames(tbl),
                    background =
                        styleInterval(c(c2, c1),
                                      c("pink", "lightblue", "lightgreen")))

}

## -----------------------------------------------------------------
##
##             Panel: Design Parameter
##
## -----------------------------------------------------------------

par_desn_simu <- reactive({
    list(n_samples    = input$inLargeN,
         rand_seed    = input$inSeed,
         n_cores      = input$inNCores)
})

par_desn_dec <- reactive({
    dec_simp <- input$inChkDecSimp
    if (dec_simp) {
        tv_o       <- input$inLRV
        rule_go    <- 1
        rule_nogo  <- 2
        rule_opt   <- "gofirst"
    } else {
        tv_o       <- input$inTV
        rule_go    <- as.numeric(input$inRuleGo)
        rule_nogo  <- as.numeric(input$inRuleNoGo)
        rule_opt   <- input$inRuleOpt
    }

    list(dec_simp     = dec_simp,
         lrv_o        = input$inLRV,
         thresh_lrv_o = input$inThreshLRV,
         thresh_tv_o  = input$inThreshTV,
         tv_o         = tv_o,
         rule_go      = rule_go,
         rule_nogo    = rule_nogo,
         rule_opt     = rule_opt)
})

par_desn_study <- reactive({
    list(prior_trt    = tkt_assign(input$prior_trt),
         prior_ctl    = tkt_assign(input$prior_ctrl),
         is_two_arm   = "Two-arm randomized study" == input$arm)
})

plt_prior <- reactive({
    par_desn <- par_desn_study()
    p_trt    <- par_desn$prior_trt
    p_ctl    <- par_desn$prior_ctl
    is_two   <- par_desn$is_two_arm

    if (is.null(p_trt))
        return(NULL)

    if (is_two) {
        if (is.null(p_ctl))
            return(NULL)
    } else {
        p_ctl <- NULL
    }

    gn_beta_plt_prior(p_trt, p_ctl) +
        theme(text = element_text(size = 15))
})


## -----------------------------------------------------------------
##
##             Panel: Posterior Probabilities
##
## -----------------------------------------------------------------

par_pp_obs <- reactive({

    desn_study <- par_desn_study()
    n1         <- input$size_trt
    y1         <- input$n_suc_trt
    phat_trt   <- y1 / n1

    n0         <- NULL
    y0         <- NULL
    phat_ctl   <- NULL

    if (desn_study$is_two_arm) {
        n0       <- input$size_ctrl
        y0       <- input$n_suc_ctrl
        phat_ctl <- y0 / n0
    } else {
        phat_ctl <- input$rate_ctrl
    }

    list(n1       = n1,
         y1       = y1,
         n0       = n0,
         y0       = y0,
         phat_ctl = phat_ctl,
         phat_trt = phat_trt,
         effect   = phat_trt - phat_ctl)
})

par_pp_dec <- reactive({
    desn_dec   <- par_desn_dec()
    if (desn_dec$dec_simp) {
        tv         <- input$inLRV2
        lrv        <- input$inLRV2
        thresh_lrv <- input$inThreshLRV2
        thresh_tv  <- input$inThreshTV2
    } else {
        tv         <- input$in4TV2
        lrv        <- input$in4LRV2
        thresh_lrv <- input$in4ThreshLRV2
        thresh_tv  <- input$in4ThreshTV2
    }

    list(lrv          = lrv,
         thresh_lrv   = thresh_lrv,
         tv           = tv,
         thresh_tv    = thresh_tv)
})

par_pp_obs_opt <- reactive({
    list(hl    = input$inPPObsHl2,
         xaxis = input$inPPObsX2)
})

par_pp_exp_opt <- reactive({
    list(hl    = input$inPPHl,
         xaxis = input$inPPx,
         tbl   = input$inPPFilTbl)
})

## get observed posterior samples
get_pp_post_obs <- reactive({
    pp_obs     <- par_pp_obs()
    desn_study <- par_desn_study()
    desn_simu  <- par_desn_simu()

    gn_beta_effect_posts(n1         = pp_obs$n1,
                         y1         = pp_obs$y1,
                         n0         = pp_obs$n0,
                         y0         = pp_obs$y0,
                         phat_ctl   = pp_obs$phat_ctl,
                         pri_trt    = desn_study$prior_trt,
                         pri_ctl    = desn_study$prior_ctl,
                         n_samples  = desn_simu$n_samples,
                         seed       = desn_simu$rand_seed)
})

## decision
get_pp_dec_obs <- reactive({
    post_smps <- get_pp_post_obs()
    if (is.null(post_smps))
        return(NULL)

    desn_dec <- par_desn_dec()
    dec_obs  <- gn_decision(post_smps$post_delta,
                            lrv        = desn_dec$lrv_o,
                            tv         = desn_dec$tv_o,
                            thresh_lrv = desn_dec$thresh_lrv_o,
                            thresh_tv  = desn_dec$thresh_tv_o,
                            rule_go    = desn_dec$rule_go,
                            rule_nogo  = desn_dec$rule_nogo,
                            option     = desn_dec$rule_opt)

    print(dec_obs)
})


## posterior of observed
plt_pp_post_obs <- reactive({

    post_smps <- get_pp_post_obs()
    if (is.null(post_smps))
        return(NULL)

    pp_obs_opt <- par_pp_obs_opt()
    desn_dec   <- par_desn_dec()
    rst        <- plot(post_smps,
                       option = pp_obs_opt$xaxis,
                       alpha  = 0.1,
                       adjust = 1.5) +
        theme_bw() +
        theme(text = element_text(size = 15))

    if ("effect" == pp_obs_opt$xaxis) {
        if ("lrv" %in% pp_obs_opt$hl) {
            rst <- rst +
                geom_vline(xintercept = desn_dec$lrv_o,
                           lty = 2, col = "red")
        }

        if ("tv" %in% pp_obs_opt$hl) {
            rst <- rst +
                geom_vline(xintercept = desn_dec$tv_o,
                           lty = 2, col = "blue")
        }

    }
    rst
})


## get posterior effects samples
get_pp_post_explore <- reactive({

    pp_obs     <- par_pp_obs()
    desn_study <- par_desn_study()
    desn_simu  <- par_desn_simu()

    gn_beta_effect_posts_all(n1         = pp_obs$n1,
                             n0         = pp_obs$n0,
                             y0         = pp_obs$y0,
                             phat_ctl   = pp_obs$phat_ctl,
                             pri_trt    = desn_study$prior_trt,
                             pri_ctl    = desn_study$prior_ctl,
                             n_samples  = desn_simu$n_samples,
                             seed       = desn_simu$rand_seed,
                             n_cores    = desn_simu$n_cores)
})

## post prob decisions
get_pp_dec_tbl <- reactive({
    post_smps <- get_pp_post_explore()
    if (is.null(post_smps)) {
        return(NULL)
    }

    pp_dec   <- par_pp_dec()
    desn_dec <- par_desn_dec()

    dat_dec  <- gn_decision_all(
        post_smps,
        lrv        = pp_dec$lrv,
        tv         = pp_dec$tv,
        thresh_lrv = pp_dec$thresh_lrv,
        thresh_tv  = pp_dec$thresh_tv,
        rule_go    = desn_dec$rule_go,
        rule_nogo  = desn_dec$rule_nogo,
        option     = desn_dec$rule_opt
    )

    dat_dec
})


## xaxis for the pp plot
get_plt_xaxis <- reactive({
    pp_exp_opt <- par_pp_exp_opt()
    pp_obs     <- par_pp_obs()
    x          <- seq(0, pp_obs$n1)
    if ("effect" == pp_exp_opt$xaxis) {
        xlab <- "Treatment Effect"
        x    <- x / pp_obs$n1 - pp_obs$phat_ctl
    } else {
        xlab <- "Treatment Successes"
    }

    list(xlab = xlab,
         x    = x)
})

## plot of posterior probabilities
plt_pp_dec_map <- reactive({
    dec_tbl <- get_pp_dec_tbl()
    xaxis   <- get_plt_xaxis()
    x_axis  <- switch(xaxis$xlab,
                      "Treatment Effect"    = "delta",
                      "Treatment Successes" = "y1")

    pp_dec     <- par_pp_dec()
    pp_exp_opt <- par_pp_exp_opt()
    pp_obs     <- par_pp_obs()

    rst <- plot(dec_tbl,
                x_axis    = x_axis,
                hl_region = "regn" %in% pp_exp_opt$hl) +
        theme(text = element_text(size = 10),
              legend.position = "bottom") +
        labs(y = "Probability", x = xaxis$xlab) +
        theme(text = element_text(size = 15))

    if ("lrv" %in% pp_exp_opt$hl) {
        rst <- rst +
            geom_hline(yintercept = pp_dec$thresh_lrv,
                       lty = 2, col = "red")
    }

    if ("tv" %in% pp_exp_opt$hl) {
        rst <- rst +
            geom_hline(yintercept = pp_dec$thresh_tv,
                       lty = 2, col = "blue")
    }

    if ("obs" %in% pp_exp_opt$hl) {
        if ("Treatment Effect" == xaxis$xlab) {
            rst <- rst +
                geom_vline(xintercept = pp_obs$effect, lty = 2, col = "red")
        } else {
            rst <- rst +
                geom_vline(xintercept = pp_obs$y1, lty = 2, col = "red")
        }
    }

    rst
})


## -----------------------------------------------------------------
##
##             Operating Characteristics
##
## -----------------------------------------------------------------

par_opc_truth <- reactive({
    desn_study <- par_desn_study()
    if (desn_study$is_two_arm) {
        rand_ratio <- input$rand_ratio
    } else {
        rand_ratio <- 0
    }

    list(n1          = tkt_assign(input$opc_size_trt),
         true_trt    = tkt_assign(input$opc_prop_suc_trt),
         true_ctl    = input$opc_prop_suc_ctl,
         rand_ratio  = rand_ratio)
})

## opc decision parameters
par_opc_dec <- reactive({
    desn_dec <- par_desn_dec()

    if (desn_dec$dec_simp) {
        tv         <- input$inLRV3
        lrv        <- input$inLRV3
        thresh_lrv <- input$inThreshLRV3
        thresh_tv  <- input$inThreshTV3
    } else {
        tv         <- input$in4TV3
        lrv        <- input$in4LRV3
        thresh_lrv <- input$in4ThreshLRV3
        thresh_tv  <- input$in4ThreshTV3
    }

    list(lrv          = lrv,
         thresh_lrv   = thresh_lrv,
         tv           = tv,
         thresh_tv    = thresh_tv)
})

par_opc_opt <- reactive({
    list(tbl   = input$inOPCtblOpt,
         map   = input$inOPCMapOpt)
})

## get simulation results for OPC
get_opc_comb <- reactive({

    if (0 == input$btnSimu)
        return(NULL)

    opc_truth  <- par_opc_truth()
    desn_study <- par_desn_study()
    desn_simu  <- par_desn_simu()

    rst_comb <- gn_beta_simu_comb(
        vec_n        = opc_truth$n1,
        rand_ratio   = opc_truth$rand_ratio,
        phat_ctl     = opc_truth$true_ctl,
        pri_trt      = desn_study$prior_trt,
        pri_ctl      = desn_study$prior_ctl,
        seed         = desn_simu$rand_seed,
        n_cores      = desn_simu$n_cores)

    rst_comb
}) %>%
    bindEvent(input$btnSimu)

## opc decision
get_opc_dec_tbl <- reactive({
    if (0 == input$btnSimu)
        return(NULL)

    rst_comb <- get_opc_comb()
    if (is.null(rst_comb))
        return(NULL)

    opc_dec  <- par_opc_dec()
    desn_dec <- par_desn_dec()

    rst <- gn_decision_all(
        rst_comb,
        lrv        = opc_dec$lrv,
        tv         = opc_dec$tv,
        thresh_lrv = opc_dec$thresh_lrv,
        thresh_tv  = opc_dec$thresh_tv,
        rule_go    = desn_dec$rule_go,
        rule_nogo  = desn_dec$rule_nogo,
        option     = desn_dec$rule_opt)

    rst
})

## opc
get_opc_tbl <- reactive({
    dec_tbl   <- get_opc_dec_tbl()
    if (is.null(dec_tbl))
        return(NULL)

    opc_truth <- par_opc_truth()
    opc_opt   <- par_opc_opt()
    rst_opc   <- gn_decision_binary_opc(
        dec_tbl$dec_table,
        opc_truth$true_trt,
        opc_truth$true_ctl)

    print(rst_opc,
          option = opc_opt$tbl)
})

## opc plot
plt_opc_dec_map <- reactive({
    dec_tbl  <- get_opc_dec_tbl()
    if (is.null(dec_tbl))
        return(NULL)


    opc_truth <- par_opc_truth()
    opc_opt   <- par_opc_opt()
    rst_opc   <- gn_decision_binary_opc(
        dec_tbl$dec_table,
        seq(0, 1, by = 0.02),
        opc_truth$true_ctl)

    rst <- plot(rst_opc,
                opc_opt$map,
                alpha = 0.2) +
        theme(text = element_text(size = 15))

    if (input$inOpcHl) {
        for (ptrt in opc_truth$true_trt) {
            rst <- rst +
                geom_vline(xintercept = ptrt, lty = 2, col = "red")
        }
    }

    rst
})
