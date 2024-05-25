options(shiny.maxRequestSize = 200 * 1024 ^ 2)

library(data.table)
library(DT)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(parallel)
library(plotly)
library(gonogo)

shinyServer(function(input, output, session) {

    source("design_ui.R",    local = TRUE)
    source("design_utils.R", local = TRUE)

    userLog <- reactiveValues()

    ##--------------------------------------
    ##---------main-------------------------
    ##--------------------------------------
    output$mainpage <- renderUI({
        tab_main()
    })

    ##--------------------------------------
    ##---------exit-------------------------
    ##--------------------------------------
    observeEvent(input$close, {
        stopApp()})


    ##--------------------------------------
    ##---------design para------------------
    ##--------------------------------------
    output$pltPriors <- renderPlot({
        plt_prior()
    })

    ##--------------------------------------
    ##---------post prob--------------------
    ##--------------------------------------
    output$pltPPDecMap <- renderPlot({
        plt_pp_dec_map()
    })

    output$pltPPobs2 <- renderPlot({
        plt_pp_post_obs()
    })

    output$tblDec <- renderTable({
        get_pp_dec_obs()
    })

    output$dtbDec <- DT::renderDataTable({
                             dat <- get_pp_dec_tbl()

                             if (is.null(dat)) {
                                 return(NULL)
                             }

                             pp_opt <- par_pp_exp_opt()
                             datatable(dat$dec_table %>%
                                       filter(decision %in% pp_opt$tbl),
                                       options = list(paging     = FALSE,
                                                      scrollX    = TRUE,
                                                      scrollY    = "450px")
                                       ) %>%
                                 formatRound(columns = c("delta", "lrv_p", "tv_p",
                                                         "mean", "q25", "q975"),
                                             digits  = 3)
                         })

    ##--------------------------------------
    ##---------OPC--------------------------
    ##--------------------------------------
    output$dtbOpcDec <- DT::renderDataTable({
                                dat <- get_opc_tbl()

                                if (is.null(dat)) {
                                    return(NULL)
                                }

                                datatable(dat %>%
                                          filter(Decision %in% input$inOPCtype),
                                          options = list(paging     = FALSE,
                                                         scrollX    = TRUE,
                                                         scrollY    = "450px")
                                          ) %>%
                                 formatRound(columns = 3:ncol(dat),
                                             digits  = 3)
                            })

    output$pltOpcDecMap <- renderPlot({
        plt_opc_dec_map()
    })


})
