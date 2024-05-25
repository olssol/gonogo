##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##
##
##                UI FUNCTIONS
##
##
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------

txt_all <- function(type) {
    rst <- NULL

    if ("decision" == type) {
        rst <- "We provide two decision rules here, where <b>delta</b>
           indicates the true
           difference of rates between treatment and control. <br>
           <b>Target value (TV):</b> desired effect to potentially establish the
           compound as the treatment of choice. <br>
           <b>Lower reference value (LRV):</b> smallest clinically
           meaningful treatment
           effect (dignity line) for the development of a compound. <br>
           <b>Target region cutoff (k): </b> the cutoff for delta when we do not
           distinguish TV and LRV. This is the desirable improvement
           of treatment
           versus control. <br>
           <b>c1</b>: the Go threshold for the posterior probability. Typically,
           c1 is big, e.g., 0.8 or 0.9. The greater the c1, the harder to make
           <q>Go</q> decision. <br>
           <b>c2</b>: the No-Go threshold for the posterior probability.
           Typically,
           c2 is small, e.g., 0.1 or 0.2. The smaller the c2, the harder to make
           <q>No-Go</q> decision."
    }

    if ("grid" == type) {
        rst <- "<p>Grid: the posterior probabilities are calculated
                based on grid based method. This are the same as EAST. </p>
                <p> Resampling: the posterior probabilities are calculated
                based on resampling. </>"
    }

    if ("dec2" == type) {
        rst <- "<p>Please refer to the Technical Details for the details of LRV, TV and
                Go/No-Go criteria.</p> <p>Specifically, the probability threshold for
                meeting LRV or TV refers to the probability rate above which the treatment
                effect \\(\\Delta\\) is better than the LRV or TV value.
                </p><p> Moreover, situations where neiter Go or No-Go criteria are met
                will correspond to making the <b>'Consider'</b> decision.</p>"
    }

    if ("dec2simp" == type) {
        rst <- "<p> 'Go' if the probability that the treatment effect better than
                'Treatment Effect Threshold' is more than 'Probability Threshold 1';
                'No o' if the probability that the treatment effect better than
                'Treatment Effect Threshold' is less than 'Probability Threshold 2'.
               </p>"
    }

    rst
}


panel_explore_setting <- function(suffix = 2) {
    div(
        sliderInput(paste("inLRV", suffix, sep = ""),
                    "LRV",
                    value = 0,
                    min  = 0,
                    max  = 1,
                    step = 0.01),

        sliderInput(paste("inThreshLRV", suffix, sep = ""),
                    "Probability threshold meeting LRV",
                    value = 0.8,
                    min  = 0,
                    max  = 1,
                    step = 0.01),

        sliderInput(paste("inTV", suffix, sep = ""),
                    "TV",
                    value = 0,
                    min  = 0,
                    max  = 1,
                    step = 0.01),

        sliderInput(paste("inThreshTV", suffix, sep = ""),
                    "Probability threshold meeting TV",
                    value = 0.1,
                    min  = 0,
                    max  = 1,
                    step = 0.01),
        )

}

panel_explore_setting_simp <- function(suffix = 2) {
    div(
        sliderInput(paste("inLRV", suffix, sep = ""),
                    "Treatment Effect Threshold",
                    value = 0,
                    min  = 0,
                    max  = 1,
                    step = 0.01),

        sliderInput(paste("inThreshLRV", suffix, sep = ""),
                    "Probability threshold 1",
                    value = 0.8,
                    min  = 0,
                    max  = 1,
                    step = 0.01),

        sliderInput(paste("inThreshTV", suffix, sep = ""),
                    "Probability threshold 2",
                    value = 0.1,
                    min  = 0,
                    max  = 1,
                    step = 0.01),
        )
}


panel_dec_para_simp <- function() {
    fluidRow(
        msg_box(txt_all("dec2simp")),
        column(3,
               numericInput(
                   "inLRV",
                   "Treatment Effect Threshold",
                   0,
                   min = 0, max = 1, step = 0.01)),
        column(3,
               numericInput(
                   "inThreshLRV",
                   "Probability Threshold 1",
                   value = 0.8,
                   min = 0, max = 1, step = 0.01)),
        column(3,
               numericInput(
                   "inThreshTV",
                   "Probability Threshold 2",
                   value = 0.1,
                   min = 0, max = 1, step = 0.01)
               )
    )
}

panel_dec_para <- function() {
    fluidRow(
        msg_box(txt_all("dec2")),
        column(6,
               wellPanel(
                   style  = "background: gray90; height: 270px",
                   fluidRow(
                       column(6,
                              h4("Low Reference Value (LRV)"),
                              numericInput(
                                  "inLRV",
                                  "Value",
                                  0,
                                  min = 0, max = 1, step = 0.01),
                              numericInput(
                                  "inThreshLRV",
                                  "Probability Threshold for Meeting LRV",
                                  value = 0.8,
                                  min = 0, max = 1, step = 0.01)
                              ),
                       column(6,
                              h4("Target Value (TV)"),
                              numericInput(
                                  "inTV",
                                  "Value",
                                  0,
                                  min = 0, max = 1, step = 0.01),
                              numericInput(
                                  "inThreshTV",
                                  "Probability Threshold for Meeting TV",
                                  value = 0.1,
                                  min = 0, max = 1, step = 0.01)
                              )
                   )
               )
               ),

        column(6,
               wellPanel(
                   style  = "background: gray90; height: 270px",
                   fluidRow(
                       column(6,
                              h4("Go Criteria"),
                              radioButtons(
                                  "inRuleGo",
                                  "",
                                  c("Meeting LRV"             = 1,
                                    "Meeting TV"              = 2,
                                    "Meeting LRV or TV"       = 3,
                                    "Meeting both LRV and TV" = 4),
                                  select = 1)
                              ),
                       column(6,
                              h4("No-Go Criteria"),
                              radioButtons(
                                  "inRuleNoGo",
                                  "",
                                  c("Failing to meet LRV"              = 1,
                                    "Failing to meet TV"               = 2,
                                    "Failing to meet either LRV or TV" = 3,
                                    "Failing to meet both LRV and TV"  = 4),
                                  select = 2)
                              )
                   ),

                   radioButtons("inRuleOpt",
                                "If both Go and No-Go criteria are met, then",
                                c("Go"       = "gofirst",
                                  "No-Go"    = "nogofirst",
                                  "Consider" = "consider"),
                                select = "consider",
                                inline = TRUE)
               ))
    )
}


hlabel <- function(x) {
    HTML(paste('<label class = "control-label">',
               x,
               '</label>',
               sep = ""))
}



panel_bin_prior <- function() {
    div(
        msg_box("Specify beta prior for binary outcomes.
                 For example, '0.25, 0.25' specifies a non-informative
                 beta prior that corresponds prior data of 0.25 success
                 and 0.25 failure."),

        fluidRow(
        column(3,
               textInput("prior_trt",
                         "Prior for treatment",
                         "0.25, 0.25")),

        conditionalPanel(
            condition = "input.arm == 'Two-arm randomized study'",
            column(3,
                   textInput("prior_ctrl",
                             "Prior for control",
                             "0.25, 0.25"))
        ))
    )
}

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##
##
##           MAIN TABS
##
##
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------

##define the main tabset
tab_main <- function() {
    tabsetPanel(type    = "pills",
                id      = "mainpanel",
                tab_basic()
                ,tab_pp()
                ,tab_opc()
                ,tab_slides()
                ## ,tab_download()
                )
}


tab_slides <- function() {
    tabPanel("Technical Details",
             tags$iframe(src    = "slides.html",
                         style  = "border:none;width:100%",
                         height = 800),
             )
}


## basic design parameters
tab_basic <- function() {
    tabPanel("Design Parameters",
             wellPanel(
                 msg_box("Please specify the parameters of the clinical study,
                          the simulation, and the decision criteria.")
             ),

             wellPanel(
                 h3("Design Parameters"),
                 fluidRow(
                     column(3,
                            h4("Study and Outcome"),
                            radioButtons("arm",
                                         "",
                                         c("Single arm study",
                                           "Two-arm randomized study"),
                                         "Single arm study",
                                         inline = FALSE),

                            selectInput(inputId = "dist",
                                        label = "",
                                        choices = list("Binary",
                                                       "Continuous",
                                                       "Count",
                                                       "Time-to-event"),
                                        selected = "Binary"),

                            conditionalPanel(
                                condition = "input.arm == 'Two-arm randomized study'",
                                msg_box(txt_all("grid")),
                                radioButtons("method",
                                             "Posterior Probability Calulation",
                                             c("Grid", "Resampling"),
                                             "Resampling",
                                             inline = TRUE))
                            ),
                     column(9,
                            h4("Specify Priors"),
                            conditionalPanel(
                                condition = "input.dist == 'Binary'",
                                panel_bin_prior()),

                            conditionalPanel(
                                condition = "input.dist != 'Binary'",
                                fluidRow(
                                column(9,
                                       "The prior information panel will be added."))
                            ),

                            checkboxInput("inChkShowPrior",
                                          "Show prior distributions",
                                          FALSE),

                            conditionalPanel(
                                condition = "input.inChkShowPrior == true",
                                style = "padding: 10px; height: 420px",
                                plotOutput("pltPriors", height = "350px")
                            )
                            )
                 )),

             wellPanel(
                 h3("Decision-Making Parameters"),
                 checkboxInput("inChkDecSimp",
                               "Use the simplified version",
                               value = TRUE),

                 conditionalPanel(
                     condition = "input.inChkDecSimp == true",
                     panel_dec_para_simp()
                 ),

                 conditionalPanel(
                     condition = "input.inChkDecSimp == false",
                     panel_dec_para()
                 )
             ),

             wellPanel(
                 h3("Simulation Study Parameters"),
                 fluidRow(
                     column(3,
                            numericInput(inputId = "inNSimu",
                                         label   = "Number of simulations:",
                                         value   = 200),
                            ),
                     column(3,
                            numericInput("inSeed",
                                         "Choose a seed for simulation",
                                         100)),
                     column(3,
                            numericInput("inLargeN",
                                         "Number of posterior samples",
                                         3000)),
                     column(3,
                            numericInput("inNCores",
                                         "Number of parallel cores",
                                         4))
                 )
             ))
}

## posterior probabilities
tab_pp <- function() {
    tabPanel("Posterior Probabilities",
             wellPanel(
                 msg_box("On this panel, the software analyzes observed data,
                          calculates the posterior probabilities, and
                          suggests the go/no-go decision based on
                          the various design parameters.")
             ),

             wellPanel(
                 h3("Observed Data"),
                 fluidRow(
                     column(3,
                            sliderInput("size_trt",
                                        label = "Treatment size",
                                        value = 50,
                                        min = 20,
                                        max = 300)),
                     column(3,
                            sliderInput("n_suc_trt",
                                        label = "Observed treatment successes",
                                        value = 30,
                                        min = 0,
                                        max = 300)
                            ),

                     conditionalPanel(
                         condition = "input.arm == 'Two-arm randomized study'",
                         column(3,
                                sliderInput("size_ctrl",
                                            label = "Control size",
                                            value = 50,
                                            min = 20,
                                            max = 300)),
                         column(3,
                                sliderInput("n_suc_ctrl",
                                            label = "Observed control successes",
                                            value = 10,
                                            min = 0,
                                            max = 300))
                     ),

                     conditionalPanel(
                         condition = "input.arm != 'Two-arm randomized study'",
                         column(3,
                                numericInput(inputId = "rate_ctrl",
                                             label = "Assumed rate for control arm:",
                                             value = 0.2,
                                             min = 0,
                                             max = 1,
                                             step = 0.01))
                     )
                 )),

             wellPanel(
                 h3("Decision"),
                 fluidRow(
                     column(5,
                            tableOutput("tblDec")
                            ),
                     column(7,
                            h4("Posterior distributions based on observed data"),

                            fluidRow(
                                column(6,
                                       radioButtons(
                                           "inPPObsX2",
                                           "",
                                           c("Treatment effect" = "effect",
                                             "Success rate"     = "rate"),
                                           inline = TRUE)),
                                column(6,

                                       conditionalPanel(
                                           condition = "input.inPPObsX2 == 'effect'",
                                           checkboxGroupInput(
                                               "inPPObsHl2",
                                               "",
                                               c("Highlight LRV" = "lrv",
                                                 "Highlight TV"  = "tv"),
                                               selected = c("lrv", "tv"),
                                               inline = TRUE)
                                       ))),

                            plotOutput("pltPPobs2")
                            )
                 )
             ),

             wellPanel(
                 h3("Exploration"),
                 fluidRow(
                     column(3,
                            wellPanel(
                                style = "background: #e4dee7d1; height: 700px",
                                h4("Setting"),
                                conditionalPanel(
                                    condition = "input.inChkDecSimp == true",
                                    panel_explore_setting_simp("2")
                                ),

                                conditionalPanel(
                                    condition = "input.inChkDecSimp == false",
                                    panel_explore_setting("2")
                                ),

                                conditionalPanel(
                                    condition = "input.tabDec == 'Decision Table'",
                                    checkboxGroupInput("inPPFilTbl",
                                                       "Filter Decision Table",
                                                       c("Go", "No Go", "Consider"),
                                                       selected = c("Go",
                                                                    "No Go",
                                                                    "Consider"))
                                ),

                                conditionalPanel(
                                    condition = "input.tabDec == 'Decision Map'",
                                    radioButtons("inPPx",
                                                 "Show x-axis as",
                                                 c("Treatment effect"    = "effect",
                                                   "Treatment successes" = "n")),

                                    checkboxGroupInput(
                                        "inPPHl",
                                        "Highlight",
                                        choices = c("Observed Data" = "obs",
                                                    "Decision Region" = "regn",
                                                    "LRV Threshold"   = "lrv",
                                                    "TV Threshold"    = "tv"),
                                        selected = c("obs", "regn", "lrv", "tv")
                                    )
                                )
                            )),

                     column(9,
                            wellPanel(
                                style = "background: #e4dee7d1; height: 700px",
                                tabsetPanel(
                                    id = "tabDec",
                                    tabPanel("Decision Table",
                                             DT::dataTableOutput("dtbDec")),
                                    tabPanel("Decision Map",
                                             plotOutput("pltPPDecMap",
                                                        height = "600px")),
                                    selected = "Decision Map"
                                )
                            ))
                 ))
             )
}

## operating characteristics
tab_opc <- function() {
    tabPanel("Operating Characteristics",
             wellPanel(
                 msg_box("On this panel, the software conducts simulation studies
                          and generates the study operating characteristics based on the
                          simulation settings and various design parameters related to the
                          decision criteria.")
             ),

             wellPanel(
                 h3("Simulation Setting"),
                 msg_box("Split numbers using comma ','"),
                 fluidRow(
                     column(3,
                            textInput("opc_prop_suc_trt",
                                      label = "True rates for treatment",
                                      value = "0.2, 0.5")
                            ),
                     column(3,
                            numericInput("opc_prop_suc_ctl",
                                         label = "True rate for control",
                                         value = 0.2,
                                         min   = 0,
                                         max   = 1,
                                         step  = 0.01)
                            ),
                     column(3,
                            textInput("opc_size_trt",
                                      label = "Sample size (treatment)",
                                      value = "20, 30, 40")
                            ),

                     conditionalPanel(
                         condition = "input.arm == 'Two-arm randomized study'",
                         column(3,
                                numericInput(
                                    "rand_ratio",
                                    label = "Random ratio (control vs. treatment)",
                                    value = 1)))
                 ),

                 actionButton("btnSimu", "Conduct Simulation")
             ),

             wellPanel(
                 h3("Operating Characteristics"),
                 fluidRow(
                     column(3,
                            wellPanel(
                                style = "background: #e4dee7d1; height: 700px",
                                h4("Setting"),
                                conditionalPanel(
                                    condition = "input.inChkDecSimp == true",
                                    panel_explore_setting_simp("3")
                                ),

                                conditionalPanel(
                                    condition = "input.inChkDecSimp == false",
                                    panel_explore_setting("3")
                                ),

                                conditionalPanel(
                                    condition = "input.tabDec3 == 'Decision Map'",
                                    checkboxInput(
                                        "inOpcHl",
                                        "Highlight true treatment arm rates",
                                        TRUE)),

                                conditionalPanel(
                                    condition = "input.tabDec3 == 'Decision Table'",
                                    checkboxGroupInput("inOPCtype",
                                                       "Table Option",
                                                       c("Go", "Consider", "No Go"),
                                                       selected = c("Go", "Consider", "No Go")
                                                       ),
                                    radioButtons("inOPCtblOpt",
                                                 "Organize the decision table by",
                                                 c("Sample Size" = "wide_n",
                                                   "True Rate"   = "wide_rate"),
                                                 selected = "wide_n")
                                ),

                                conditionalPanel(
                                    condition = "input.tabDec3 == 'Decision Map'",
                                    radioButtons("inOPCMapOpt",
                                                 "Map Option",
                                                 c("As line" = "line",
                                                   "As area" = "area"),
                                                 selected = "line")
                                )
                            )
                            ),
                     column(9,
                            wellPanel(
                                style = "background: #e4dee7d1; height: 700px",
                                tabsetPanel(
                                    id = "tabDec3",
                                    tabPanel("Decision Table",
                                             DT::dataTableOutput("dtbOpcDec")),
                                    tabPanel("Decision Map",
                                             plotOutput("pltOpcDecMap",
                                                        height = "600px")),
                                    selected = "Decision Map"
                                ))
                            )
                  )
             ))
}

## download results
tab_download <- function() {
    tabPanel("Download Report")
}
