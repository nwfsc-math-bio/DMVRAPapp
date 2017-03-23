#.libPaths(c("/usr/lib64/R/shiny_library",.libPaths()))
library("appFrame")

source("version.R")

appTitle <- paste0("<span title='Shiny app version / DM version /",
                   " VRAP version'",
                   "style='font-weight:700;font-size:24px'>",
                   shiny.version)

vrapVersion <- packageVersion("VRAP")
dmVersion <- packageVersion("DM")

if (!is.null(dmVersion) && length(dmVersion) > 0) {
  appTitle <- paste0(appTitle, " / ", dmVersion)
} else {
  appTitle <- paste0(appTitle, " / ")
}

if (!is.null(vrapVersion) && length(vrapVersion) > 0) {
  appTitle <- paste0(appTitle, " / ", vrapVersion)
}

appTitle <- paste0(appTitle, "</span>")

shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="style.css"),
      tags$script(includeHTML("www/timer2.js")),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setwaitmsg',
                                     function(msg) {
                                     $('#waitmsg').html(msg);
                                    })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setmsgclass',
                                     function(msg) {
                                     $('#spinnerhost').attr('class',msg);
                                    })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setprogressdisplay',
                                     function(disp) {
                                     $('#progresscounter').css('display',disp);
                                    })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setusermsg',
                                     function(msg) {
                                     $('#msghostmsg').html(msg);
                                     $('#msghost').attr('class', 'msgdisplay');
                                    })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('clearusermsg',
                                     function(msg) {
                                     $('#msghostmsg').html('');
                                     $('#msghost').attr('class', 'nomsgdisplay');
                                    })"
             )
      )
    ),
    appFrameHeaderFixed(),
    ## headerPanel("Dynamic Model + VRAP 1.1"),
    headerPanel(HTML(appTitle), windowTitle="DM + VRAP"),
    conditionalPanel(
      condition=paste(
        "(updateBusy()) || ",
        "$('html').attr('class')=='shiny-busy' ||",
        "$('#spinnerhost').hasClass('busy')",
        sep=""),
      div(" "),
      div(id="spinnerhost",
          class="notbusy",
          p(id="waitmsg", " "),
          img(src="spinner.gif",
              alt=paste("Busy, please wait",
                        "(Close browser window to stop job)"
                        )
              ),
          p(' '),
          div(id="progress")
          )
    ),
    conditionalPanel(
      condition="$('#msghost').hasClass('msgdisplay')",
      div(id="msghost",
          class="nomsgdisplay",
          p(id="msghostmsg"," "),
          actionButton("msgclosebtn","Dismiss message")
          )
    ),
    tabsetPanel(
      id="DM_VRAP_tabs",
      tabPanel(
        title = "DM",
        value = "dmtab",
        sidebarLayout(
          sidebarPanel(
            width=5,
            div(id="spacer"," ",style="min-height:35px;float:left"),
            div(uiOutput("runbutton")),
            tags$br(),
            selectInput("type",
                        paste("Select an input type",
                              "(A&P XLSX or a demo file):"),
                        list("A&P XLSX file (.xlsx)" = "XLSX",
                             "DM model file (.RData)" = "RData",
                             "Demo A & P file" = "Demo"),
                        selected="XLSX"
                        ),
            conditionalPanel(
              "input.type == 'XLSX'",
              consecFileUploadInput("xlsxfile","Choose xlsx file"),
              br()
            ),
            conditionalPanel(
              "input.type == 'RData'",
              consecFileUploadInput("rdfile", "Upload RData file"),
              br()
            ),
            conditionalPanel(
              paste("input.type == 'AP' || input.type == 'XLSX' ",
                    "|| input.type == 'Demo'",sep=''), 
              selectInput("srFunc", "SR Function:",
                          list("Ricker" = "ricker", 
                               "Beverton-Holt" = "bevertonHolt", 
                               "Hockey Stick" = "hockeyStick")),
              selectInput("covariates",
                          "Include Marine Survival and Stream Flow covariates?",
                          list("No" = "no", 
                               "Yes" = "yes")), 
              selectInput("analysisType", "Analysis Type:",
                          list("DM" = "DM", 
                               "SS" = "SS")))
          ),
          mainPanel(
            width=7,
            tabsetPanel(
              id="output",
              tabPanel("Info", uiOutput("info")),
              tabPanel(HTML("<center>DM<br/>A&P Data</center>"),
                       div(plotOutput("plot1")),
                       br(), uiOutput("caption1")),
              tabPanel(HTML("<center>DM<br/>Posteriors</center>"), 
                       div(plotOutput("plot2"), width="90%"), br(),
                       uiOutput("caption2")),
              tabPanel("Downloads", uiOutput("downloadtab")),
              tabPanel("Priors",
                       source("inputpriors.R")),
              tabPanel("Help", includeHTML("html/dmmain_help.html"),
                       value="dmmainhelp"),
              selected="dmmainhelp"
            )
          )
        )
      ),
      tabPanel(
        title="VRAP",
        value="vraptab",
        sidebarLayout(
          sidebarPanel(
            div(uiOutput("vraprunbutton")),
            tags$br(),
            div(uiOutput("vraptypeselect")),
            conditionalPanel(
              "input.vraptype == 'RAV'",
              consecFileUploadInput("ravfile","Upload RAV file (.rav)"),
              br()
            ),
            conditionalPanel(
              "input.vraptype == 'demo'",
              selectInput("vrapdemofile", "Select demo file:", EXAMPLES)
            ),
            conditionalPanel(
              paste("input.vraptype=='RAV' |",
                    "input.vraptype=='demo' |",
                    "input.vraptype=='currentRData'"),
              radioButtons(
                'vrapnumruns',
                'Number of runs per simulation',
                c('1'=1, '10'=10, '100'=100, '1000'=1000,
                  'Use NRuns from RAV file' = -1),
                selected=-1)
            ),
            conditionalPanel(
              "input.vraptype == 'demo'",
              selectInput( "vrapdemodownload", "Download demo RAV files:",
                          EXAMPLES),
              downloadButton('downloadDemo', 'Download Demo File')
              )
          ),
          mainPanel(
            tabsetPanel(
              id="vrapoutput",
              tabPanel(HTML("<center>VRAP<br/>Summary</center>"),
                       uiOutput("VRAPsum")),
              tabPanel(HTML("<center>VRAP<br/>Brood Year</center>"),
                       uiOutput("VRAPbyr")),
              tabPanel(HTML("<center>VRAP<br/>Escapement</center>"),
                       uiOutput("VRAPesc")),
              tabPanel("Downloads", uiOutput("vrapdownloadtab")),
              tabPanel("Help",
                       includeHTML("html/vrapmain_help.html"))
            )
          )
        )
      )
    ),

    appFrameFooterFixed(displayAppsURL="../..")
  )
)

