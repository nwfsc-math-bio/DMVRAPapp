.libPaths(c("/usr/lib64/R/shiny_library",.libPaths()))
require(DM)
require(shiny) # 0.14.2
require(openxlsx)
require(VRAP)

options(shiny.maxRequestSize=20*1024^2)

if (file.exists("parcores.R")) {
  source("parcores.R")
} else {
  parcores <- function() { 1 }
}

ONSERVER <- serverInfo()$shinyServer
## ONSERVER <- TRUE
## ONSERVER <- FALSE

XLSXSHEET <- "DynamicsInput"
BASEOUTFILE <- "DMresults"
VRAPBASENAME <- "VRAPresults"

DEMOFILESPATH <- "demofiles"

DMNUMRUNS <- 1000

DUMMYMSG <- "reset"

PROCESSINGMSG <- paste("Calculating dynamic model - please wait.",
                       "</br>",
                       "<em>Close browser window to halt processing</em>")
VRAPPROCESSINGMSG <- paste("Running VRAP - please wait.",
                           "</br>",
                           "<em>Close browser window to halt processing</em>")
UPLOADERRORMSG <- paste("<br/><br/><br/><b>",
                        "An error occurred during the file upload.",
                        "Please try again.</br>",
                        "If the error persists,",
                        "contact a system administrator.</b>")
VIRUSDETECTIONMSG <- paste("<br/><br/><br/><b>The uploaded file has triggered",
                           "a virus detection.</br>",
                           "Please contact a",
                           "system administrator.</b>")
WAITSCANNINGMSG <- paste0("<div style='min-height:40px'> </div>",
                          "Scanning uploaded file - please wait.")
XLSXERRORMSG <- paste("<b>A DynamicsInput sheet could not be extracted<br/>",
                      "from the uploaded file.</b><br/><br/>",
                      "Please verify the file format.<br/>")
XLSXVERSIONMSG <- paste("<b>The DynamicsInput sheet of the selected XLSX file",
                        "has too few columns.<br/>",
                        "This file may be an older version input.",
                        "Please read the \"Input versions\" portion of the<br/>",
                        "help file for more details.<br/>")
HIDEWAITMSGCLASS <- "notbusy"
SHOWWAITMSGCLASS <- "busy"

VRAPTABLOADCOUNTER <- 0
VRAPTABLOADLIMIT <- 3
VRAPPREVTYPE <- NULL

NOMINALXLSXCOLS = 42
EMPTYXLSXCOL = 30

## HIDEUSERMSGCLASS <- "nomsgdisplay"
## SHOWUSERMSGCLASS <- "msgdisplay"

SCANCMD <- "clamdscan --fdpass --remove"
## SCANCMD <- "~/bin/ec3"
## SCANCMD <- "sleep 2;false"
## SCANCMD <- "true"


##################################################################
## global utility functions
##################################################################

## Workaround for skipEmptyCols bug in openxlsx 4.0.0

readXLSX <- function(xlsxfile, nominalcols, emptycol=30, sheet="DynamicsInput") {
  df <- read.xlsx(xlsxfile, sheet=sheet, colNames=FALSE)
  if (ncol(df) > emptycol) {
    if (ncol(df) < nominalcols) {
      df <- cbind(df[1:emptycol-1],rep(0,nrow(df)),df[emptycol:ncol(df)])
      colnames(df) <- paste0('X',1:ncol(df))
    }
  }
  df
}

## Sanity check for DM input xlsx/csv files

checkDMFile <- function(file="~/Desktop/A&P03LoSkagitV68.csv",
                        filetype,
                        covariates="yes") {

  errors <- list()
  
  thedf <- NULL
  
  if ("xlsx" == filetype) {
    thedf <- readXLSX(file, nominalcols=NOMINALXLSXCOLS,
                      emptycol=EMPTYXLSXCOL)
  } else if ("csv" == filetype) {
    thedf <- read.csv(file, stringsAsFactors=FALSE, header=FALSE)
  } else {
    stop(paste("Unknown file type : ", file_ext(file)))
  }

  
  ## check begin, end years for validity
  
  firstYear <- as.integer(thedf[4,3])
  lastYear <- as.integer(thedf[5,3])

  if ((0 == length(firstYear)) ||
        is.null(firstYear) ||
        is.na(firstYear)) {
    errors$firstYear <- paste("not valid :", firstYear)
  }

  if ((0 == length(lastYear)) ||
        is.null(lastYear) ||
        is.na(lastYear)) {
    errors$lastYear <- paste("not valid :", lastYear)
  }

  if (0 < length(errors)) { return(errors) }

  
  ## check for lastYear > firstYear
  
  if (lastYear <= firstYear) {
    errors$yearRange <- paste0("inconsistent firstYear, lastYear: ",
                               firstYear,", ",lastYear)
    return(errors)
  }

  tDat <- thedf[which(thedf[,1]=="Source->")+1:dim(thedf)[1],]
  tDat=tDat[,1:29]
  tDat=tDat[!is.na(tDat[,1]),]
  tDat=tDat[!(tDat[,1]==""),]
  tDat[tDat==""]=NA
  for(i in 1:dim(tDat)[2]){
    tDat[,i]=str_replace_all(tDat[,i],",","")
    tDat[,i]=as.numeric(tDat[,i])
  }

  ## check for firstYear, lastYear in broodyear values
  broodyear <- tDat[,1]
  fyIndex <- which(broodyear == firstYear)
  if (0 == length(fyIndex)) {
    errors$broodyear = paste("doesn't contain firstYear:", firstYear)
  }
  
  lyIndex <- which(broodyear == lastYear)
  if (0 == length(lyIndex)) {
    errors$broodyear = paste("doesn't contain lastYear", lastYear)
  }
  
  if (0 < length(errors)) { return(errors) }


  ## check that columns contain firstYear, lastYear values

  checkedCols <- c(matureFishingRate=list(15:17),
                   maturationRate=list(19:22),
                   mixedMaturityFishingRate=list(9:12),
                   preSpawnMortRate=list(24:27),
                   totalSpawnersAge3to5=2)

  if ("yes" == covariates) {
    checkedCols <- c(checkedCols,
                     list(flow=7,
                          marineSurvivalIndex=5))
  }
  
  for (tag in names(checkedCols)) {
    data <- tDat[,checkedCols[[tag]]]
    er <- colRangeCheck(data, firstYear, lastYear, fyIndex, lyIndex)
    if (0 < length(er)) { errors[[tag]] <- er }
  }
  
  errors
}

## index column checker for checkDMFile()

colRangeCheck <- function(dat, firstYear, lastYear,
                          fyIndex, lyIndex) {
  d <- dim(dat)
  multi <- ((1 < length(d)) && 1 < d[2])
  
  er <- vector("list")
  nasFound <- ifelse(multi,
                     any(is.na(dat[fyIndex,])),
                     is.na(dat[fyIndex]))

  if (nasFound) {
    if (multi) {
      er <- list(
        paste0("firstYear (", firstYear, ") index missing ",
               "in at least one column."))
    } else {
      er <- list(
        paste0("firstYear (", firstYear, ") index missing "))
    }
  }
  
  nasFound <- ifelse(multi,
                     any(is.na(dat[lyIndex,])),
                     is.na(dat[lyIndex]))
  if (nasFound) {
    if (multi) {
      er <- c(er,
              paste0("lastYear (", lastYear, ") index missing ",
                     "in at least one column."))
    } else {
      er <- c(er,
              paste0("lastYear (", lastYear, ") index missing "))
    }
  }
  

  return(er)
}

## HTML decoration for checkDMFile() error message

formatDMFileErrorMsg <- function(filename, er, covmsg=FALSE) {
  msg <- paste0(
    "<center>",
    "<div style='text-align:left;width:40%'>",
    "<center><h4 style='font-weight:bold'>Input file problems</h4>",
    "<br/>")
  if (covmsg) {
    if (is.null(filename)) {
      msg <- paste0(msg,
                    "<b>Issues which prevent using covariates:</b>",
                    "<br/><br/>")
    } else {
      msg <- paste0(msg,
                    "<b>Problems with input file<br/>", filename,
                    "<br/>which prevent using covariates:</b>",
                    "<br/><br/>")
    }
  }
  for (name in names(er)) {
    msg <- paste0(msg, name, " :<br/>")
    for (i in 1:length(er[[name]])) {
      msg <- paste(msg, "&nbsp;&nbsp;", er[[name]][[i]],"<br/>")
    }
    msg <- paste0(msg, "<br/>")
  }
  msg <- paste0(msg,"<br/><br/></center></div></center>")

  return(msg)
}

## appends timestamp to filename input string

appendTimestamp <- function(filename) {
  paste0(filename,"_",strftime(Sys.time(),"%Y%m%d_%H%M%S"))
}


##################################################################
## VRAP output display utils
##################################################################

## Read file, collapse lines into single string

getoutputfile <- function(filepath) {
  singlestring <- ""

  if (file.exists(filepath)) {
    thefile <- file(filepath,"r")
    lines <- readLines(thefile)
    close(thefile)
    singlestring <- paste0(lines,collapse="\n")
  }

  return(singlestring)
}

## Simple reformatting of output file to HTML for display in Shiny interface

htmlize <- function(blob, tagid="outputtext", padding=0) {
  thehtml <- paste(blob, collapse="")
  thehtml <- str_replace_all(thehtml,"\r",'')
  thehtml <- str_replace_all(thehtml,"<","&lt;")
  thehtml <- str_replace_all(thehtml,">","&gt;")
  thehtml <- str_replace_all(thehtml,"\n","<br/>")
  thehtml <- str_replace_all(thehtml,"[[:space:]]", "&nbsp;")
  for (i in 1:padding) {thehtml <- paste0(thehtml,"<br/>")}
  thehtml <- paste(c("<div id='",tagid,"' class='tabtext'>",
                     thehtml,"</font></div>"), collapse="")
  return(thehtml)
  
}


##################################################################
## Shiny sever
##################################################################

shinyServer( function(input, output, session) {

  ## number of (logical) cores available
  lcores <- parcores()

  ## switches serve as collection point for notification of output file
  ## creation and deletion, among other things
  
  switches <- reactiveValues(
    outputs=TRUE,
    vrapoutputs=TRUE,
    resetravupload=FALSE,    ## clear notification for rav uploader
    resetxlsxupload=FALSE,   ## clear notification for xlsx uploader
    resetrdupload=FALSE      ## clear notification for Rdata uploader
  )

  ## Attempt to detect user platform for download format default
  
  if (("request" %in% names(session)) &&
        ("HTTP_USER_AGENT" %in% names(session$request))) {
    osIsWindows <- grepl("windows", session$request$HTTP_USER_AGENT,
                         ignore.case=TRUE)
  } else {
    osIsWindows <- TRUE
  }

  ##################################################################
  ## Integration of DM, VRAP calculations
  ##################################################################

  ## flag and accessors
  
  VRAPRUNFROMDM <- FALSE

  setRunFromDM <- function(runFromDM=FALSE) {
    VRAPRUNFROMDM <<- runFromDM
    ## if (!is.null(input$vrapnumruns)) {
    ##   updateRadioButtons(session, "vrapnumrums", selected="Use NRuns from RAV file")
    ## }
  }

  runFromDM <- function() {
    VRAPRUNFROMDM
  }

  ## input$vraptype wrapper which also clears run-from-DM flag
  
  currentVRAPType <- reactive({
    if (VRAPRUNFROMDM && input$vraptype != "currentRData") {
      setRunFromDM(FALSE)
    }
    input$vraptype
  })
  
  ##################################################################
  ## output file and directory utilities
  ##################################################################

  ##################################################################
  ## DM output files and directories
  ##################################################################


  ## get the DM output directory, creating a new one if necessary
  theOutputDir <- NULL
  
  getOutputDir <- function() {
    if (is.null(theOutputDir)) {
      theOutputDir <<- tempfile(pattern="DMout")
      dir.create(theOutputDir)
    }

    return(theOutputDir)
  }

  ## DM output file basename: includes a timestamp
  
  outputBaseName <- NULL

  getOutputBaseName <- function(delete=FALSE) {
    if (delete) {
      outputBaseName <<- NULL
    } else {
      if (is.null(outputBaseName)) {
        outputBaseName <<- appendTimestamp(BASEOUTFILE);
      }
    }
    return( outputBaseName );
  }

  ## complete path to DM output file, less extension
  
  outputPathBase <- function() {
    file.path(getOutputDir(), getOutputBaseName())
  }

  ## complete path to DM outputfile, including specified extension
  
  outputPath <- function(ext) {
    paste0(outputPathBase(), ext)
  }

  ## remove DM output directory and contents
  
  clearOutputDirectory <- function() {
    getOutputBaseName(delete=TRUE)
    unlink(file.path(getOutputDir(),"*"), recursive=TRUE)
  }

  ## remove contents of DM output directory (directory remains)
  
  clearDMOutputs <- function() {
    outdir <- getOutputDir()
    baseName <- getOutputBaseName()
    files <- Sys.glob(c(outputPath("*")))
    lapply(files, function(x) {file.remove(x)})

    getOutputBaseName(delete=TRUE)
  }

  ##################################################################
  ## VRAP output files and directories
  ##################################################################

  ## get the VRAP output directory, creating a new one if necessary
  vrapOutputDir <- NULL

  getVRAPOutputDir <- function() {
    if (is.null(vrapOutputDir)) {
      vrapOutputDir <<- tempfile(pattern="VRAPout")
      dir.create(vrapOutputDir)
    }
    return(vrapOutputDir)
  }

  ## VRAP output file basename : includes a timestamp
  
  vrapBaseName <- NULL

  getVRAPBaseName <- function(delete=FALSE) {
    if (delete) {
      vrapBaseName <<- NULL
    } else if (is.null(vrapBaseName)) {
      vrapBaseName <<- appendTimestamp(VRAPBASENAME);
    }
    return( vrapBaseName );
  }

  ## complete path to VRAP output file, less extension
  
  vrapOutputPathBase <- function() {
    file.path(getVRAPOutputDir(), getVRAPBaseName())
  }

  ## complete path to VRAP output file, including specified extension
  
  vrapOutputPath <- function(ext) {
    paste0(vrapOutputPathBase(), ext)
  }

  ## remove VRAP output directory and contents

  clearVRAPOutputDirectory <- function() {
    getVRAPBaseName(delete=TRUE)
    unlink(file.path(getVRAPOutputDir(),"*"), recursive=TRUE)
  }

  ## remove VRAP output directory contents (directory remains)
  
  clearVRAPOutputs <- function() {
    vrapoutdir <- getVRAPOutputDir()
    vrapBase <- getVRAPBaseName()
    files <- Sys.glob(c(vrapOutputPath(".*")))
    lapply(files, function(x) {file.remove(x)})

    getVRAPBaseName(delete=TRUE)
  }

  ## remove contents of DM and VRAP output directories (directories remain)
  
  clearOutputs <- function() {
    clearDMOutputs()
    clearVRAPOutputs()
  }

  ## modifies VRAP summary file generated by VRAP package:
  ## - remove "copy of input file" line
  ## - inserts input file name (=DM output if appropriate)
  ## - inserts DM input file name (if appropriate)
  
  vrapFilenameFilter <- function(DMoutput=FALSE) {
    baseName <- getVRAPBaseName()
    filepath <- vrapOutputPath(".sum")
    if (file.exists(filepath)) {
      thefile <- file(filepath,"r")
      lines <- readLines(thefile)
      close(thefile)
      if (DMoutput || input$vraptype == "currentRData") {
        uploadname <- inputFileName()
        lines <- sub("^ *Copy of Input File.*$",
                     paste0("DM Input File: ",uploadname),
                     lines,perl=TRUE)
        vrapInput <- paste0(getOutputBaseName(),".rav")
        lines <- sub("^( *Input File:).*$",paste0("\\1","    ",vrapInput),
                     lines,perl=TRUE)
      } else if(input$vraptype == "RAV") {
        vrapupload <- inputVRAPFileName()
        lines <- sub("^( *Copy of Input File.*)$",
                     "",
                     lines,perl=TRUE)
        lines <- sub("( *Input File:).*$",paste0("\\1","   ",vrapupload),
                     lines,perl=TRUE)
      } else if(input$vraptype == "demo") {
        thedemofile <- inputVRAPFileName()
        friendly_demofile <- paste0("DEMO : ",
                                    names(EXAMPLES)[grepl(thedemofile,EXAMPLES)])
        lines <- sub("( *Input File:).*$",
                     paste0("\\1","   ",friendly_demofile),
                     lines,perl=TRUE)
        lines <- lines[!grepl("^( *Copy of Input File.*)$",lines)]
      }
      outfile <- file(filepath,"w")
      cat(lines, file=outfile, sep="\n")
      close(outfile)
    }
    
  }

  ##################################################################
  ## output directory removal on session-end event
  ##################################################################
  
  session$onSessionEnded(function() {
    unlink(getOutputDir(), recursive=TRUE)
    unlink(getVRAPOutputDir(), recursive=TRUE)
  })

  ##################################################################
  ## dummy elements to aid in triggering of wait displays
  ##################################################################
  
  triggerShinyBusy2 <- reactive({
    input$DMButton
    input$VRAPButton
    return("")
  })

  output$dummy2 <- renderText({
    return(triggerShinyBusy2())
  })

  ##################################################################
  ## wait and general user message displaying and hiding
  ##################################################################

  setWaitMsg <- function(msg) {
    session$sendCustomMessage('setwaitmsg', msg)
  }
  
  showWaitMsg <- function(showprogress=TRUE) {
    if (showprogress) {
      session$sendCustomMessage("setprogressdisplay", "inline")
    } else {
      session$sendCustomMessage("setprogressdisplay", "none")
    }
    session$sendCustomMessage("setmsgclass", SHOWWAITMSGCLASS)
  }

  hideWaitMsg <- function() {
    session$sendCustomMessage("setmsgclass", HIDEWAITMSGCLASS)
    session$sendCustomMessage("setprogressdisplay", "show")
  }
  
  showUserMsg <- function(msg) {
    session$sendCustomMessage("setusermsg", msg)
  }

  hideUserMsg <- function() {
    session$sendCustomMessage("clearusermsg", DUMMYMSG)
  }
  
  ## reactive hides user message when close button clicked
  
  observe({
    input$msgclosebtn
    hideUserMsg()
  })

  ##################################################################
  ## AV scanner
  ##################################################################

  ## invoke ClamAV
  
  scanclam <- function(path) {
    if (is.null(path)) { return("SCANNOFILE") }

    returncode <- "SCANVIRUS"
    
    cmd <- paste(SCANCMD, path)
    ec <- system(cmd)
    if (0 == ec) { return("SCANOKAY") }
    else if (1 == ec) {
      if (file.exists(path)) {
        file.remove(path)
      }
    }
    else { returncode <- "SCANERROR" }

    return(returncode)
  }

  ## wrap ClamAV call, format results
  
  scanfile <- function(filepath) {
    if( !ONSERVER ) return(filepath)
    setWaitMsg(WAITSCANNINGMSG)
    showWaitMsg(FALSE)
    scanresponse <- scanclam(filepath)
    hideWaitMsg()
    errmsg <- NULL
    if ("SCANVIRUS" == scanresponse) {
      errmsg <- VIRUSDETECTIONMSG
    } else if ("SCANERROR" == scanresponse) {
      errmsg <- UPLOADERRORMSG
    }

    if (is.null(errmsg)) {
      return (filepath)
    } else {
      showUserMsg(errmsg)
      return (NULL)
    }
  }


  ##################################################################
  ## Reactive wrappers for input parameters
  ##################################################################

  ## VRAP number of runs (always set to file value for current DM output)
  
  getNumRuns <- reactive({
    if (is.null(input$vrapnumruns) && runFromDM()) {return(-1)}
    if (is.null(input$vraptype)) { return(-1) }
    return(as.integer(input$vrapnumruns))
  })

  ## DM input file name (type dependent)
  
  inputFileName <- reactive({
    filename <- NULL
    if (input$type=="XLSX") {
      filename <- xlsxFile()$name
    } else {
      filename <- "A & P Demo"
    }
    return(filename)
  })

  ## VRAP input file name (type dependent)
  
  inputVRAPFileName <- reactive({
    filename <- NULL
    if (input$vraptype=="RAV") {
      filename <- ravFile()$name
    } else if (input$vraptype=="demo") {
      filename <- input$vrapdemofile
    } else {
      filename <- input$rdfile$name
    }
    return(filename)
  })

  ## reactive check on DM input file:
  ## - if covariates are used, check input file for consistency
  
  observe({
    input$covariates
    isolate({
      if (input$type == "XLSX") {
        currentfile <- inputfilexlsx()[1]
      } else {
        currentfile <- NULL
      }
      if (!is.null(currentfile) && "yes" == input$covariates) {
        ## Note that xlsx input file has been extracted to temp csv
        er <- checkDMFile(currentfile, "csv", input$covariates)
        if (0 < length(er)) {
          msg <- formatDMFileErrorMsg(inputFileName(), er, TRUE)
          showUserMsg(msg)
          updateSelectInput(session, "covariates", selected="no")
        }
      }
    })
  })

  
  ##################################################################
  ## reactive display component renderers
  ##################################################################


  ##################################################################
  ## reactive output display renderers
  ##################################################################

  ## VRAP summary display
  
  output$VRAPsum <- renderUI({
    switches$vrapoutputs
    if (vrapOutputAvailable()) {
      vrapBase <- getVRAPBaseName()
      return(HTML(
        htmlize(getoutputfile(vrapOutputPath(".sum")), "tabsumtext",2)))
    } else {
      return(includeHTML("html/vrapsum_help.html"))
    }
  })

  ## VRAP bycatch rate display
  
  output$VRAPbyr <- renderUI({
    switches$vrapoutputs
    if (vrapOutputAvailable()) {
      vrapBase <- getVRAPBaseName()
      return(HTML(
        htmlize(paste0(getoutputfile("html/byr_colheaders.txt"),
                       getoutputfile(vrapOutputPath(".byr"))),
                "byrtabtext",2)))
    } else {
      return(includeHTML("html/vrapbyr_help.html"))
    }
  })

  ## VRAP escapement display
  
  output$VRAPesc <- renderUI({
    switches$vrapoutputs
    if (vrapOutputAvailable()) {
      vrapBase <- getVRAPBaseName()
      return(HTML(
        htmlize(paste0(getoutputfile("html/esc_colheaders.txt"),
                       getoutputfile(vrapOutputPath(".esc"))),
                "esctabtext",2)))
    } else {
      return(includeHTML("html/vrapesc_help.html"))
    }
  })

  ## reactive wrapper for DM inputs: srFunc, covariates, output path
  
  inputdata <- reactive({
    return(c(input$srFunc, input$covariates, input$analysisType, outputPathBase()))
  })

  ##################################################################
  ## reactive renderers of file input controls
  ## - non-reactive contents
  ## - reactive wrappers
  ##################################################################

  cancelRavUpload <- reactive({
    tmp <- switches$resetravupload
    isolate({switches$resetravupload <- FALSE})
    tmp
  })
  
  ravFile <- callModule(consecFileUpload, "ravfile", clear=cancelRavUpload,
                        currentFileLabel="Current file",
                        accept=c(".rav",".RAV", ".Rav"),
                        buttonLabel="Upload RAV file (.rav):")

  observe({
    input$vraptype
    switches$resetravupload <- TRUE
  })
  
  cancelRdUpload <- reactive({
    tmp <- switches$resetrdupload
    isolate(switches$resetrdupload <- FALSE)
    tmp
  })

  rdFile <- callModule(consecFileUpload, "rdfile", clear=reactive(FALSE),
                       currentFileLabel="Current file:",
                       accept=c(".RData"),
                       buttonLabel="Upload RData file")

  observe({
    input$type
    isolate(switches$resetrdupload <- TRUE)
  })
  
  cancelXlsxUpload <- reactive({
    tmp <- switches$resetxlsxupload
    isolate({ switches$resetxlsxupload <- FALSE })
    tmp
  })

  xlsxFile <- callModule(consecFileUpload, "xlsxfile", clear=cancelXlsxUpload,
                         currentFileLabel="Current file:",
                         accept=paste0("application/vnd.openxmlformats-",
                                       "officedocument.spreadsheetml.sheet"),
                         buttonLabel="Upload xlsx file")
  observe({
    input$type
    switches$resetxlsxupload <- TRUE
  })
  
  output$runbutton <- renderUI({
    if (inputAvailable()) {
      if (outputAvailable()) {
        list(
          actionButton(inputId="DMButton", label="Run Dynamic Model",
                       title="Click to run DM on selected input")
        )
      } else {
        list(
          actionButton(inputId="DMButton", label="Run Dynamic Model",
                       title="Click to run DM on selected input")
        )
      }
    } else {
      list(
        actionButton(inputId="DMButton", label="Run Dynamic Model",
                     disabled="",
                     title="Select input type and file below")
      )
    }
  })

  ## reactive renderer for VRAP run button
  ## - if no input available, button is present but disabled

  output$vraprunbutton <- renderUI({
    if (vrapInputAvailable()) {
      list(
        actionButton(inputId="VRAPButton",
                     label="Run VRAP"))
    } else {
      list(
        actionButton(inputId="VRAPButton",
                     label="Run VRAP", disabled="",
                     title="Select input type and file below"))
    }
  })

  ## reactive renderer for VRAP input file type selector
  ## - include DM output option if DM output exists
  ## - if run-from-DM flag is set, select DM output
  
  output$vraptypeselect <- renderUI({
    if (outputRAVAvailable()) {
      selectthis <- "currentRData"
      list(
        selectInput("vraptype",
                    paste("Select an input type",
                          "(RAV, DM RData, or demo):"),
                    list("Upload RAV file (.rav)" = "RAV", 
                         "Use current DM output" = "currentRData",
                         "Demo file" = "demo"),
                    selected=selectthis
                    )
      )
    } else {
      list(
        selectInput("vraptype",
                    paste("Select an input type",
                          "(RAV or demo):"),
                    list("RAV file (.rav)" = "RAV",
                         "Demo file" = "demo") 
                    )
      )
    }
  })

  ## reactive switch to DM Info tab when run button is clicked
  
  observe({
    if (!is.null(input$DMButton) && input$DMButton > 0) {
      updateTabsetPanel(session, "output", "Info")
    } else {
      updateTabsetPanel(session, "output", "Help")
    }
  })

  ##################################################################
  ## reactive monitors for presence of input files
  ##################################################################

  ## reactive flag for DM input available
  
  inputAvailable <- reactive({
    switch(input$type,
           XLSX = {return(!is.null(inputfilexlsx()))},
           RData = {return(!is.null(inputfilerd()))},
           Demo = {return(TRUE)})
  })

  ## reactive flag for VRAP input available

  vrapInputAvailable <- reactive({
    if (is.null(input$vraptype)) {
      return(FALSE)
    } else if (input$vraptype %in% c("RAV", "currentRData")) {
      return(!is.null(vrapinputfile()))
    } else if (runFromDM()) {
      return(outputAvailable())
    } else {
      return(input$vraptype=="demo")
    }
  })
  

  ##################################################################
  ## reactives to clear all outputs when input parameters change
  ##################################################################

  ## clear DM outputs, remove plots and captions from output tabs
  ## if any DM input configuration item changes

  observe({
    ## triggers
    input$type
    rdFile()
    xlsxFile()
    input$srFunc
    input$covariates
    input$analysisType
    
    ## actions
    clearDMOutputs()

    output$plot1 <- renderPlot({ NULL })
    output$caption1 <- renderUI({ "" })
    output$plot2 <- renderPlot({ NULL })
    output$caption2 <- renderUI({ "" })
    output$info <- renderUI(includeHTML("html/defaultinfo.html"))

    isolate({
      switches$outputs <- !switches$outputs
    })
  })

  ## clear VRAP outputs, remove plots and captions from output tabs
  ## if any VRAP input configuration item changes

  observe({
    ## triggers
    currentVRAPType()

    if (runFromDM()) { return() } # don't clear output on
                                  # initial VRAP tab display
    ravFile()

    ## actions
    
    clearVRAPOutputs()

    isolate({
      switches$vrapoutputs <- !switches$vrapoutputs
    })
  })

  ##################################################################
  ## reactive flags indicating whether output files exist or not
  ##################################################################

  outputAvailable <- reactive({
    switches$outputs
    return(file.exists(outputPath(".rav")))
  })
  
  outputRAVAvailable <- reactive({
    switches$outputs
    return(file.exists(outputPath(".rav")))
  })
  
  outputReportAvailable <- reactive({
    switches$outputs
    return(file.exists(outputPath(".pdf")))
  })
  
  outputPlotsAvailable <- reactive({
    switches$outputs
    return(file.exists(outputPath("_plots.zip")))
  })
  
  vrapOutputAvailable <- reactive({
    switches$vrapoutputs
    return(file.exists(vrapOutputPath(".sum")))
  })

  
  ##################################################################
  ## Download control renderers
  ## - default format selection based on OS detection
  ##################################################################

  ## DM download renderer
  
  output$downloadtab <- renderUI({
    a <- vector("list")
    if (outputAvailable()) {
      a <- list(
        tags$h4('Download DM Results',style="align:center;"),
        if (outputReportAvailable()) {
          downloadButton('downloadDMReport', HTML('Report</br>(pdf)'))
        } else { NULL },
        if (outputRAVAvailable()) {
          downloadButton('downloadDMRav', HTML('Rav file</br>(rav)'))
        } else { NULL },
        if (outputPlotsAvailable()) {
          downloadButton('downloadPlotsZip', HTML('Plots<br/>(zipped)'))
        } else {NULL},
        downloadButton('downloadDMRData', HTML('Posteriors</br>(RData)')))
    } else {
      a <- includeHTML("html/dmdownload_help.html")
    }

    return(a)
  })
  
  ## VRAP download renderer
  
  output$vrapdownloadtab <- renderUI({
    a <- list()
    if (vrapOutputAvailable()) {
      sel <- "win"
      if (!is.null(osIsWindows) && !osIsWindows) { sel="unix" }
      a <- list(
        a,
        tags$hr(),
        tags$h4('Download VRAP Results',style="align:center;"),
        selectInput("os", "Choose OS:",
                    list("Windows" = "win", 
                         "Mac/Unix" = "unix"),
                    selected=sel),
        downloadButton('downloadVRAPsum', HTML('Summary<br/>(sum)')),
        downloadButton('downloadVRAPbyr', HTML('Brood Year<br/>(byr)')),
        downloadButton('downloadVRAPesc', HTML('Escapement<br/>(esc)')))
    } else {
      a <- includeHTML("html/vrapdownload_help.html")
    }

    return(a)
  })

  ## create a dependence on input file detectors so they will update
  
  observe({
    vrapinputfile()
    inputfilerd()
    inputfilexlsx()
  })

  ##################################################################
  ## reactives for handling, naming, sanity-checking,
  ## and formatting input files
  ##################################################################

  ## DM XLSX input files
  
  inputfilexlsx <- reactive({
    inFile <- xlsxFile()
    if (is.null(inFile)) return(NULL)
    if (is.null(scanfile(inFile$datapath))) {
      isolate(switches$resetxlsxupload <- TRUE)
      return(NULL)
    } else {
      outfile = getOutputBaseName()
      xlsxerror <- try(
        df.xlsx <- readXLSX(inFile$datapath, nominalcols=NOMINALXLSXCOLS,
                          emptycol=EMPTYXLSXCOL),
        silent=TRUE
      )
      if (inherits(xlsxerror, "try-error")) {
        if (file.exists(inFile$datapath)) file.remove(inFile$datapath)
        isolate(switches$resetxlsxupload <- TRUE)
        showUserMsg(XLSXERRORMSG)
        return(NULL)
      } else if (ncol(df.xlsx) <= EMPTYXLSXCOL) {
        if (file.exists(inFile$datapath)) file.remove(inFile$datapath)
        isolate(switches$resetxlsxupload <- TRUE)
        showUserMsg(XLSXVERSIONMSG)
        return(NULL)
      } else {
        isolate({
          er <- checkDMFile(inFile$datapath, "xlsx", input$covariates)
          if (0 < length(er)) {
            msg <- formatDMFileErrorMsg(inFile$name, er)
            showUserMsg(msg)
            if (file.exists(inFile$datapath)) {
              file.remove(inFile$datapath)
              isolate(switches$resetxlsxupload <- TRUE)
            }
            return(NULL)
          } else {
            existingcsvs <- Sys.glob(file.path(tempdir(),"DMCSV*.csv"))
            file.remove(existingcsvs)
            csvfile <- tempfile("DMCSV",fileext=".csv")
            write.table(df.xlsx, file=csvfile, sep=",",
                        row.names=FALSE, col.names=FALSE)
            if (file.exists(inFile$datapath)) {
              file.remove(inFile$datapath)
            }
          }
        })
        return(c(csvfile, outfile))
      }
    }
  })

  ## VRAP input files
  
  vrapinputfile <- reactive({
    input$DMButton
    input$vrapdemofile
    if (is.null(input$vraptype)) { return(NULL) }
    if (input$vraptype == "RAV") {
      inFile <- ravFile()
      if (is.null(inFile)) return(NULL)
      if (is.null(scanfile(inFile$datapath))) {
        isolate({switches$resetravupload <- TRUE})
        return(NULL)
      } else {
        return(inFile$datapath)
      }
    } else if (input$vraptype == "currentRData") {
      return(outputPath(".rav"))
    } else if (input$vraptype == "demo") {
      if (is.null(input$vrapdemofile)) {
        return(NULL)
      } else {
        return(file.path(DEMOFILESPATH,input$vrapdemofile))
      }
      
    } else {
      return(NULL)
    }
  })
  
  ## DM RDdata input files
  
  inputfilerd <- reactive({
    inFile <- rdFile()  
    if (is.null(inFile)) return(NULL)
    if (is.null(scanfile(inFile$datapath))) {
      isolate(switches$resetrdupload <- TRUE)
      return(NULL)
    } else {
      outfile = getOutputBaseName()
      return(c(inFile$datapath, outfile))
    }
  })

  
  ##################################################################
  ## VRAP processing and triggers
  ##################################################################

  ## run VRAP
  
  VRAPprocessing <- function(infile=NULL, runOnDMResults=FALSE) {
    howmanyruns <- getNumRuns()
    isolate({
      setWaitMsg(VRAPPROCESSINGMSG)
      showWaitMsg()

      clearVRAPOutputs()

      fileinput <- ifelse(is.null(infile), vrapinputfile(), infile)
      output <- Main(fileinput, OutFileBase=vrapOutputPathBase(),
                     NRuns=howmanyruns, forceNewRav=FALSE, silent=TRUE,
                     lcores=lcores)

      vrapFilenameFilter(runOnDMResults)

      output$VRAPsum <- HTML(htmlize(getoutputfile(vrapOutputPath(".sum"))))
      output$VRAPbyr <- HTML(htmlize(
        paste0(getoutputfile("html/byr_colheaders.txt"),
               getoutputfile(vrapOutputPath(".byr")))))
      output$VRAPsum <- HTML(htmlize(
        paste0(getoutputfile("html/esc_colheaders.txt"),
               getoutputfile(vrapOutputPath(".esc")))))
      
      hideWaitMsg()

      isolate({switches$vrapoutputs <- !switches$vrapoutputs})
      return(0)
    })
  }

  ## VRAP-tab run button triggers VRAPprocessing
  
  observe({
    if (!is.null(input$VRAPButton)) {
      isolate({
        if (0 < input$VRAPButton) {
          VRAPprocessing()
          updateTabsetPanel(session, "vrapoutput",
                            selected="<center>VRAP<br/>Summary</center>")
        }
      })
    }
  })

  ## VRAP invocation from DM-tab (run-VRAP button or checkbox)

  runVRAPfromDM <- function() {
    VRAPprocessing(infile=outputPath(".rav"), runOnDMResults=TRUE)
    setRunFromDM(TRUE)
    updateSelectInput(session, "vraptype", selected="currentRData")
    updateTabsetPanel(session, "DM_VRAP_tabs", selected="vraptab")
    updateTabsetPanel(session, "vrapoutput",
                      selected="<center>VRAP<br/>Summary</center>")
  }

  ##################################################################
  ## DM processing and triggers
  ##################################################################

  ## run-DM button
  
  observe({
    if (!is.null(input$DMButton) && input$DMButton > 0) {
      setRunFromDM(FALSE)
      procout <- DMprocessing(TRUE)
      if (is.null(procout)) {
        output$info <- renderUI(includeHTML("html/defaultinfo.html"))
      } else {
        output$info <- renderUI(procout)
      }
    }
  })

  ## DM processing
  
  DMprocessing <- function(calculate=FALSE) {
    isolate({
      if (!calculate ||
            ## (input$type=="AP" && is.null(inputfilecsv())) ||
            (input$type=="XLSX" && is.null(inputfilexlsx())) ||
            (input$type=="RData" && is.null(inputfilerd()))
          ) {
        return(includeHTML("html/defaultinfo.html"))
      }

      setWaitMsg(PROCESSINGMSG)
      showWaitMsg()

      clearDMOutputs()

      ## suppress console messages
      
      if (ONSERVER) sink("/dev/null")
      pseudoSuppressMessages <- if (ONSERVER) {suppressMessages}
      else {function(blob) {blob}}
      pseudoSuppressMessages({
        ## change the working directory to induce the
        ## tools to create temp files where they belong
        owd <- setwd(getOutputDir())
        if (input$type == "XLSX" ||
              input$type == "Demo") {
          if (input$type == "XLSX") {
            a.and.p.file <- inputfilexlsx()[1]
          } else if (input$type == "Demo") {
            ## temporary package name change
            a.and.p.file <-
              file.path(path.package("DM"),"doc","DemoAandP-Stilliguamish.csv")
          }
          if (ONSERVER) options(warn=-1)
          readInput <- list(SRfunction=inputdata()[1],
                            covariates=inputdata()[2],
                            analysisType=inputdata()[3]
                            )
          if(readInput$covariates=="yes"){
            readInput$includeMarineSurvival <-"yes"
            readInput$includeFlow <-"yes"
          }
          tmp <- readData(a.and.p.file=a.and.p.file, input=readInput, silent=TRUE)
          if (ONSERVER) options(warn=0)
          dat <- tmp$dat
          
          ## zeros in broodYear distort the plots; replace them with NAs
          dat$broodYear[dat$broodYear==0] <- NA
          
          ap.input <- tmp$input

          population=tmp$input$population
          
          ## Get the priors list from the priors tab
          ## This is the priors arg for new createBUGSdata()
          
          inputPriors = createPriors(
            pMode=input$pMode, pSig=input$pSig, pMin=input$pRange[1], pMax=input$pRange[2],
            cMu=input$cMu, cSig=input$cSig, cMin=input$cRange[1], cMax=input$cRange[2],
            msMu=input$msMu, msSig=input$msSig, msMin=input$msRange[1], msMax=input$msRange[2],
            flowMu=input$flowMu, flowSig=input$flowSig, flowMin=input$flowRange[1], flowMax=input$flowRange[2])
            

          # logMu <- log(input$pMode) + input$pSig^2
          # pTau <- 1/(input$pSig^2)
          # cTau <- 1/(input$cSig^2)
          # msTau <- 1/(input$msSig^2)
          # flowTau <- 1/(input$flowSig^2)
          # inputPriors <- list(
          #   prodPrior = c(logMu=logMu, tau=pTau,
          #                 lowerBound=input$pRange[1],
          #                 upperBound=input$pRange[2]),
          #   logCapPrior = c(mu=input$cMu, tau=cTau,
          #                   lowerBound=input$cRange[1],
          #                   upperBound=input$cRange[2]),
          #   msCoefPrior = c(mu=input$msMu, tau=msTau,
          #                   lowerBound=input$msRange[1],
          #                   upperBound=input$msRange[2]),
          #   flowCoefPrior = c(mu=input$flowMu, tau=flowTau,
          #                     lowerBound=input$flowRange[1],
          #                     upperBound=input$flowRange[2])
          # )

          ## run BUGS and get the posteriors
          gperror <-
            try({
              tmp.dmObj <- runModel(a.and.p.file, input=ap.input,
                                    priors=inputPriors)
            })
          if (inherits(gperror, "try-error")) {
            hideWaitMsg()
            msg <- paste0("Error in DM::runModel\n", attr(gperror, "condition"))
            msg1 <- htmlize(msg,"posterrorupper")
            msg2 <- htmlize("", "posterrorlower")
            showUserMsg(paste0(msg1, msg2))
            modpath <- file.path(getOutputDir(), "mod1.txt")
            if(file.exists(modpath)) file.remove(modpath)
            setwd(owd)
            return(NULL)
          }
          
          mlEst = findOptimum(dat, ap.input)

          tDat = tmp.dmObj$tDat

          plist <- list()
          plist$jagsOut$BUGSoutput$sims.list <-
            tmp.dmObj$jagsOut$BUGSoutput$sims.list
          plist$otherDat$broodYear <- tmp.dmObj$otherDat$broodYear
          plist$otherDat$AEQR <- tmp.dmObj$otherDat$AEQR
          plist$priors <- tmp.dmObj$priors
          theinits <- tmp.dmObj$calcInits()
          plist$theinits <- theinits

          ## Write posteriors to .csv and RData file
          writeResultsToFile(ap.input, dat, tDat, mlEst, tmp.dmObj$bdat,
                             plist, filename=outputPathBase())
          filename=outputPath(".RData")
          outfilename=outputPathBase()
        } else if(input$type == "RData") {
          filename=inputfilerd()[1]
          outfilename=outputPathBase()
          file.copy(filename, outputPath(".RData"), overwrite=TRUE)
          tmp.dmObj <- fakeResult(outputPath(".RData"))
        }
        

        fig.cap1 = plotResults(tmp.dmObj, plotType="a.and.p.data", plotDest="none")
        output$plot1=renderPlot( plotResults(tmp.dmObj, plotType="a.and.p.data", plotDest="default") )

        output$caption1 <-
          renderUI(list(HTML(fig.cap1),includeHTML("html/captionfooter.html")))
        
        fig.cap2 = plotResults(tmp.dmObj, plotType="SR", plotDest="none")
        output$plot2=renderPlot( plotResults(tmp.dmObj, plotType="SR", plotDest="default") )

        output$caption2 <-
          renderUI(list(HTML(fig.cap2),includeHTML("html/captionfooter.html")))

        rendererror <- 
          try(
            Report1(dmObj.RData.file=filename, output.file=basename(outfilename),
                    rav.options=list(numRuns=DMNUMRUNS)),
            silent=TRUE)

        createRAVerror <- try(
          createRAVfile(tmp.dmObj$bdat, tmp.dmObj$input, tmp.dmObj$tDat,
                        tmp.dmObj$dat,
                        estType = "median",
                        filename=paste0(outfilename,".rav"),
                        rav.options=list(numRuns=DMNUMRUNS))
        )

        if (inherits(createRAVerror, "try-error")) {
          crerrmsg <- paste0("RAV creation failed with error:<br/><br/>",
                             createRAVerror[1],
                             "No RAV output generated.")
          showUserMsg(crerrmsg)
        }

        setwd(owd)

        ## clean-up
        badfiles <- Sys.glob(paste0(outfilename,'*'))
        badfiles <- badfiles[!(str_detect(badfiles,"[.]pdf") |
                                 str_detect(badfiles,"[.]html") |
                                 str_detect(badfiles,"[.]RData") |
                                 str_detect(badfiles,"[.]rav") |
                                 str_detect(badfiles,"[.]png") |
                                 str_detect(badfiles,"_plots.zip"))]
        file.remove(badfiles)
        modpath <- file.path(getOutputDir(), "mod1.txt")
        if(file.exists(modpath)) file.remove(modpath)

        modpath <- file.path(getOutputDir(), "Report1-knitr.Rmd")
        if(file.exists(modpath)) file.remove(modpath)

        if(file.exists("Rplots.pdf")) file.remove("Rplots.pdf")

        texfigsdir <- file.path(getOutputDir(),"figure")
        if (dir.exists(texfigsdir)) {
          unlink(texfigsdir, recursive=TRUE)
        }

        htmldetritus <- Sys.glob(file.path(tempdir(),'*.html'))
        file.remove(htmldetritus)
        
        hideWaitMsg()
      })  ## suppressMessages
      if( ONSERVER ) sink()
      isolate({switches$outputs = !switches$outputs})
    })
    return(HTML(DM:::info(filename)))
  }
  
  ## function to create pseudo-dmObj list from RData file
  fakeResult <- function(rdata.file) {
    ## RData file include plist, other objects
    load(rdata.file)

    ## augment plist to look "enough" like runModel() return
    
    plist$input <- input
    plist$dat <- dat
    plist$mlEst <- mlEst
    plist$tDat <- tDat
    plist$bdat <- bdat

    plist$calcInits <- function() { plist$theinits }
    
    plist
  }

  ##################################################################
  ## output file downloader handlers and helper routines
  ##################################################################

  ## DM downloaders

  output$downloadDMReport = downloadHandler(
    filename = function() {paste0(getOutputBaseName(), ".pdf")},
    content = function(file) {file.copy(outputPath(".pdf"), file)})
  
  output$downloadDMRData = downloadHandler(
    filename = function() {paste0(getOutputBaseName(), ".RData")},
    content = function(file) {file.copy(outputPath(".RData"), file)})

  output$downloadDMRav = downloadHandler(
    filename = function() {paste0(getOutputBaseName(), ".rav")},
    content = function(file) {file.copy(outputPath(".rav"), file)},
    contentType = "text"
  )

  ## extra plot downloader & zip creator

  output$downloadPlotsZip <- downloadHandler(
    filename <- function() {paste0(getOutputBaseName(), "_plots.zip")},
    content <- function(file) {file.copy(outputPath("_plots.zip"), file)}
  )

  zipPngs <- function(appRoot) {
    ret <- NULL
    owd <- setwd(getOutputDir())
    pngs <- Sys.glob("*.png")
    toc <- file.path(appRoot, "html", "plottoc.txt")
    if (length(pngs) > 0) {
      if (file.exists(toc)) {
        file.copy(toc, "plots_readme.txt")
        pngs <- c(pngs, Sys.glob("plots_readme.txt"))
      }
      zipfile <- paste0(outputPathBase(), "_plots.zip")
      zipcmd <- Sys.getenv("R_ZIPCMD")
      zipcmd <- if (zipcmd == '') { "/usr/bin/zip" } else { zipcmd }
      zip(zipfile, pngs,zip=zipcmd)
      ret <- file.path(getOutputDir(),zipfile)
    }
    setwd(owd)
    ret
  }

  zipPngsAvailable <- function() {
    zipfile <- paste0(outputPathBase(), "_plots.zip")
    file.exists(zipfile)
  }
  
  ## VRAP downloaders

  vrapDownloadHelper <- function(file, ext) {
    dlfile <- vrapOutputPath(ext)
    if(input$os=="win"){ 
      tmp.unix <- tempfile("unixtmp")
      file.copy(dlfile, tmp.unix)
      system(paste0("sed -e 's/$/\r/' ", tmp.unix, " > ", file))
      file.remove(tmp.unix)
    }else{
      file.copy(dlfile, file)
    }
  }
  
  output$downloadVRAPsum <- downloadHandler(
    filename = function() {
      paste0(getVRAPBaseName(),".sum")},
    content = function(file) {
      vrapDownloadHelper(file, ".sum")
    },
    contentType="text"
  )
  
  output$downloadVRAPbyr <- downloadHandler(
    filename = function() {
      paste0(getVRAPBaseName(),".byr")},
    content = function(file) {
      vrapDownloadHelper(file, ".byr")
    },
    contentType="text/csv"
  )

  output$downloadVRAPesc <- downloadHandler(
    filename = function() {
      paste0(getVRAPBaseName(),".esc")},
    content = function(file) {
      vrapDownloadHelper(file, ".esc")
    },
    contentType="text/csv"
  )

  ## VRAP demo file downloader

  output$downloadDemo <- downloadHandler(
    filename = function() {
      if (!is.null(input$vraptype) && input$vraptype=="demo") {
        if (!is.null(input$vrapdemodownload)) {
          downloadname <- EXAMPLENAMES[[input$vrapdemodownload]]
          return(downloadname)
        }
      }
      "Demo.rav"
    },
    content = function(file) {
      if (!is.null(input$vrapdemodownload)) {
        file.copy(file.path(DEMOFILESPATH,input$vrapdemodownload),file)
      }
    },
    contentType="text"
  )

  
  ##################################################################
  ## Generate a plot of the priors
  ##################################################################
  output$priorsplot <- renderPlot({
    par(mfrow=c(2,2))
    x <- seq(0,60,.1)
    pVar <- input$pSig^2
    ix <- rep(1, length(x)); ix[x>input$pRange[2]]=0; ix[x<input$pRange[1]]=0
    plot(x, dlnorm(x,log(input$pMode)+pVar,sqrt(pVar))*ix, 
         main='Production Prior',xlab='Production',ylab='Density',type='l',bty='n')
    ## plot(x, dlnorm(x,log(input$pMode)+input$pSigma^2,sdlog=input$pSigma)*ix, 
    ##    main='Production Prior',xlab='Production',ylab='Density',type='l',bty='n')
    
    x <- seq(0,20,.1)
    ix <- rep(1, length(x)); ix[x>input$cRange[2]]=0; ix[x<input$cRange[1]]=0
    plot(x, dnorm(x,mean=input$cMu,sd=input$cSig)*ix, 
         main='Log Capacity Prior',xlab='Log Capacity',ylab='Density',type='l',bty='n')
    
    x <- seq(-100,100,0.001)
    ix <- rep(1, length(x)); ix[x>input$msRange[2]]=0; ix[x<input$msRange[1]]=0
    plot(x,dnorm(x,mean=input$msMu,sd=input$msSig)*ix,
         main='marine survival coef Prior',xlab='ms coef',ylab='Density',type='l',bty='n')

    ## x <- seq(0,5001,.01)
    x <- seq(-100,100, 0.001)
    ix <- rep(1, length(x)); ix[x>input$flowRange[2]]=0; ix[x<input$flowRange[1]]=0
    plot(x,dnorm(x,mean=input$flowMu,sd=input$flowSig)*ix,
         main='freshwater survival coef Prior',xlab='fshwater coef',ylab='Density',type='l',bty='n')
  }, height = 400, width = 600 )
  
})
