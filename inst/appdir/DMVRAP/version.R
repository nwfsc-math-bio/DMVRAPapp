#shiny.version <- "Dynamic Model + VRAP 1.1.8"
shiny.version <- paste(
  packageDescription("DMVRAPapp", fields="Title"),
  packageDescription("DMVRAPapp", fields="Version")
)
