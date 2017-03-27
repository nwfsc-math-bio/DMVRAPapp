exportApp <- function(dir=file.path(getwd(),DMVRAP)) {
  if (!dir.exists(dir)) {
    if (!dir.create(dir, recursive=TRUE, mode="644")) {
      stop(paste0("DMVRAPapp::exportApp : Can't create directory ", dir, "\n"))
    }
  }

  appdir <- system.file('appdir/DMVRAP',package="DMVRAPapp");
  if (file.access(appdir, mode=5)) {
    stop(paste0("DMVRAPapp::exportApp : Can't access source directory ",
                appdir, "\n"))
  }

  if (!file.copy(appdir, dir, recursive=TRUE,
                 copy.mode=TRUE, copy.date=TRUE)) {
    stop(paste0("DMVRAPapp::exportApp : Copy from", appdir, " to ",
                dir, " failed.\n"))
  }
}
