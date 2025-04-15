#' Remove temporary files from NONMEM run
#' 
#' @param path path to NONMEM run folder
#' 
#' @export
clean_nonmem_folder <- function(path) {
  files <- dir(path)
  blacklist <- c(
    "compile.lnk",
    "FCON",
    "FDATA",
    "FDATA.csv",
    "FMSG",
    "FORIG",
    "FREPL",
    "FREPORT",
    "FSIZES",
    "FSTREAM",
    "FSUBS",
    "FSUBS2",
    "FSUBS.f90",
    "gfortran.txt",
    "INTER",
    "LINKC.LNK",
    "LINK.LNK",
    "nmpathlist.txt",
    "nmprd4p.mod",
    "nonmem",
    "PRDERR",
    "PRSIZES.f90"
  )
  rm_files <- file.path(path, intersect(blacklist, files))
  unlink(rm_files)
}
