#' Import ClinVar VCF
#'
#' This function allows you to read a ClinVar VCF into a table
#'
#' @export

download_clinvar <- function() {
  dir <- system.file("extdata", "Supplementary_Files/", package = "clinvaR")
  file <- sprintf("%sclinvar_%s.vcf.gz", dir, Sys.Date())
  download.file(url = "ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/vcf_GRCh37/clinvar.vcf.gz",
                destfile = file, method = "internal")
  system(sprintf("gunzip %s", file))
  file <- sprintf("%sclinvar_%s.vcf", dir, Sys.Date())

  file.by.line <- readLines(file)

  clean.lines <- file.by.line[!grepl("##.*", file.by.line)] #Remove ## comments
  clean.lines[1] <- sub('.', '', clean.lines[1]) #Remove # from header

  input <- read.table(text = paste(clean.lines, collapse = "\n"), header = T, stringsAsFactors = F,
                      comment.char = "", quote = "", sep = "\t")
  return(input)
}
