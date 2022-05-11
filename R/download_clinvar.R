#' Import ClinVar VCF
#'
#' This function allows you to read a ClinVar VCF into a table
#' @return chrom_counts a barplot of the couunt of mutations in each chromosome
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

  ##FILTERING DATA

  chrom <- c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14",
             "15","16","17","18","19","20","21","22","X","Y")


  ##Loop calling function filer_by_chrom for each chromosome

  for (x in chrom) {
    chrom_x = gsub(" ", "", paste("CHR_",x))
    #assign(chrom_x, filer_by_chrom(x))
    assign(chrom_x, nrow(input[(input$CHROM)==x,]))
  }

  ##Filtering INPUT by chromosome
  ##filer_by_chrom <- function(wkg_chrom) {
    ##return(nrow(input[(input$CHROM)==x,]))
  ##}


  ##Plot chromosome vs chromosome totals counts
  CHROM <- c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14",
             "15","16","17","18","19","20","21","22","X","Y")

  COUNTS = c(CHR_1	,CHR_2	,CHR_3	,CHR_4	,CHR_5	,CHR_6	,CHR_7	,CHR_8	,
             CHR_9	,CHR_10	,CHR_11	,CHR_12	,CHR_13	,CHR_14	,CHR_15	,CHR_16	,
             CHR_17	,CHR_18	,CHR_19	,CHR_20	,CHR_21	,CHR_22	,CHR_X	,CHR_Y	)

  df <- data.frame(CHROM,COUNTS)

  df$CHROM <- factor(df$CHROM, levels = c("1","2","3","4","5","6","7","8","9",
                                          "10","11","12","13","14","15","16","17","18","19","20","21","22","X","Y"))

  chrom_counts <- barplot( df$COUNTS ~ df$CHROM,
                           las = 2,
                           cex.names = 0.7,
                           cex.lab = 0.7,
                           cex.axis = 0.7,
                           xlab="Chromosome Number",
                           ylab="Total Number of Mutations Reported",
                           main = "Summary of mutations reported in CLINVAR")

  return(chrom_counts)
}


