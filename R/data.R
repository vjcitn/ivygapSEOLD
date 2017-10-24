#' ivySE: SummarizedExperiment for IvyGAP expression data and metadata
#' @format SummarizedExperiment instance
#' @note
#' Expression data retrieved from \url{http://glioblastoma.alleninstitute.org/api/v2/well_known_file_download/305873915}
#' @details
#' %vjcair> unzip -l gene*zip \cr
#' Archive:  gene_expression_matrix_2014-11-25.zip \cr
#'   Length      Date    Time    Name \cr
#' ---------  ---------- -----   ---- \cr
#'     50585  03-31-2015 13:27   columns-samples.csv \cr
#'  86153820  10-31-2014 14:04   fpkm_table.csv \cr
#'      2015  11-24-2014 18:06   README.txt \cr
#'   1689619  10-31-2014 13:55   rows-genes.csv \cr
#' ---------                     ------- \cr
#'  87896039                     4 files \cr
#' @source processed from \url{glioblastoma.alleninstitute.org}; see Note.
#' @examples
#' \dontrun{   # how it was made
#' ivyFpkm = read.csv("fpkm_table.csv", stringsAsFactors=FALSE, 
#'       check.names=FALSE)
#' g = read.csv("rows-genes.csv", stringsAsFactors=FALSE)
#' library(SummarizedExperiment)
#' imat = data.matrix(ivyFpkm[,-1])
#' ivySE = SummarizedExperiment(SimpleList(fpkm=imat))
#' rowData(ivySE) = g
#' rownames(ivySE) = g$gene_symbol
#' col = read.csv("columns-samples.csv", stringsAsFactors=FALSE)
#' rownames(col) = col$rna_well_id
#' stopifnot(all.equal(as.character(col$rna_well_id), 
#'      as.character(colnames(imat))))
#' colData(ivySE) = DataFrame(col)
#' colnames(ivySE) = colnames(imat)
#' metadata(ivySE) = list(README=readLines("README.txt"))
#' metadata(ivySE)$URL = "http://glioblastoma.alleninstitute.org/static/download.html"
#' # metadata(ivySE)$builder = readLines("build.R")
#' de = read.csv("tumor_details.csv", stringsAsFactors=FALSE)
#' metadata(ivySE)$tumorDetails = de
#' subbl = read.csv("sub_block_details.csv", stringsAsFactors=FALSE)
#' metadata(ivySE)$subBlockDetails = subbl
#' bamtab = read.csv("bam.csv", stringsAsFactors=FALSE)
#' rownames(bamtab) = as.character(bamtab$rna_well)
#' bamtab[colnames(ivySE),] -> bamtreo
#' all.equal(rownames(bamtreo), colnames(ivySE))
#' colData(ivySE) = cbind(colData(ivySE), bamtreo)
#' }
#' data(ivySE)
#' names(metadata(ivySE))
"ivySE"

#' msigdb: 47 gene sets related to glioblastoma by 'Search Gene Sets' at msigdb
#' @format GeneSetCollection
#' @note Retrieved 20 Oct 2017, and imported with getGmt of GSEABase
"glioGSC"
