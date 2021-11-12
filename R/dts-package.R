#' @title Tools for the analysis of dts
#' @name dts
#'
#' @description This package provides a set of tools for the analysis of dts data.
#'
#'
#' @useDynLib dts, .registration = TRUE
#' @docType package
#' @aliases dts dts-package
#' 
#' @importFrom Rcpp evalCpp
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom parallel stopCluster
#' @importFrom data.table data.table
#' @importFrom data.table setDT
#' @importFrom data.table setkey
#' @importFrom data.table setnames
#' @importFrom data.table %between%
#' @importFrom data.table shift
#' @importFrom data.table rbindlist
#' @importFrom data.table nafill
#' @importFrom stats na.omit
#' 
#' @keywords internal
"_PACKAGE"