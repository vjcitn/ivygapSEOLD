#' simple app to explore image property quantifications in relation to survival and expression
#' @import shiny
#' @import survminer
#' @import hwriter
#' @import SummarizedExperiment
#' @import survival
#' @import plotly
#' @import ggplot2
#' @export
ivyGlimpse = function() runApp( system.file("ivyGlimpse", package="ivygapSE") )
