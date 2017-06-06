# ivygapSE

This package provides a SummarizedExperiment for Ivy-GAP metadata.  See the [ivyGlimpse app](http://vjcitn.shinyapps.io/ivyGlimpse) and an [explanatory video](https://www.youtube.com/watch?v=4qJaodLEI5o).

The metadata and expression data are
as distributed by [Allen Institute for Brain Science](http://glioblastoma.alleninstitute.org/static/download.html) as of 31 May 2017.

A brief vignette is available, and the ivyGlimpse() function will initiate a shiny app
for exploring relationships between sub-block level image features quantified in 'subBlockDetails',
survival time, and expression patterns.  Essentially, one can use a scatterplot of
a pair of image features to stratify the data.

ivyGlimpse will be accompanied by ivyTest, a tool for formally testing hypotheses
relating image characteristics to expression patterns, and ivyBridge, which will
emphasize interactive amalgamation of data and annotation related to glioblastoma.
