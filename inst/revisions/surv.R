#
# this code is moderately cleaned up relative to ivyGlimpse components
# purpose was to isolate issue of non-complementarity of survival curves
# when complementary selections are made.  the priority goes to identifying
# events for donors of points in the selection.  those donors yield the grp=1
# survival curve, all others contribute to the grp=0 curve.  when a selection
# is complemented, the new grp=0 curve may be severely depleted of events
#

library(survival)
library(ivygapSE)
library(shiny)
library(plotly)
library(ggplot2)
if (!exists("ivySE")) data(ivySE)

image_featurenames = function() {
 c("normalized_area_le", "normalized_area_lehbv", "normalized_area_it", 
  "normalized_area_ithbv", "normalized_area_ct", "normalized_area_ctpnz", 
  "normalized_area_ctpnn", "normalized_area_ctpan", "normalized_area_cthbv", 
  "normalized_area_ctmvp", "normalized_area_ctne")
}

.check_ivy = function(se) {
 nmd = names(metadata(se))
 stopifnot("subBlockDetails" %in% nmd)
 md = metadata(se)$subBlockDetails
 stopifnot("donor_id" %in% names(md))
}

get_survdf = function(se) {
 .check_ivy(se)
 smd = split(md, md$donor_id)
 data.frame(donor_id=names(smd), 
   stime=vapply(smd, function(x) x$survival_days[1], double(1)))
}

get_blockFeats = function(se) {
 .check_ivy(se)
 md = metadata(se)$subBlockDetails
 cbind(donor_id = md$donor_id, md[, image_featurenames()])
}

survdf = get_survdf(ivySE)
survdf$grp = 1*sapply(smd, function(x) any(x$normalized_area_cthbv > .15, na.rm=TRUE))

blockfeats = get_blockFeats(ivySE)

print(table(survdf$grp))

ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    selectInput("featx", "x-feature", choices=image_featurenames(),
      selected = image_featurenames()[1]),
    selectInput("featy", "y-feature", choices=image_featurenames(),
      selected = image_featurenames()[2])
    ),
   mainPanel(
    tabsetPanel(
     tabPanel("Z",
      fluidRow(
       column(
        plotlyOutput("splot2"), width=5
        ),
       column(
        plotOutput("splot"), width=5
        )
       )),
     tabPanel("A", helpText("A"), textOutput("xpick"), textOutput("ypick")),
     tabPanel("B", dataTableOutput("seltab"))
     )
   )
  )
)

server = function(input, output) {
   output$xpick = renderText(input$featx)
   output$ypick = renderText(input$featy)
   blockfeats = get_blockFeats(ivySE)
   identify_drag = reactive({
       event_data("plotly_selected", source = "subset")
       })
   output$splot2 = renderPlotly({
       df = blockfeats[, c(input$featx, input$featy) ]
       myp = ggplot(df, aes_(x=as.name(input$featx), y=as.name(input$featy))) + geom_point()
       ggplotly(myp, source="subset", tooltip="text") %>% layout(dragmode="select")
       })
   output$splot = renderPlot({
       survdf = get_survdf(ivySE)
       blockfeats = get_blockFeats(ivySE)
       validate( need( nrow(ii <- identify_drag())>0, "waiting for drag over feature pairs"))
       ids = unique(blockfeats$donor_id[ ii$pointNumber+1 ])
       mm = match(ids, survdf$donor_id)
       survdf$grp = 0
       survdf$grp[mm] = 1
       plot(survfit(Surv(stime, rep(1, nrow(survdf)))~grp, data=survdf), main=input$featx)
       })
   output$xpick = renderText(input$featx)
   output$seltab = renderDataTable({
                      seltab = identify_drag() # seltab$pointNumber+1 is row number in blockfeats
                      inds = seltab$pointNumber+1
                      cbind(blockfeats[inds, c("donor_id", input$featx, input$featy)], seltab)
                    })
}
