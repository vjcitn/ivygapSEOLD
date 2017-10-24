#' simple app to explore image property quantifications in relation to survival and expression
#' @import shiny
#' @import survminer
#' @import hwriter
#' @import SummarizedExperiment
#' @import survival
#' @import plotly
#' @import ggplot2
#' @export
ivyGlimpse = function() {

## START UI
# define gene sets from cbioPortal

data(ivySE)

sb = metadata(ivySE)$subBlockDetails
sb = sb[!is.na(sb$survival_days),]

feats = colnames(sb)

ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   fluidRow(
      helpText("IvyGAP explorer: expression, clinical, and image-based data for glioblastoma patients; see background panel for more details")
   ),
   fluidRow(
      helpText("subBlockDetail features for selectables scatterplot")
   ),
   fluidRow(
    selectInput("x", "x", choices=feats, selected="normalized_area_it")
   ),
   fluidRow(
    selectInput("y", "y", choices=feats, selected="normalized_area_ct")
   ), 
   fluidRow(
    selectInput("gs", "cbioP sets", choices=names(someSets), selected=names(someSets)[1])
   ),
   fluidRow(
    helpText("Supported by NCI ITCR U01 CA214846 and U24 CA180996")
   ),
   width=3
  ),
  mainPanel(
   tabsetPanel(
    tabPanel("basic",
     fluidRow(
      column( 
       fluidRow( helpText("hover over for tumor donor ID; partition data by dragging over points to select; click on a specific point to visit IvyGAP clinical specimen page for that sample") ),
       fluidRow( plotlyOutput("xyplot") ), width=5 ),
      column( 
       fluidRow( helpText("Kaplan-Meier, grp=1 for selected donors") ),
       fluidRow( plotOutput("plot2") ), width=5)
      ),
     fluidRow(
      column( plotOutput("boxes1"), width=5 ),
      column( plotOutput("boxes2"), width=5 )
      )
     ),
    tabPanel("vocab", tableOutput("vocab")),
    tabPanel("background", 
     fluidRow(
       helpText("Read the ", a(href='http://help.brain-map.org/display/glioblastoma/Documentation?preview=/8028197/8454231/IvyOverview.pdf', 'Allen Institute Technical White Paper'))
      ),
     fluidRow(helpText("The README from the ", a(href='http://glioblastoma.alleninstitute.org/api/v2/well_known_file_download/305873915',"zip archive of expression data"))),
     fluidRow(verbatimTextOutput("bkgrd"))
     )
    )
   )
  )
 )

## END UI
## START SERVER


featanno = c("normalized_area_le"   =   
	"Leading Edge (LE)",
"normalized_area_lehbv" =
	"Hyperplastic blood vessels in leading edge (LEhbv)",
"normalized_area_it"   =   
	"Infiltrating Tumor (IT)",
"normalized_area_ithbv" =
	"Hyperplastic blood vessels in infiltrating tumor (IThbv)",
"normalized_area_ct"   =   
	"Cellular Tumor (CT)",
"normalized_area_ctpnz" =
	"Perinecrotic zone (CTpnz)",
"normalized_area_cthbv" =
	"Hyperplastic blood vessels in cellular tumor (CThbv)",
"normalized_area_ctpnn" =
	"Pseudopalisading cells but no visible necrosis (CTpnn)",
"normalized_area_ctpan" =
	"Pseudopalisading cells around necrosis (CTpan)",
"normalized_area_ctmvp" =
	"Microvascular proliferation (CTmvp)",
"normalized_area_ctne" =
	"Necrosis (CTne)")

molmap = structure(c("NA", "C", "CM", "CN", 
     "M", "MN", "N", "NP", "P"), .Names = c("", 
         "Classical", "Classical, Mesenchymal", "Classical, Neural", 
         "Mesenchymal", "Mesenchymal, Neural", "Neural", "Neural, 
          Proneural", "Proneural"
   ))

server = function(input, output, session) {
 sb = metadata(ivySE)$subBlockDetails
 sb = sb[!is.na(sb$survival_days),]
 output$xyplot = renderPlotly({
   df = sb[, c(input$x, input$y, "donor_id", "molecular_subtype",
                "specimen_page_link")]
   df$donor_id = paste0("donor: ", df$donor_id)
   df$molm = molmap[df$molecular_subtype]
   p = ggplot(df, aes_(x=as.name(input$x), y=as.name(input$y), text=as.name("donor_id"))) + 
          geom_point() #data=df, mapping=aes_(colour=as.name("molm")))
   gp = ggplotly(p, source="subset", tooltip="text") %>% 
          layout(dragmode="select") 
   event.data <- event_data("plotly_click", source = "subset")
   if (!is.null(event.data)) browseURL(df[event.data$pointNumber+1,
       "specimen_page_link"])
   gp
   })
 
procSel = reactive({
    event.data <- event_data("plotly_selected", source = "subset")
    if(is.null(event.data) == TRUE) return(NULL)
    dr = duplicated(sb$donor_id)
    udf <<- sb[-which(dr),]  # survfit does not find without <<-
    udf$grp <<- 0
    sdf = sb[event.data$pointNumber+1,]
    indo = sdf$donor_id
    udf[which(udf$donor_id %in% indo),]$grp <<- 1
    survfit(Surv(survival_days, rep(1,nrow(udf)))~grp, data=udf)
    })
 
 output$plot2 = renderPlot({
    validate(need(!is.null(procSel()), "waiting for (dragged) selection"))
    mm = procSel() #survfit(Surv(survival_days, rep(1,nrow(udf)))~grp, data=udf)
    suppressWarnings({ ggsurvplot(mm) })
   })

 output$boxes1 = renderPlot({
    # Get subset based on selection
    event.data <- event_data("plotly_selected", source = "subset")
    # If NULL dont do anything
    if(is.null(event.data) == TRUE) return(NULL)
    dr = duplicated(sb$donor_id)
    udf <<- sb[-which(dr),]
    udf$grp = 0
    sdf = sb[event.data$pointNumber+1,]
    intu = sdf$tumor_name
    seSEL = ivySE[someSets[[input$gs]], which(ivySE$tumor_name %in% intu)]
    logp = function(x) log(x+1)
    par(mar=c(5,4,2,2))
    meds = apply(assay(seSEL),1,median,na.rm=TRUE)
    omeds = order(meds)
    boxplot( data.frame(logp(t(assay(seSEL)[omeds,]))), main="in image subset", las=2, ylab="log fpkm",
      ylim=c(0,7))
   })

 output$boxes2 = renderPlot({
    # Get subset based on selection
    event.data <- event_data("plotly_selected", source = "subset")
    # If NULL dont do anything
    if(is.null(event.data) == TRUE) return(NULL)
    dr = duplicated(sb$donor_id)
    udf <<- sb[-which(dr),]
    udf$grp = 0
    sdf = sb[event.data$pointNumber+1,]
    intu = sdf$tumor_name
    seSEL = ivySE[someSets[[input$gs]], which(ivySE$tumor_name %in% intu)] # for ordering
    meds = apply(assay(seSEL),1,median,na.rm=TRUE)
    omeds = order(meds)
    seUNSEL = ivySE[someSets[[input$gs]], -which(ivySE$tumor_name %in% intu)]
    logp = function(x) log(x+1)
    par(mar=c(5,4,2,2))
    boxplot( data.frame(logp(t(assay(seUNSEL)[omeds,]))), main="not in image subset", las=2, ylab="log fpkm",
        ylim=c(0,7))
   })
  output$vocab = renderTable({
    plop = function(x) gsub("^", " ", x)
    data.frame(short=c("_Molec. subtype_", names(molmap), "_Feature_", names(featanno)), 
             long=c(" ", as.character(molmap), " ", as.character(featanno)))
    })
  output$bkgrd = renderText( paste(metadata(ivySE)$README, collapse="\n") )
 }
shinyApp(ui=ui, server=server)
}
