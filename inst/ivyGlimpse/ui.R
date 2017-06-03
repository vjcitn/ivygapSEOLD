

# define gene sets from cbioPortal

glioRTK = c(
"EGFR","ERBB2","PDGFRA","MET","KRAS","NRAS","HRAS","NF1","SPRY2","FOXO1","FOXO3","AKT1","AKT2","AKT3","PIK3R1","PIK3CA","PTEN"
)

pi3k = c(
"PIK3CA","PIK3R1","PIK3R2","PTEN","PDPK1","AKT1","AKT2","FOXO1","FOXO3","MTOR","RICTOR","TSC1","TSC2","RHEB","AKT1S1","RPTOR","MLST8"
)

ovtumsupp=c("DIRAS3","RASSF1","DLEC1","SPARC","DAB2","PLAGL1","RPS6KA2","PTEN","OPCML","BRCA2","ARL11","WWOX","TP53","DPH1","BRCA1","PEG3")

rasraf = c("KRAS","HRAS","BRAF","RAF1","MAP3K1","MAP3K2","MAP3K3","MAP3K4","MAP3K5","MAP2K1","MAP2K2","MAP2K3","MAP2K4","MAP2K5",
  "MAPK1","MAPK3","MAPK4","MAPK6","MAPK7","MAPK8","MAPK9","MAPK12","MAPK14","DAB2","RASSF1","RAB25")

someSets = list(rasraf=rasraf, glioRTK=glioRTK,
   pi3k=pi3k,
   ovtumsupp = ovtumsupp)

attr(someSets, "fullTitle") = list(
   glioRTK="Glioblastoma: RTK/Ras/PI3K/AKT Signaling (17 genes)",
   pi3k="General: PI3K-AKT-mTOR signaling (17 genes)",
   ovtumsupp = "Ovarian Cancer: Putative tumor-suppressor genes in epithelial ovarian cancer (16 genes)",
   rasraf = "General: Ras-Raf-MEK-Erk/JNK signaling (26 genes)")

names(someSets) = unlist(attr(someSets, "fullTitle")[names(someSets)])



library(plotly)

library(ivygapSE)
data(ivySE)


sb = metadata(ivySE)$subBlockDetails
sb = sb[!is.na(sb$survival_days),]

feats = colnames(sb)

ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   fluidRow(
      helpText("IvyGAP explorer: expression, clinical, and image-based data for glioblastoma patients; see backgrouund panel for more details")
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
   width=3
  ),
  mainPanel(
   tabsetPanel(
    tabPanel("basic",
     fluidRow(
      column( 
       fluidRow( helpText("hover over for tumor donor ID; partition data by dragging over points to select") ),
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

