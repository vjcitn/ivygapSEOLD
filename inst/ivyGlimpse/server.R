library(shiny)
library(ggplot2)
library(plotly)
library(survival)
library(survminer)
library(hwriter)




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

molmap = structure(c("NA", "C", "CM", "CN", "M", "MN", "N", "NP", "P"), .Names = c("", 
   "Classical", "Classical, Mesenchymal", "Classical, Neural", "Mesenchymal", 
   "Mesenchymal, Neural", "Neural", "Neural, Proneural", "Proneural"
   ))

server = function(input, output, session) {
 library(ivygapSE)
 data(ivySE)
 sb = metadata(ivySE)$subBlockDetails
 sb = sb[!is.na(sb$survival_days),]
 output$xyplot = renderPlotly({
   df = sb[, c(input$x, input$y, "donor_id", "molecular_subtype")]
   df$donor_id = paste0("donor: ", df$donor_id)
   df$molm = molmap[df$molecular_subtype]
   p = ggplot(df, aes_(x=as.name(input$x), y=as.name(input$y), text=as.name("donor_id"))) + 
          geom_point() #data=df, mapping=aes_(colour=as.name("molm")))
   ggplotly(p, source="subset", tooltip="text") %>% layout(dragmode="select") #plot(sb[, input$x], sb[, input$y], xlab=input$x, ylab=input$y )
   })
 
procSel = reactive({
    event.data <- event_data("plotly_selected", source = "subset")
    if(is.null(event.data) == TRUE) return(NULL)
    dr = duplicated(sb$donor_id)
    udf <<- sb[-which(dr),]  # survfit does not find without <<-
    udf$grp = 0
    sdf = sb[event.data$pointNumber+1,]
    indo = sdf$donor_id
    udf[which(udf$donor_id %in% indo),]$grp = 1
    survfit(Surv(survival_days, rep(1,nrow(udf)))~grp, data=udf)
    })

 
 output$plot2 = renderPlot({
# print(procSel())
    # Get subset based on selection
#    event.data <- event_data("plotly_selected", source = "subset")
#    # If NULL dont do anything
#    if(is.null(event.data) == TRUE) return(NULL)
#    dr = duplicated(sb$donor_id)
#    udf <<- sb[-which(dr),]
#    udf$grp = 0
#    sdf = sb[event.data$pointNumber+1,]
#    indo = sdf$donor_id
#    udf[which(udf$donor_id %in% indo),]$grp = 1
    validate(need(!is.null(procSel()), "waiting for selection"))
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
