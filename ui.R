require(tidyverse)
require(DT)
library(BiocManager)
#options(repos = BiocManager::install(version = '3.15'))
library(limma)
require(shinydashboard)
require(reshape2)
require(shinyjs)
require(dlookr)
require(caTools)
require(e1071)
require(caret)
require(pROC)
require(MLmetrics)
require(class)
require(ElemStatLearn)
require(RColorBrewer)
require(FNN)
require(cluster)
require(randomForest)
require(factoextra)
require(MASS)
require(psych)
require(Boruta)
require(glmnet)
require(igraph)
library(ComplexHeatmap)
require(shinyDarkmode)
require(car)
require(shinyWidgets)
require(leaflet)
require(shinyBS)
dashheader <- dashboardHeader(title = "",titleWidth = 230,
                              tags$li(class = "dropdown",fluidRow(
                              
                                tags$div(style = "margin-top: 15px;margin-right: -160px;font-size: 15px",
                                         prettySwitch("togglemode", "Night mode", value = FALSE, fill = TRUE, status = "info")
                                )
                              )),
                              tags$li(class = "dropdown",  tags$a(href="https://github.com/sharifrahmanie/DeepBC/issues/new", icon("bug"), "Report bug", target= "_blank")),
                              tags$li(class = "dropdown",  tags$a(href="mailto:rahami.biotech@gmail.com", icon("envelope"), "Contact", target= "_blank")),
                              tags$li(class = "dropdown",  tags$a(href="https://www.instagram.com/sharifrahmani_e/", icon("instagram"), "Instagram", target= "_blank")),
                              tags$li(class = "dropdown", tags$a(href="https://www.linkedin.com/in/sharifrahmanie/", icon("linkedin"), "Linkedin", target= "_blank")),
                              tags$li(class = "dropdown", tags$a(href="https://github.com/sharifrahmanie", icon("github"), "Github", target= "_blank")),
                              tags$li(class = "dropdown", tags$a(href="https://scholar.google.com/citations?user=6f8yQfsAAAAJ&hl=en", tags$img(src="google-scholar.png", height = "20", width= "20"), "Google scholar", target= "_blank"))

                              )

dashSidebar <- dashboardSidebar(
  tags$head(tags$link(rel="shortcut icon", href="favcaon.png", height ="100%", width = "100%")),
  shinyjs::useShinyjs(),
  use_darkmode(),
  sidebarMenu(id = "sidebar",
              menuItem(
                text = "Home",
                tabName = "home",
                icon = icon("home")
              ),
              menuItem(
                text = "Data preprocessing",
                tabName = "datapreprocessing",
                icon = icon("file-alt")
                
              ),
              menuItem(
                text = "Differential gene expression",
                tabName = "expression_analysis",
                icon = icon("dna")
              ),
              menuItem( 
                text = "Machine learning",
                tabName = "machinel",
                icon = icon("microchip"),
                startExpanded = T,
                menuSubItem(text = "Feature selection", tabName = "featureselection", icon = icon("check")),
                menuSubItem(text = "Dimentionality reduction", tabName = "dimentionalityreduction", icon = icon("check-circle")),
                menuSubItem(text = "Regression", tabName = "regression", icon = icon("slash")),
                menuSubItem(text = "Random forest", tabName = "randomforest",icon = icon("tree")),
                menuSubItem(text = "K-Nearst Neighbor", tabName = "knn",icon = icon("shapes")),
                menuSubItem(text = "Support vector machine", tabName = "svm",icon = icon("divide")),
                menuSubItem(text = "Naive Bayes", tabName = "naivebayes",icon = icon("envelope", lib = "font-awesome")),
                menuSubItem(text = "Clustering", tabName = "clustering",icon = icon("sitemap"))
              ),
              menuItem(text = "Breast cancer subtyping",tabName = "DL",icon = icon("connectdevelop"), badgeLabel = "Coming soon"),
              uiOutput('style_tag')
  )
              
  
)

dashBody <- dashboardBody(


  ####################################################
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
  #########################################################
tabItems(
    tabItem(
      tabName = "home",
                             column(6, box(title = "DeepBC", solidHeader = F, background = "navy", width = "100%", collapsible = T, collapsed = F,
                                            tags$p(style="text-align: justify;",tags$b("DeepBC"), "(Deep Learning for Breast Cancer) is a free shiny web application framework to allow users (especially non-coders) to perform differential gene expression analyses and machine learning algorithms just by a few simple clicks.
                           It covers routine gene expression analyses, including DEGs table, plot volcano, MD,  box, heatmap, and network.
                           The machine learning part helps users with feature selection, dimensionality reduction, regression (including simple, multiple, polynomial, and logistic), random forest, k-nearest neighbor, support vector machine, naive Bayes, and clustering.
                           A breast cancer subtyping section using artificial neural networks will be available soon. A complete list of R packages used by this website is available ",actionBttn(inputId = "packagesver",label = "here;",style = "jelly", size = "xs")," however, a short description and the name and the carn's link of packages are provided in each section.
                           DeepBC deals with user-provided files; if you encounter an error, please report it in report bug with a short discription of your file. The maximum file size supported is", tags$b("10 Mb."), tags$a(href= "https://www.youtube.com/channel/UCMk882DRuOSwrCSbn0xHk-w/videos", target= "_blank", "Tutorials.")))),
                             
      bsTooltip(id = "packagesver", "Click me",
                "right", options = list(container = "body")),       
        column(6, box(title = "Contributor", solidHeader = F, background = "navy", width = "100%", collapsible = T, collapsed = F,
                       column(3, tags$image(src= "edi.jpg", height = "100", width = "90"),
                        column(12,br()),
                       uiOutput("download_cv", width = "100%"),
                       column(12,br()),
                       actionButton(inputId = "findmemap",label = "Find me", icon = icon("map"), width = "90%")
                              ),
                       
                       column(9, tags$p(style="text-align: justify;", tags$b("Edris Sharif Rahmani"), "has a master's degree in medical biotechnology. He worked five years on high-throughput sequencing data analyses (exome sequencing, RNA-Seq, and microarray). His passion for machine learning led him to learn programming languages, such as R, Python, awk,
                                       and shell scripting. Now he is teaching R and Python to postgraduate students in the ",tags$a(href= "http://genomefan.ir/", target= "_blank", "GenomeFan"), " company. He is interested in designing web applications, R packages, algorithms, and predicting models to solve bioinformatics problems. He is currently looking for a Ph.D. in bioinformatics
                                       or computational biology focusing on cancer genomics.")),
                       column(12, br()),
                        column(12,
                               #leafletOutput("mylocation", width = "100%", height = 170)
                               uiOutput("mylocation")
                               ),
                       
        ))
    ),

 ######################################### data preprocessing ###################
    tabItem(tabName = "datapreprocessing",
      fluidRow(
            column(12, box(title = "Data preprocessing",status = "info", collapsible = T, solidHeader = T, width = "100%",
                           infoBox(title = "Missing value, transformation, outliers", icon = icon("file-alt"), color = "yellow", width = 4, fill = T),
                           p(style="text-align: justify;","Data preprocessing is the first crucial step in preparing the raw data and making it suitable for a machine learning model. 
                             Finding missing values and detecting outliers are essential steps before performing any analysis. 
                             Some algorithms require data to meet some assumptions, such as following a normal distribution. 
                             Data transformation (e.g., log, z-score, scaling, etc.) can prevent model bias and reduce computation time. 
                             DeepBC uses", tags$a(href= "https://cran.r-project.org/web/packages/dlookr/index.html", target= "_blank", "dlookr"),"package to diagnose, explore, and transform data."))),
            column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                          radioButtons(inputId = "datapreprofileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                          radioButtons(inputId = "preprocessheader",label = "Header",choices = c("Yes", "No")),
                          fileInput("uploaded_predata",label = "Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                          actionButton("prepross","Process", width = "100%"))),
            column(8,tabBox(width = 100,
                            tabPanel(title = "Diagnosis panel",
                                     fluidRow(
                                       column(12,
                                              tags$img(src="data_diagnisis.png", height = 300, width = "100%")))),
                            tabPanel(title = "Treatment panel",
                                     fluidRow(
                                       column(12,
                                              tags$img(src="data_action.png", height = 300, width = "100%"))))
                            
                            )),
            conditionalPanel(condition = "input.prepross && output.uploaded_predata_hide_tabpanel == false",
                             infoBox(title = "Error", subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))  

      ),
      conditionalPanel(condition = c("input.prepross && output.uploaded_predata_hide_tabpanel"), 
      tabsetPanel(type = "tabs",
               tabPanel(title = "Table", icon = icon("table"),
                        box(title = "Uploaded table",status = "primary", solidHeader = T, collapsible = T,width = "100%",
                            DTOutput("data_preprocessing"))
                 
               ),
               tabPanel(title = "Diagnosis", icon = icon("stethoscope"),
                        conditionalPanel(condition = c("input.prepross && output.uploaded_predata_hide_tabpanel"),
                                         fluidRow(
                                           column(6,
                                                  box(title = "General overview",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                      column(12,selectInput( inputId = "generaloverview", label = "Select one", choices = c("All", "Numeric", "Categorical", "Missing", "MissingCategorical"),multiple = F, width = "40%")),
                                                      column(12, br()),
                                                      DTOutput("dataprestatistics"))),
                                           column(6, box(title = "Outlier detection",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                         column(6,selectInput(inputId = "outlierdetection", label = "Select one",choices = c("All", "Outliers", "OutliersRatio"), multiple = FALSE, width = "100%"),
                                                         ),
                                                         column(6,sliderInput(inputId = "outlier_ratio",label = "Change the outlier ratio %",min = 0,  max = 100, value = 5, step = 5,width = "100%")),
                                                         DTOutput("outlierDT")))),
                                         fluidRow(
                                           column(6,box(title = "Missing values",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                        plotOutput("paretoNA"))),
                                           column(6,box(title = "Outlier distribution",status = "primary", solidHeader = T,collapsible = T, width = "100%",height = 460,
                                                      column(6, selectInput(inputId = "outlierplotvariable",label = "Select a variable",multiple = F,selected = , choices = NULL)),
                                                      plotOutput("outliersplot" ,height = 325)))))
               ),
               tabPanel(title = "Treatment", icon = icon('syringe'),
                        conditionalPanel(condition = c("input.prepross && output.uploaded_predata_hide_tabpanel"),
                                         fluidRow(
                                           box(title = "1-Missing value",status = "primary",solidHeader = T, collapsible = TRUE,
                                               column(6, selectInput(inputId = "missingtype", label = "Select one", choices = c("Numeric", "Character", "Numeric-Character", "Remove", "No-action"), multiple = FALSE)),
                                               column(6,conditionalPanel(condition = "output.hide_noaction_choices && output.hide_romve_missing_choices == false",
                                                                         selectInput(inputId = "missingmethod", label = "Select a method", choices = NULL, multiple = FALSE, selected = NULL)),
                                                      conditionalPanel(condition = "output.hide_romve_missing_choices",
                                                                       selectInput(inputId = "roworcolumn", label = "Row or column", choices = c("row", "column"), multiple = FALSE, selected = NULL))),
                                               column(12,actionButton(inputId = "missinaction", label = "Apply",width = "47%")),
                                               column(12,br(), br()),
                                               DTOutput("missingprerocessingaction")
                                               #verbatimTextOutput("missing_table_reactive")
                                               ),
                                           box(title = "2-Outilers",status = "primary",solidHeader = T, collapsible = TRUE,
                                                   column(6,pickerInput(inputId = "outliertransformation", label = "Select the variable(s)", choices = "", options = list(`actions-box`=TRUE, pickerOptions(maxOptions = 2)), multiple = T,width = "100%"),
                                                          selectInput(inputId = "outliermethod", label = "Select a method", choices = c("mean", "median", "mode", "capping", "No-action"), multiple = FALSE, selected = "No-action")),
                                                   
                                                   
                                                   #outlierprerocessingaction
                                                   column(6,sliderInput(inputId = "outlier_ratio_imputation",label = "Change the outlier ratio %", min = 0, max = 100,value = 0,step = 5,width = "100%"),
                                                          actionButton(inputId = "oulieractionaction", label = "Apply",width = "100%")),
                                                   DTOutput("outlierprerocessingaction"))
                                         ),
                                         fluidRow(
                                           box(title = "3-Numeric transformation",status = "primary",solidHeader = T, collapsible = TRUE,
                                               column(6,selectInput(inputId = "datatransformation", label = "Select a method", choices = c("log", "log2", "log10", "log+1", "zscore", "minmax", "sqrt", "x^number", "No-action"), multiple = F),
                                                      actionButton(inputId = "numerictransformationaction", label = "Apply",width = "100%")),
                                               column(6,
                                                      conditionalPanel(condition = "output.hide_timesnumber_degree",
                                                                       sliderInput(inputId = "timenumbwer", label = "Select a degree",min = -3, max = 3, value = -0.5, step = 0.5))),
                                               column(12, br()),
                                               DTOutput("numerictransformationtable")
                                           ),
                                           
                                           
                                           box(title = "4-Character transformation",status = "primary",solidHeader = T, collapsible = TRUE, 
                                               column(6,pickerInput(inputId = "charvariablestotransform", label = "Select variable(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                      actionButton(inputId = "chartransformationaction", label = "Apply",width = "100%")),
                                               column(5, br(),  
                                                      uiOutput("download_preprocess_table")),
                                               column(12, br()),
                                               DTOutput("chartransformationtofactor"))
                                         )))
      ))
      ),
 
    ########################################  DEGs   #####################################
    tabItem(
      tabName = "expression_analysis",
      fluidRow(
      column(12, box(title = "High throughput sequencing analysis",status = "info", collapsible = T, solidHeader = T, width = "100%",
                     infoBox(title = "RNA-Seq, Microarray", icon = icon("dna"), color = "navy", width = 4, fill = T),
                     p(style="text-align: justify;", "DeepBC uses", 
                       tags$a(href= "https://bioconductor.org/packages/release/bioc/html/limma.html", target= "_blank", "limma"), "package to perform differential gene expression analysis, ", 
                       tags$a(href= "https://cran.r-project.org/web/packages/ggplot2/index.html", target= "_blank", "ggplot2"), "to visualize it, ",
                       tags$a(href= "https://cran.r-project.org/web/packages/igraph/index.html", target= "_blank", "igraph"), " to analyse gene expression network, and finally,",
                       tags$a(href= "https://bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html", target= "_blank", "ComplexHeatmap"), "package to construct heatmap graph.
                       With DeepBC, it is highly convenient to change the parameters of the volcano, heatmap, and network plot to observe changes in real-time. Thanks to shiny."
                       ))),
      #######################################
      # Separation selection chose file and analysis
        column(4,box(title = "File uploading",status = "primary", solidHeader = TRUE, collapsible = TRUE,width = "100%",
            column(9,radioButtons(inputId = "data_t",label = "Select a data format",choices = c(csv = ",", Tab= "\t"))),
            selectInput("adjpvalmethod", "Select an adjusting p-value method", choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"),selected = "fdr", width = "100%"),
            br(),
            fileInput("uploaded_expression_table",label = "Upload an expression table",accept = c(".csv", ".tsv"),multiple = FALSE),
            actionButton("anaylse_data_display","Analyse",width = "100%"),
            )),
            column(8,tabBox(width = 100,
                            tabPanel(title = "Heatmap", 
                                     fluidRow(
                                       column(12,
                                              tags$img(src="heatmap.png", height = 315, width = "100%")))),
                            tabPanel(title = "Network",
                                     fluidRow(
                                       column(12,
                                              tags$img(src="network.png", height = 315, width = "65%")))),
                            tabPanel(title = "Volcano plot", 
                                     fluidRow(
                                       column(12,
                              tags$img(src="geneexp_volcano.png", height = 315, width = "100%")))),
                            tabPanel(title = "MD plot",
                                     fluidRow(
                                       column(12,
                                              tags$img(src="geneexp_MD.png", height = 315, width = "100%")))),
                            tabPanel(title = "Box plot",
                                     fluidRow(
                                       column(12,
                                              tags$img(src="geneexp_box.png", height = 315, width = "100%")))),
                            
                   tabPanel(title = "Help",
                            fluidRow(
                            column(12,
                                   tags$p("Crucial steps:"),
                                   tags$ul(
                                     tags$li("The ", tags$b("second row"), "must start with", tags$b('Group')," keyword, and follows by group names."),
                                     tags$li("It is manditory to specify only two names (",tags$b("treat"),"and", tags$b("control"), ")."),
                                     tags$li("R is", tags$b("case sensitive"),"; therefore, each group name should be typed the same."),
                                     tags$li("An" , tags$b(" auto log transformation"), "applies by the tool if data is unlogged."),
                                     tags$li("The maximum file size supported is", tags$b("10 Mb"),".")
                                   ),
                                   tags$p("Example:"),
                                   column(8,tableOutput("example_show")),column(4, uiOutput("download_expression_exmp")), column(12))))))
    ),
      conditionalPanel(condition = c("input.anaylse_data_display && output.fileUploaded"),
      tabsetPanel(type = "tabs",
                  
                  tabPanel(title = "Table", icon = icon("table"),
                           fluidRow(
                          column(12,box(width = "100%",status = "primary",solidHeader = T, collapsible = T,
                                
                            uiOutput("download_diff_table"),
                            br(),
                            column(12,DTOutput("DeepBC"))
                           )
                           ))),
                  tabPanel(title = "Box plot", icon = icon("chart-line"),
                           box(width = "100%",status = "primary",solidHeader = T, collapsible = T,
                               column(5, column(6,uiOutput("download_box_plot"), column(6))),
                               column(2, actionButton(inputId = "boxplotaction", label = "Display plot", width = "100%")),
                               column(5),
                               column(12,br()),
                               
                               column(12,plotOutput("box_plot_plot",width = "100%",height = 600)))),
                  tabPanel(title = "MD plot", icon = icon("chart-line"),
                           box(width = "100%",status = "primary",solidHeader = T, collapsible = T,
                               column(5, column(6,uiOutput("download_intensity_plot"),column(6))),
                               column(2, actionButton(inputId = "intensityplotaction", label = "Display plot", width = "100%")),
                               column(5),
                               column(12,br()),
                               column(12,plotOutput("intensity_plot",width = "100%",height = 600)))),
                  tabPanel(title = "Volcano plot", icon = icon("chart-line"),
                           box(width = "100%",status = "primary",solidHeader = T, collapsible = T,
                               fluidRow(
                                 column(6, column(6,uiOutput("download_volcano_plot", width = "100%")),column(6)),
                                 column(2, actionButton(inputId = "volcanoaction", label = "Display plot", width = "100%")),
                                 column(4, br()),
                                 column(12,column(3,br(),
                                                  sidebarPanel(width = "100%",
                                                               selectInput("pval_adjpval", width = 300,"Select P-value or Adjusted P-value",choices = c("Pvalue", "AdjustedPvalue")),
                                                               sliderInput("significance", width = 300,"Change the significance level",min =1,max = 3,value = 2,step = 0.1),
                                                               sliderInput("logfcup", width = 300, "Change the log2FC (Up)",min = 0, max = 10, value = 1,step = 0.1),
                                                               sliderInput("logfcdown", width = 300,"Change the log2FC (Down)", min = -10,max = 0,value = -1,step = 0.1))),
                                        column(9,br(),plotOutput("volcano_plot",width = "100%", height = 600)))
                               ),
                               
                           )),
                  tabPanel(title = "Heatmap", icon = icon("map"),
                    box(width = "100%", status = "primary",solidHeader = T, collapsible = T,
                        column(3, sidebarPanel(width = "100%",
                                               selectInput("heatmappvalue", width = 300,"Select P-value or Adjusted P-value",choices = c("Pvalue", "AdjustedPvalue")),
                                               sliderInput("heatmapsignificance", width = 300,"Change the significance level",min =1,max = 3,value = 2,step = 0.1),
                                               sliderInput("heatmaplogfcup", width = 300, "Change the log2FC (Up)",min = 0, max = 10, value = 1,step = 0.1),
                                               sliderInput("heatmaplogfcdown", width = 300,"Change the log2FC (Down)", min = -10,max = 0,value = -1,step = 0.1),
                                               sliderInput("heatmap_degsnumber", width = 300,"Number of DEGs to include (each)", min = 1,max = 100,value = 25,step = 1),
                                               sliderInput(inputId = "heatmapclusternumber", label = "Number of clusters",min = 1, max = 10, value = 5, step = 1),
                                               sliderInput(inputId = "heatmapgenelabesize", label = "Label size",min = 0, max = 20, value = 7, step = 1),
                                               actionButton(inputId = "heatmapaction", label = "Display map", width = "100%")
                                               
                        )),
                        column(9,plotOutput("heatmap_analysis", width = "100%", height = 730)),
                  )),
                  tabPanel(title = "Network analysis", icon = icon("project-diagram"),
                           box(width = "100%",status = "primary",solidHeader = T, collapsible = T,
                               column(3, sidebarPanel(width = "100%",
                                            selectInput("genenetworkpvalue", width = 300,"Select P-value or Adjusted P-value",choices = c("Pvalue", "AdjustedPvalue")),
                                            sliderInput("genenetworksignificance", width = 300,"Change the significance level",min =1,max = 3,value = 2,step = 0.1),
                                            sliderInput("genenetworklogfcup", width = 300, "Change the log2FC (Up)",min = 0, max = 10, value = 1,step = 0.1),
                                            sliderInput("genenetworklogfcdown", width = 300,"Change the log2FC (Down)", min = -10,max = 0,value = -1,step = 0.1),
                                            sliderInput("genenetwork_degsnumber", width = 300,"Number of DEGs to include (each)", min = 1,max = 100,value = 25,step = 1),
                                            
                                            br(),
                                  
                                            actionButton(inputId = "genenetworkeaction", label = "Display network", width = "100%"),
                                            br()
                                            )),
                               column(3, sidebarPanel(width = "100%",
                                                      selectInput("genenetworkcormethod",label = "Correlation method",choices = c("pearson","spearman"), multiple = F, width = "100%"),
                                                      sliderInput(inputId = "genenetworkcordegree", label = "Remove edges below correlation",min = 0, max = 0.99, value = 0.8, step = 0.01),
                                                      selectInput("genenetworkmode", label = "Network mode",choices = c("directed", "undirected", "max", "min", "upper", "lower", "plus"), multiple = F, width = "100%"),
                                                      selectInput(inputId = "networklayout", label = "Layout", choices = c("layout.auto", "layout.random" ,"layout.circle","layout.sphere", "layout.fruchterman.reingold", "layout.kamada.kawai", "layout.spring", "layout.reingold.tilford","layout.fruchterman.reingold.grid", "layout.lgl", "layout.graphopt", "layout.mds", "layout.svd", "layout.norm"), multiple = F),
                                                      sliderInput(inputId = "genenetworklablesize", label = "Size of lables",min = 0.1, max = 1.5, value = 1, step = 0.1),
                                                      sliderInput(inputId = "genenetworklabledist", label = "Lables distance",min = 0.1, max = 1.5, value = 0.6, step = 0.1),
                                                      
                                                      
                                      )),
                               column(6,plotOutput("genenetwork_analysis", width = "100%", height = 650)),
                               )),
                  )),
      conditionalPanel(condition = "input.anaylse_data_display && output.fileUploaded == false",
                       infoBox(title = "Error",
                               subtitle = "It must be  group's row, separation field, or file format.",
                               color = "olive",
                               icon = icon("bug"),fill = TRUE)
      )
    ),
 
    ###################################################### Feature selection ##############################
    tabItem(tabName = "featureselection",
            fluidRow(
              column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                             
                             infoBox(title = "feature selection", value = 1, icon = icon("check-circle"), color = "teal", width = 4, fill = T),
                             
                             p(style="text-align: justify;","Reducing the number of input variables when developing a predictive model is beneficial to reduce the computational cost and improve the model performance. DeepBC uses", tags$a(href= "https://cran.r-project.org/web/packages/Boruta/index.html", target= "_blank", "Boruta"), ",",
                               tags$a(href= "https://cran.r-project.org/web/packages/randomForest/index.html", target= "_blank", "randomForest"),",",
                               tags$a(href= "https://cran.r-project.org/web/packages/glmnet/index.html", target= "_blank", "glmnet"), ",",
                  "and ", tags$a(href= "https://cran.r-project.org/web/packages/caret/", target= "_blank", "caret"),"package to select important features."))),
              column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                            radioButtons(inputId = "datafeatuerselfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                            radioButtons(inputId = "featuerselheader",label = "Header",choices = c("Yes", "No")),
                            fileInput("uploaded_featuersel","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                            actionButton("featuerselprepross","Process", width = "100%"),
                            )),
              column(8,tabBox(width = 100,
                              tabPanel(title = "LASSO", 
                                       fluidRow(
                                         column(12,
                                                tags$img(src="lasso.png", height = 300, width = "100%")))))),
              
              conditionalPanel(condition = "input.featuerselprepross && output.uploaded_featuersel_hide_tabpanel == false",
                               infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
            ), 
            conditionalPanel(condition = c("input.featuerselprepross && output.uploaded_featuersel_hide_tabpanel"), 
                             tabsetPanel(type = "tabs",
                                         tabPanel(title = "Table", icon = icon("table"),
                                                  box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                      DTOutput("featuerseldatashow"))),
                             tabPanel(title = "Methods", icon = icon("cog"),
                                      box(title = "Analyse",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                          column(3,box(title = "Method and variables",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                           selectInput(inputId = "featureselemethods", label = "Method", choices = c("Boruta", "Random forest", "LASSO", "Stepwise regression"), multiple = F),
                                           conditionalPanel(condition = "output.hide_boruta_panel",
                                           selectInput(inputId = "boruta_response", label = "Select response", choices = "", multiple = F),
                                           pickerInput(inputId = "boruta_predictors", label = "Select numeric predictors", choices = "", options = list(`actions-box`=TRUE), multiple = TRUE,width = "100%"),
                                           sliderInput(inputId = "boruta_pvalue", label = "P-value", min = 0.01,max = 0.1, value = 0.05, step = 0.01),
                                           selectInput(inputId = "boruta_mcAdj", label = "Adjustment (Bonferroni)", choices = c(FALSE, TRUE), multiple = F),
                                           numericInput(inputId = "boruta_maxrun", label = "Maximum runs", value = 100, min = 11, max = 1000, step = 1),
                                           actionButton(inputId = "boruta_start", label = "Start", width = "100%")),
                                           conditionalPanel(condition = "output.hide_featureselectionrandomforest_panel",
                                           selectInput(inputId = "featureselrandomforestresponse",label = "Select Response",choices = "", selected = NULL, multiple = FALSE),
                                           pickerInput(inputId = "featureselrandomforestpredictors_numeric", label = "Select numeric predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                           pickerInput(inputId = "featureselrandomforestpredictors_categorical", label = "Select categorical predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                           column(12, br(), br()),
                                           actionButton("featureselrandomforestaction","Start", width = "100%")
                                                            ),
                                           conditionalPanel(condition = "output.hide_featureselectionlasso_panel",
                                           selectInput(inputId = "lasso_response", label = "Select response", choices = "", multiple = F),
                                           pickerInput(inputId = "lasso_predictors", label = "Select numeric predictors", choices = "", options = list(`actions-box`=TRUE), multiple = TRUE,width = "100%"),
                                           sliderInput(inputId = "lassokfoldcv", label = "Select a k-fold cross validation",min = 3 , max = 20, value = 10,step = 1),
                                           actionButton("lassoaction","Start", width = "100%")
                                                            ),
                                           conditionalPanel(condition = "output.hide_stepwiseregression_panel",
                                           selectInput(inputId = "stepwisereg_response",label = "Select Response",choices = "", selected = NULL, multiple = FALSE),
                                           pickerInput(inputId = "stepwisereg_predictors", label = "Select predictors", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                           selectInput(inputId = "stepwisereg_method", label = "Method", choices = c("Backward selection", "Forward selection", "Stepwise selection"), multiple = F),
                                           sliderInput(inputId = "stepwiseregnvmax", label = "Maximal number of predictors", min = 1 , max = 5, value = 1,step = 1),
                                           sliderInput(inputId = "stepwiseregkfoldcv", label = "Select a k-fold cross validation",min = 1 , max = 20, value = 10,step = 1),
                                           actionButton("stepwiseaction","Start", width = "100%")
                                                            )
                                          )),
                                          conditionalPanel(condition = "output.hide_featureselectionlasso_panel",
                                          column(3,box(title = "Parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                          selectInput(inputId = "lasso_type", label = "Select type", choices = c("Classification", "Regression"), multiple = F),
                                          conditionalPanel(condition = "output.hide_lassotype_panel",
                                          selectInput(inputId = "lasso_classtype", label = "Class type", choices = c("Binomial", "Multinomial"), multiple = F)),
                                          selectInput(inputId = "lasso_type.measure", label = "Measure type", choices = "", multiple = F),
                                          conditionalPanel(condition = "output.hide_lassotype_panel",
                                          textInput("lassoweights", "Class weights", placeholder = "Comma separated values")),
                                          column(12,br(),br(),br()),
                                          conditionalPanel(condition = "output.hide_lassotype_panel  == false",
                                          column(12,br(),br(),br(), br(), br(), br(),br(), br()))
                                                 ))),
                                          #### lasso result
                                          conditionalPanel(condition = c("input.lassoaction && output.hide_featureselectionlasso_panel"),
                                          column(6,box(title = "LASSO important variables",status = "primary", collapsible = T, solidHeader = T, width = "100%", 
                                                       uiOutput("download_lasso_output_table"),
                                                       column(12, br()),
                                                       DTOutput("lasso_output_table")
                                                       )),
                                          
                                          column(12, box(title = "Plot", status = "primary", solidHeader = T, collapsible = T, width = "100%",
                                                         plotOutput("lassoplot", height = 700)
                                                         ))
                                                           ),
                                          
                                          
                                          
                                          conditionalPanel(condition = "output.hide_featureselectionrandomforest_panel",
                                          column(9, box(title = "Parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                          column(3,
                                          selectInput(inputId = "featureselrandomforesttype",label = "Random forest type", choices = c("Classification", "Regression"), multiple = F),
                                          numericInput(inputId = "featureselrandomforestntree",label = "Number of trees", 100, min = 1, max = 1000),
                                          conditionalPanel(condition = "output.hide_featureselrandomforest_classification",
                                          numericInput(inputId = "featureselrandomforestmtryclass",label = "Random samples", 0)),
                                          conditionalPanel(condition = "output.hide_featureselrandomforest_classification == false",
                                          numericInput(inputId = "featureselrandomforestmtryreg",label = "Random samples", 0)),
                                          numericInput(inputId = "featureselrandomforestsampsize",label = "Sample size", 0),
                                          pickerInput(inputId = "featureselrandomforeststrata",label = "Stratified sampling on:", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%", selected = FALSE)),
                                          column(3,
                                                 selectInput(inputId = "featureselrandomforestproximity",label = "Proximity",choices = c(FALSE, TRUE), multiple = FALSE),
                                                 selectInput(inputId = "featureselrandomforestoob.prox",label = "Proximity only for OOB",choices = c(FALSE, TRUE), multiple = FALSE),
                                                 
                                                 selectInput(inputId = "featureselrandomforestreplace",label = "Replace",choices = c(TRUE, FALSE), multiple = FALSE),
                                                 conditionalPanel(condition = "output.hide_featureselrandomforest_classification",
                                                 numericInput(inputId = "featureselrandomforestnodesizeclass",label = "Node size", value = 1, min = 1, max = 1000)),
                                                 conditionalPanel(condition = "output.hide_featureselrandomforest_classification == false",
                                                  numericInput(inputId = "featureselrandomforestnodesizereg",label = "Node size", value = 5, min = 1, max = 1000)),
                                                 numericInput(inputId = "featureselrandomforestmaxnodes",label = "Maximum nodes",value = 5, min = 1, max = 1000)),
                                          column(3,
                                                 conditionalPanel(condition = "output.hide_featureselrandomforest_classification",
                                                 textInput("featureselrandomforestcutoff", "Cutoff", placeholder = "Comma separated proportions")),
                                                 conditionalPanel(condition = "output.hide_featureselrandomforest_classification",
                                                 textInput("featureselrandomforestclasswt", "Class weights", placeholder = "Comma separated values")),
                                                 numericInput(inputId = "featureselrandomforestnPerm",label = "nPerm", value = 1, min = 1, max = 100),
                                                 selectInput(inputId = "featureselrandomforestimportance",label = "Importance",choices = c(TRUE, FALSE), multiple = FALSE),
                                                 selectInput(inputId = "featureselrandomforestlocalImp",label = "Local importance",choices = c(FALSE, TRUE), multiple = FALSE),
                                          ),
                                          column(3,
                                                 conditionalPanel(condition = "output.hide_featureselrandomforest_classification",
                                                  selectInput(inputId = "featureselrandomforestnorm.votes",label = "Express final votes",choices = c(TRUE, FALSE), multiple = FALSE)),
                                                 selectInput(inputId = "featureselrandomforestdo.trace",label = "Do trace",choices = FALSE, multiple = FALSE),
                                                 selectInput(inputId = "featureselrandomforestkeep.forest",label = "Keep forest",choices = TRUE, multiple = FALSE),
                                                 conditionalPanel(condition = "output.hide_featureselrandomforest_classification == false",
                                                 selectInput(inputId = "featureselrandomforestcorr.bias",label = "Bias correction",choices = c(FALSE, TRUE), multiple = FALSE)),
                                                 selectInput(inputId = "featureselrandomforestkeep.inbag",label = "Keep inbag",choices = FALSE, multiple = FALSE)),
                                                        ))),
                                          
                                          ###### boruta result
                                          conditionalPanel(condition = c("input.boruta_start && output.hide_boruta_panel"),
                                          column(9, box(title = "Boruta result", status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                 column(6, verbatimTextOutput("boruta_output")),
                                                 column(6, uiOutput("download_boruta_result"), column(12, br()),
                                                 DTOutput("boruta_output1")),
                                                ))),
                                          ########## stepwise result 
                                          conditionalPanel(condition = c("input.stepwiseaction && output.hide_stepwiseregression_panel"),
                                          column(9, box(title = "Stepwise regression result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                          column(12, box(title = "Model result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                        DTOutput("stepwisereg_ouput_result"))),
                                          column(6, box(title = "Best predictors",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                        column(12,uiOutput("download_stepwise_bestpredictors_table")),
                                                        column(12,br()),
                                                        DTOutput("stepwisereg_output_predictors"))),
                                          column(6, box(title = "Coefficient of best predictors",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                        column(12,uiOutput("download_stepwise_bestpredictors_coef_table")),
                                                        column(12,br()),
                                                        DTOutput("stepwisereg_output_predictors_coef")))
                                                           ))),
                                          ############### random forest result
                                          column(12, conditionalPanel(condition = c("input.featureselrandomforestaction && output.hide_featureselectionrandomforest_panel"),
                                                                    box(title = "Randomforest result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                        column(12,uiOutput("download_featuerselection_randomforest")),
                                                                        column(12, br()),
                                                                        column(12,DTOutput("featureselrandomforestoutput"))))),
                                          
                                          
                                          
                                         
                                          
                                          )
                             )
            )
            )
            ),
    ################################################### Dimentionality reduction ################################
    tabItem(tabName = "dimentionalityreduction",
                                 fluidRow(
                                   column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                                                   
                                                  infoBox(title = "principal component analysis (PCA)", value = 2, icon = icon("check-circle"), color = "aqua", width = 4, fill = T),
                                                  infoBox(title = "Linear Discriminant Analysis (LDA)", value = 3, icon = icon("check-circle"), color = "blue", width = 4, fill = T),
                                                  p(style="text-align: justify;","Dimensionality reduction aimes to turn a set of 
                                                    variables into a smaller number of variables that retain as much of the
                                                    original multidimensional information as possible. 
                                                    DeepBC uses", tags$a(href= "https://cran.r-project.org/web/packages/psych/index.html", target= "_blank", "pysch")," 
                                                    and ", tags$a(href= "https://cran.r-project.org/web/packages/MASS/index.html", target= "_blank", "MASS"),"package to perform PCA and LDA techniques."))),
                                   column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                 radioButtons(inputId = "datadimredfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                                 radioButtons(inputId = "dimredheader",label = "Header",choices = c("Yes", "No")),
                                                 fileInput("uploaded_dimred","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                                                 actionButton("dimredprepross","Process", width = "100%"),
                                                  )),
                                   column(8,tabBox(width = 100,
                                                   tabPanel(title = "PCA", 
                                                            fluidRow(
                                                              column(12,
                                                                     tags$img(src="dimred_pca.png", height = 300, width = "100%")))),
                                                   tabPanel(title = "LDA pair plot", 
                                                            fluidRow(
                                                              column(12,
                                                                     tags$img(src="dimred_lda_cor.png", height = 300, width = "100%")))),
                                                   tabPanel(title = "LDA bar plot", 
                                                            fluidRow(
                                                              column(12,
                                                                     tags$img(src="dimred_lda_bar.png", height = 300, width = "100%")))),
                                                   tabPanel(title = "LDA biplot", 
                                                            fluidRow(
                                                              column(12,
                                                                     tags$img(src="dimred_biplot.png", height = 300, width = "100%")))),
                                                   
                                                   )),
                                   conditionalPanel(condition = "input.dimredprepross && output.uploaded_dimred_hide_tabpanel == false",
                                                    infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                                 ), 
                                 conditionalPanel(condition = c("input.dimredprepross && output.uploaded_dimred_hide_tabpanel"), 
                                                  tabsetPanel(type = "tabs",
                                                              tabPanel(title = "Table", icon = icon("table"),
                                                                       box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                           DTOutput("dimreddatashow"))),
                                                              tabPanel(title = "Model", icon = icon("cogs", lib = "font-awesome"),
                                                                       box(title = "Dimentionality reduction",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                           column(3,box(title = "Parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                                        selectInput(inputId = "dimred_method", label = "Method", choices = c("PCA", "LDA"), multiple = F),
                                                                                        conditionalPanel(condition = "output.hide_dimred_pca",
                                                                                        pickerInput(inputId = "dimred_pca_variables", label = "Select numeric predictors", choices = "", options = list(`actions-box`=TRUE), multiple = TRUE,width = "100%"),
                                                                                        
                                                                                        selectInput(inputId = "dimred_pca_cor", label = "Select one", choices = c("Correlation matrix", "Covariance matrix"), multiple = F),
                                                                                        sliderInput(inputId = "dimred_pcacomponents", label = "Number of components to keep", min = 1, max = 10, value = 2 , step = 1),
                                                                                        actionButton(inputId = "dimred_pca_action", label = "Analyse", width = "100%")),
                                                                                        
                                                                                        conditionalPanel(condition = "output.hide_dimred_pca == false",
                                                                                        selectInput(inputId = "dimred_lda_response", label = "Select response", choices = "", multiple = F),
                                                                                        pickerInput(inputId = "dimred_lda_predictors", label = "Select numeric predictors", choices = "", options = list(`actions-box`=TRUE), multiple = TRUE,width = "100%"),
                                                                                        sliderInput(inputId = "dimreg_lda_ratiosplit", label = "Select training split ratio",min = 0.05, max = 0.95, value = 0.7,step = 0.05),
                                                                                        sliderInput(inputId = "dimreg_lda_kfoldcv", label = "Select a k-fold cross validation",min = 0 , max = 20, value = 10,step = 5),
                                                                                        selectInput(inputId = "dimred_lda_method", label = "LDA method", choices = c("moment", "mle", "mve"), multiple = F),
                                                                                        actionButton(inputId = "dimred_lda_action", label = "Analyse", width = "100%"),
                                                                                        column(12,br()))
                                                                                        )),
                                                                           conditionalPanel(condition = c("input.dimred_pca_action && output.hide_dimred_pca"),
                                                                           column(9, box(title = "Components distribution",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                                         plotOutput("pca_scree_plot", height = 357))),
                                                                           column(12,box(title = "Computed components",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                                         uiOutput("download_pcacomponents_table"),
                                                                                         column(12, br()),
                                                                                         DTOutput("pca_components_table")))),
                                                                           conditionalPanel(condition = c("input.dimred_lda_action && output.hide_dimred_pca == false"),
                                                                           column(9, box(title = "Pair plot",status = "primary",solidHeader = T,collapsible = T, width = "100%",
                                                                                        plotOutput("dimred_lda_pairplot") 
                                                                                         ))),
                                                                           conditionalPanel(condition = c("input.dimred_lda_action && output.hide_dimred_pca == false"),
                                                                           column(9,box(title = "Performance", status = "primary", solidHeader = T, collapsible = T, width = "100%",
                                                                                         DTOutput("dimred_ldaoutput"))))),
                                                                       conditionalPanel(condition = c("input.dimred_lda_action && output.hide_dimred_pca == false"),
                                                                           box(title = "Model evaluation plots", status = "primary", solidHeader = T, collapsible = F, width = "100%", height = 900,
                                                                                         column(3),
                                                                                         column(3, selectInput(inputId = "dimred_lda_histplot_choices", label = "Select one", choices = "", multiple = F)),
                                                                                         column(3, selectInput(inputId = "dimred_lda_histplot_dataset", label = "Select one", choices = c("Trianing set", "Test set"), multiple = F)),
                                                                                         column(3),
                                                                                         column(5),
                                                                                         column(2, actionButton(inputId = "dimred_lda_hitplotview", label = "Display plot", width = "100%")),
                                                                                         column(5),
                                                                                         column(12,column(2),column(8,plotOutput("dimreg_lda_histplot", width = "100%", height = 700),column(2)))),
                                                                                         box(title = "Biplot",status = "primary", solidHeader = T, collapsible = F, width = "100%",height = 700,
                                                                                         #column(12,
                                                                                         column(3),
                                                                                         column(3, selectInput(inputId = "dimred_lda_ldas_choicex", label = "Select x", choices = "", multiple = F)),
                                                                                         column(3, selectInput(inputId = "dimred_lda_ldas_choicey", label = "Select y", choices = c("Trianing set", "Test set"), multiple = F)),
                                                                                         column(3),
                                                                                         column(5),
                                                                                         column(2, actionButton(inputId = "dimred_lda_ldas_plotview", label = "Display plot", width = "100%")),
                                                                                         column(5),
                                                                                         column(12, br()),
                                                                                         plotOutput("dimreg_lda_ldas_plot"),
                                                                               column(12, br(),br(),br(),br()),
                                                                               column(12, br(),br(),br(),br()),
                                                                               column(12, br(),br()),
                                                                               column(12,column(4),column(4,actionButton(inputId = "dimred_lda_predictiondata", label = "Prediction on the dataset", width = "100%")),column(4)),
                                                                               
                                                                               )),
                                                                       conditionalPanel(condition = c("input.dimred_lda_predictiondata && output.hide_dimred_pca == false"),
                                                                           box(title = "Prediction on the dataset", status = "primary", solidHeader = T, collapsible = F, width = "100%",
                                                                               uiOutput("download_lda_table"),
                                                                               column(12, br()),
                                                                           DTOutput("dimred_lda_prediction")))
                                                                           )
                                 
                                 ))
            ),
    ############################################### ML-Regression-Simple ##############################
    tabItem(
      tabName = "regression",
      tabsetPanel(type = "tabs",
        tabPanel(title = "Simple",
                 fluidRow(
                   column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                                  infoBox(title = "simple linear regression", value = 4, icon = icon("slash"), color = "green", width = 4, fill = T),
                                  p(style="text-align: justify;","Simple linear regression is a supervised machine learning algorithm and performs a regression task. 
                                    It tries to predict the behavior of a target variable based on an independent variable. 
                                    DeepBC uses the lm function in stats package to perform simple linear regression algorithm."))),
                   column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                 radioButtons(inputId = "dataslregfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                 radioButtons(inputId = "slregheader",label = "Header",choices = c("Yes", "No")),
                                 fileInput("uploaded_slreg","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                                 actionButton("slregprepross","Process", width = "100%"))),
                   column(8,tabBox(width = 100,
                                   tabPanel(title = "Model's plot",
                                            fluidRow(
                                              column(12,
                                                     tags$img(src="simplelinearregression.png", height = 300, width = "100%")))))),
                   conditionalPanel(condition = "input.slregprepross && output.uploaded_slreg_hide_tabpanel == false",
                                      infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                 ),
                 conditionalPanel(condition = c("input.slregprepross && output.uploaded_slreg_hide_tabpanel"),
                                  tabsetPanel(type = "tabs",
                                              tabPanel(title = "Table", icon = icon("table"),
                                                       box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                           DTOutput("slregdatashow"))),
                                              tabPanel(title = "Model", icon = icon("cogs", lib = "font-awesome"),
                                                       box(title = "Model design and result",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                           column(3,box(title = "Training and test set",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                        selectInput(inputId = "slregresponse",label = "Select Response",choices = "", selected = NULL, multiple = FALSE),
                                                                        selectInput(inputId = "slregpredictor", label = "Select predictor", choices = "",selected = NULL, multiple = FALSE),
                                                                        sliderInput(inputId = "slregratiosplit", label = "Select training split ratio",min = 0.05, max = 0.95, value = 0.7,step = 0.05),
                                                                        sliderInput(inputId = "slregkfoldcv", label = "Select a k-fold cross validation",min = 0 , max = 20, value = 10,step = 5)),
                                                                        actionButton("slregaction","Build model", width = "100%")),

                                                           column(6,conditionalPanel(condition = "input.slregaction",
                                                                                     box(title = "Result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                                         column(12,DTOutput("slregoutput"), htmlOutput("summarymlregs"))))),
                                                       )
                                              ),
                                              tabPanel(title = "Plot", icon = icon("chart-line"),
                                                       conditionalPanel(condition = "input.slregaction",
                                                                        box(title = "Plot",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                            column(12,
                                                                                   fluidRow(
                                                                                     column(12,
                                                                                            column(5),
                                                                                            column(5)),
                                                                                            column(12,
                                                                                            column(5),
                                                                                            column(2,actionButton(inputId = "slregplotview", label = "Display plots", width = "100%")),
                                                                                            column(5)))),

                                                                            column(12, br(), plotOutput("slregplottraining")),
                                                                            column(12, br(),br()),
                                                                            column(12, plotOutput("slregplottest"))))),
                                              tabPanel(title = "Prediction", icon = icon("binoculars"),
                                                       conditionalPanel(condition = "input.slregaction",
                                                                        box(title = "Prediction on new data based on the designed model", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                            column(3,box(title = "Upload new data",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                                                         radioButtons(inputId = "dataslregfileformat_prediction",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                                                                         radioButtons(inputId = "slregheader_prediction",label = "Header",choices = c("Yes", "No")),
                                                                                         fileInput("uploaded_slreg_prediction","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%"),
                                                                                         selectInput(inputId = "slreg_prediction_response",label = "Response for comparison (if any)",choices = NULL, selected = FALSE, multiple = FALSE, selectize = FALSE, size = 2),
                                                                                         pickerInput(inputId = "slreg_prediction_predictor", label = "Select predictor", choices = "", options = list(`actions-box`=TRUE), multiple = FALSE,width = "100%"),
                                                                                         actionButton("slregprepross_prediction_result", "Predict",width = "100%"),

                                                                            column(12,br()))),
                                                                            conditionalPanel(condition = "input.slregprepross_prediction_result",
                                                                                             column(9,box(title = "Prediction result", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                                          column(6,box(title = "Actual", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("slreg_actual_response"))),
                                                                                                          column(6,box(title = "Predicted", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("slreg_predicted_response"))),
                                                                                                          column(12,column(7,),column(5,uiOutput("download_prediction_slreg_table")))
                                                                                             )))))
                                              )
                                              ),
                                  conditionalPanel(condition = c("output.uploaded_slregdata_prediction_hide_tabpanel == false"),
                                                   infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                                  )),
        ############################################### ML-Regression-Multiple ############################################################
        tabPanel(title = "Multiple",
                 fluidRow(
                   column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                                  infoBox(title = "multiple linear regression", value = 5, icon = icon("slash"), color = "olive", width = 4, fill = T),
                                  p(style="text-align: justify;","In multiple linear regression, unlike simple regression, the linear relationship between two or more independent variables is modeled to forecast the dependent variable. 
                                    Again, the lm function is using to construct a multiple linear regression model and",tags$a(href= "https://cran.r-project.org/web/packages/car/index.html", target= "_blank", " car")," package to draw the model's plot."))),
                   column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                 radioButtons(inputId = "datamlregfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                 radioButtons(inputId = "mlregheader",label = "Header",choices = c("Yes", "No")),
                                 fileInput("uploaded_mlreg","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                                 actionButton("mlregprepross","Process", width = "100%"))),
                   column(8,tabBox(width = 100,
                                   tabPanel(title = "Model's plots",
                                            fluidRow(
                                              column(12,
                                                     tags$img(src="mlreg.png", height = 300, width = "100%")))))),
                   conditionalPanel(condition = c("input.mlregprepross && output.uploaded_mlreg_hide_tabpanel == false"),
                                    infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                 ),

                 conditionalPanel(condition = c("input.mlregprepross && output.uploaded_mlreg_hide_tabpanel"),
                                  tabsetPanel(type = "tabs",
                                              tabPanel(title = "Table", icon = icon("table"),
                                                       box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                           DTOutput("mlregdatashow"))),
                                              tabPanel(title = "Model", icon = icon("cogs", lib = "font-awesome"),
                                                       box(title = "Model design and result",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                           column(3,box(title = "Training and test set",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                        selectInput(inputId = "mlregresponse",label = "Select response",choices = "", selected = NULL, multiple = FALSE),
                                                                        pickerInput(inputId = "mlregpredictors_numeric", label = "Select numeric predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                        pickerInput(inputId = "mlregpredictors_categorical", label = "Select categorical predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                        sliderInput(inputId = "mlregratiosplit", label = "Select training split ratio",min = 0.05, max = 0.95, value = 0.7,step = 0.05),
                                                                        sliderInput(inputId = "mlregkfoldcv", label = "Select a k-fold cross validation",min = 0 , max = 20, value = 10,step = 5)),
                                                                        actionButton("mlregaction","Build model", width = "100%")),

                                                           column(9,
                                                                  column(12,
                                                                         column(1),
                                                                         column(10,
                                                                                conditionalPanel(condition = "input.mlregaction",
                                                                                                 box(title = "Result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                                                     DTOutput("mlregoutput")))),column(1))),

                                                           )),
                                                           tabPanel(title = "Plot", icon = icon("chart-line"),
                                                                    conditionalPanel(condition = "input.mlregaction",
                                                                                     box(title = "Plot",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                                         column(12,
                                                                                                fluidRow(
                                                                                                  column(12,
                                                                                                         column(5),
                                                                                                         column(2,actionButton(inputId = "mlregplotview", label = "Display plots", width = "100%")),
                                                                                                         column(5)))),

                                                                                         column(12, br(), plotOutput("mlregplottraining", height = 600))))),
                                                           tabPanel(title = "Prediction", icon = icon("binoculars"),
                                                                    conditionalPanel(condition = "input.mlregaction",
                                                                                     box(title = "Prediction on new data based on the designed model", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                         column(3,box(title = "Upload new data",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                                                                      radioButtons(inputId = "datamlregfileformat_prediction",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                                                                                      radioButtons(inputId = "mlregheader_prediction",label = "Header",choices = c("Yes", "No")),
                                                                                                      fileInput("uploaded_mlreg_prediction","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%"),
                                                                                                      selectInput(inputId = "mlreg_prediction_response",label = "Response for comparison (if any)",choices = NULL, selected = FALSE, multiple = FALSE, selectize = FALSE, size = 2),
                                                                                                      pickerInput(inputId = "mlreg_prediction_predictor_numeric", label = "Select numeric predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = TRUE,width = "100%"),
                                                                                                      pickerInput(inputId = "mlreg_prediction_predictor_categorical", label = "Select categorical predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = TRUE,width = "100%"),
                                                                                                      actionButton("mlregprepross_prediction_result", "Predict",width = "100%"),
                                                                                         column(12,br()))),
                                                                                         conditionalPanel(condition = "input.mlregprepross_prediction_result",
                                                                                                          column(9,box(title = "Prediction result", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                                                       column(6,box(title = "Actual", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("mlreg_actual_response"))),
                                                                                                                       column(6,box(title = "Predicted", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("mlreg_predicted_response"))),
                                                                                                                       column(12,column(7,),column(5,uiOutput("download_prediction_mlreg_table")))
                                                                                                          )))))
                                                           )
                                                           ),
                                  conditionalPanel(condition = "output.uploaded_mlregdata_prediction_hide_tabpanel == false",
                                                   infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                                              )

                 ),
        ############################################### ML-Regression-Polynomial ####################################################################################################################################################
        tabPanel(title = "Polynomial",
                 fluidRow(
                   column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                                  column(4, box(background = "lime", width = "50%", height = 89,
                                                column(3,tags$div(style = "margin-top: 1px;margin-left: -16px;font-size: 20px", tags$img(src= "poly.png", height ="60", width ="60"))),
                                                column(9,p("POLYNOMIAL REGRESSION"),
                                                       tags$b(style="font-size: 20px;","6"))
                                  )),
                                  p(style="text-align: justify;","Polynomial regression is a form of linear regression; however, the algorithm tries to find a nonlinear relationship between an independent variable and a dependent variable by transforming the independent variable in the nth degree of the polynomial."))),
                   column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                 radioButtons(inputId = "datapolyregfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                 radioButtons(inputId = "polyregheader",label = "Header",choices = c("Yes", "No")),
                                 fileInput("uploaded_polyreg","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                                 actionButton("polyregprepross","Process", width = "100%"))),
                   column(8,tabBox(width = 100,
                                   tabPanel(title = "Model's plot",
                                            fluidRow(
                                              column(12,
                                                     tags$img(src="polynomialregression.png", height = 300, width = "100%")))))),
                   conditionalPanel(condition = "input.polyregprepross && output.uploaded_polyreg_hide_tabpanel == false",
                                    infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                 ),
                 conditionalPanel(condition = c("input.polyregprepross && output.uploaded_polyreg_hide_tabpanel"),
                                  tabsetPanel(type = "tabs",
                                              tabPanel(title = "Table", icon = icon("table"),
                                               box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                   DTOutput("polyregdatashow"))),
                                              tabPanel(title = "Model", icon = icon("cogs", lib = "font-awesome"),
                                                       box(title = "Model design and result",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                           column(3,box(title = "Training and test set",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                        selectInput(inputId = "polyregresponse",label = "Select Response",choices = "", selected = NULL, multiple = FALSE),
                                                                        selectInput(inputId = "polyregpredictor", label = "Select predictor", choices = "",selected = NULL, multiple = FALSE),
                                                                        sliderInput(inputId = "polyregratiosplit", label = "Select training split ratio",min = 0.05, max = 0.95, value = 0.7,step = 0.05),
                                                                        sliderInput(inputId = "polyregkfoldcv", label = "Select a k-fold cross validation",min = 0 , max = 20, value = 10,step = 5)),
                                                                  actionButton("polyregaction","Build model", width = "100%")),
                                                           column(3,box(title = "Parameter",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                  sliderInput(inputId = "polyregdegree", label = "Select a degree",min = 1, max = 10, value = 2,step = 1))),

                                                           column(6,conditionalPanel(condition = "input.polyregaction",
                                                                                     box(title = "Result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                                         column(12,DTOutput("polyregoutput"))))))),
                                                           tabPanel(title = "Plot", icon = icon("chart-line"),
                                                                    conditionalPanel(condition = "input.polyregaction",
                                                                                     box(title = "Plot",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                                         column(12,
                                                                                                fluidRow(
                                                                                                  column(12,
                                                                                                         column(5),
                                                                                                         column(5)),
                                                                                                  column(12,
                                                                                                         column(5),
                                                                                                         column(2,actionButton(inputId = "polyregplotview", label = "Display plots", width = "100%")),
                                                                                                         column(5)))),

                                                                                         column(12, br(), plotOutput("polyregplottraining")),
                                                                                         column(12, br(),br()),
                                                                                         column(12, plotOutput("polyregplottest"))))),
                                                           tabPanel(title = "Prediction", icon = icon("binoculars"),
                                                                    conditionalPanel(condition = "input.polyregaction",
                                                                                     box(title = "Prediction on new data based on the designed model", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                         column(3,box(title = "Upload new data",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                                                                      radioButtons(inputId = "datapolyregfileformat_prediction",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                                                                                      radioButtons(inputId = "polyregheader_prediction",label = "Header",choices = c("Yes", "No")),
                                                                                                      fileInput("uploaded_polyreg_prediction","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%"),
                                                                                                      selectInput(inputId = "polyreg_prediction_response",label = "Response for comparison (if any)",choices = NULL, selected = FALSE, multiple = FALSE, selectize = FALSE, size = 2),
                                                                                                      pickerInput(inputId = "polyreg_prediction_predictor", label = "Select predictor", choices = "", options = list(`actions-box`=TRUE), multiple = FALSE,width = "100%"),
                                                                                                      actionButton("polyregprepross_prediction_result", "Predict",width = "100%"),

                                                                                                      column(12,br()))),
                                                                                         conditionalPanel(condition = "input.polyregprepross_prediction_result",
                                                                                                          column(9,box(title = "Prediction result", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                                                       column(6,box(title = "Actual", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("polyreg_actual_response"))),
                                                                                                                       column(6,box(title = "Predicted", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("polyreg_predicted_response"))),
                                                                                                                       column(12,column(7,),column(5,uiOutput("download_prediction_polyreg_table")))
                                                                                                          ))))))
                                                       ),
                                  conditionalPanel(condition = c("output.uploaded_polyregdata_prediction_hide_tabpanel == false"),
                                                   infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                 )),
        ############################################### ML-Regression-Logistic ##############################
         tabPanel(title = "Logistic",
                 fluidRow(
                   column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                                  column(4, box(background = "orange", width = "50%", height = 89,
                                      column(3,tags$div(style = "margin-top: 1px;margin-left: -16px;font-size: 20px",tags$img(src= "logistic-regression.svg", style="text-align: right;", height ="70", width ="70"))),
                                      column(9,p("LOGISTIC REGRESSION"),
                                             tags$b(style="font-size: 20px;","7"))
                                      )),

                                  p(style="text-align: justify;","Logistic regression is a supervised classification algorithm aiming to predict the probability of a target variable, which has only two possible outcomes. By defining a threshold, probabilities can turn into classes.
                                    DeepBC uses glm function to perform logistic regression algorithm,",tags$a(href= "https://cran.r-project.org/web/packages/pROC/index.html", target= "_blank", " pROC"), " and ",
                                    tags$a(href= "https://cran.r-project.org/web/packages/MLmetrics/index.html", target= "_blank", "MLmetrics"), "for model evaluation, and ",
                                    tags$a(href= "https://cran.r-project.org/web/packages/RColorBrewer/index.html", target= "_blank", "RColorBrewer"), "and ",
                                    tags$a(href= "https://github.com/cran/ElemStatLearn", target= "_blank", "ElemStatLearn"), " for drawing model's plots. DeepBC uses the later two packages wherever the high-quality plot is needed, similar to this section"))),
                   column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                 radioButtons(inputId = "datalogitregfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                 radioButtons(inputId = "logitregheader",label = "Header",choices = c("Yes", "No")),
                                 fileInput("uploaded_logitreg","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                                 actionButton("logitregprepross","Process", width = "100%"))),
                   column(8,tabBox(width = 100,
                                   tabPanel(title = "Model's plot",
                                            fluidRow(
                                              column(12,
                                                     tags$img(src="logisticregression.png", height = 300, width = "100%")))))),

                   conditionalPanel(condition = c("input.logitregprepross && output.uploaded_logitreg_hide_tabpanel == false"),
                                    infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                 ),
            conditionalPanel(condition = c("input.logitregprepross && output.uploaded_logitreg_hide_tabpanel"),
                                  tabsetPanel(type = "tabs",
                                              tabPanel(title = "Table", icon = icon("table"),
                                                       box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                           DTOutput("logitregdatashow"))),
                                              tabPanel(title = "Model", icon = icon("cogs", lib = "font-awesome"),
                                                       box(title = "Model design and result",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                           column(3,box(title = "Training and test set",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                        selectInput(inputId = "logitregresponse",label = "Select response",choices = "", selected = NULL, multiple = FALSE),
                                                                        pickerInput(inputId = "logitregpredictors_numeric", label = "Select numeric predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                        pickerInput(inputId = "logitregpredictors_categorical", label = "Select categorical predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                        sliderInput(inputId = "logitregratiosplit", label = "Select training split ratio",min = 0.05, max = 0.95, value = 0.7,step = 0.05),
                                                                        sliderInput(inputId = "logitregkfoldcv", label = "Select a k-fold cross validation",min = 0 , max = 20, value = 10,step = 5)),
                                                                  ),
                                                           column(3,box(title = "Parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                        textInput("logitregweights", "Weights", placeholder = "Comma separated values"),
                                                                        sliderInput(inputId = "logitregprob", label = "Probability threshold",min = 0.5, max = 0.99, value = 0.5,step = 0.01)),
                                                                  actionButton("logitregaction","Build model", width = "100%")),
                                                           column(6,conditionalPanel(condition = "input.logitregaction",
                                                                                     box(title = "Result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                                         DTOutput("logitregoutput")))),

                                                       )),
                                              tabPanel(title = "Plot", icon = icon("chart-line"),
                                                       conditionalPanel(condition = "input.logitregaction",
                                                                        box(title = "Plot",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                            column(12,
                                                                                   fluidRow(
                                                                                     conditionalPanel(condition = "output.hide_logitreg_choices_pca == false",
                                                                                     column(12,
                                                                                            column(3),
                                                                                            column(3, selectInput(inputId = "logitregplotx", label = "Select x", choices = NULL, multiple = F, selected = NULL)),
                                                                                            column(3, selectInput(inputId = "logitregploty", label = "Select y", choices = NULL, multiple = F, selected = NULL)),
                                                                                            column(3))),
                                                                                     column(12,
                                                                                            column(5),
                                                                                            column(2,actionButton(inputId = "logitregplotview", label = "Display plots", width = "100%")),
                                                                                            column(5))),
                                                                                   column(12, br(),
                                                                                           column(4,),
                                                                                          column(4,shinyjs::hidden(p(id = "logitregplotwait", tags$b("Plots are loading ..., please be patient. It taks a while."))),),
                                                                                          column(4)
                                                                                   )
                                                                                   ),

                                                                            column(12, br(), plotOutput("logitregplottraining")),
                                                                            column(12, br(),br()),
                                                                            column(12, plotOutput("logitregplottest"))))),
                                              tabPanel(title = "Prediction", icon = icon("binoculars"),
                                                       conditionalPanel(condition = "input.logitregaction",
                                                                        box(title = "Prediction on new data based on the designed model", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                            column(3,box(title = "Upload new data",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                                                         radioButtons(inputId = "datalogitregfileformat_prediction",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                                                                         radioButtons(inputId = "logitregheader_prediction",label = "Header",choices = c("Yes", "No")),
                                                                                         fileInput("uploaded_logitreg_prediction","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%"),
                                                                                         selectInput(inputId = "logitreg_prediction_response",label = "Response for comparison (if any)",choices = NULL, selected = FALSE, multiple = FALSE, selectize = FALSE, size = 2),
                                                                                         pickerInput(inputId = "logitreg_prediction_predictor_numeric", label = "Select numeric predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = TRUE,width = "100%"),
                                                                                         pickerInput(inputId = "logitreg_prediction_predictor_categorical", label = "Select categorical predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = TRUE,width = "100%"),
                                                                                         actionButton("logitregprepross_prediction_result", "Predict",width = "100%"),
                                                                                         column(12,br()))),
                                                                            conditionalPanel(condition = "input.logitregprepross_prediction_result",
                                                                                             column(9,box(title = "Prediction result", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                                          column(6,box(title = "Actual", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("logitreg_actual_response"))),
                                                                                                          column(6,box(title = "Predicted", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("logitreg_predicted_response"))),
                                                                                                          column(12,column(7,),column(5,uiOutput("download_prediction_logitreg_table")))
                                                                                             )))))
                                              )

                                   ),
                             conditionalPanel(condition = "output.uploaded_logitregdata_prediction_hide_tabpanel == false",
                                              infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                 )

        )
    )
    ),
 ######################################### Random forest (classification - regression) ####################################
 tabItem(
   tabName = "randomforest",
                        fluidRow(
                          column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                                         column(4, box(background = "fuchsia", width = "50%", height = 89,
                                                       column(3,tags$div(style = "margin-top: 1px;margin-left: -16px;font-size: 20px", tags$img(src= "trees.png", height ="70", width ="70"))),
                                                       column(9,p("RANDOM FOREST"),
                                                              tags$b(style="font-size: 20px;","8"))
                                         )),
                                         p(style="text-align: justify;","Random forest is a supervised machine learning algorithm that is based on the decision tree algorithm. It takes the advantage of a collection of decision trees in which each tree trains with a random subset of data; therefore, its performance will increase.
                    DeepBC uses",tags$a(href= "https://cran.r-project.org/web/packages/randomForest/index.html", target= "_blank", " randomForest") ," package to perform random forest algorithm."))),
                          column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                        radioButtons(inputId = "datarandomforestfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                        radioButtons(inputId = "randomforestheader",label = "Header",choices = c("Yes", "No")),
                                        fileInput("uploaded_randomforest","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                                        actionButton("randomforestprepross","Process", width = "100%"))),
                          column(8,tabBox(width = 100,
                                          tabPanel(title = "Model's plot", 
                                                   fluidRow(
                                                     column(12,
                                                            tags$img(src="randomforest.png", height = 300, width = "100%")))))),
                          conditionalPanel(condition = "input.randomforestprepross && output.uploaded_randomforestdata_hide_tabpanel == false",
                                           infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                        ),
                        conditionalPanel(condition = c("input.randomforestprepross && output.uploaded_randomforestdata_hide_tabpanel"),
                                         tabsetPanel(type = "tabs",
                                                     tabPanel(title = "Table", icon = icon("table"), 
                                                              box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                  DTOutput("randomforestdatashow"))),
                                                     tabPanel(title = "Model", icon = icon("cogs", lib = "font-awesome"), 
                                                              box(title = "Model design and result",status = "primary", collapsible = T, solidHeader = T, width = "100%", 
                                                                  column(3,box(title = "Training and test set",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                               selectInput(inputId = "randomforestresponse",label = "Select Response",choices = "", selected = NULL, multiple = FALSE),
                                                                               pickerInput(inputId = "randomforestpredictors_numeric", label = "Select numeric predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                               pickerInput(inputId = "randomforestpredictors_categorical", label = "Select categorical predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                               
                                                                               sliderInput(inputId = "randomforestratiosplit", label = "Select training split ratio",min = 0.05, max = 0.95, value = 0.7,step = 0.05),
                                                                               sliderInput(inputId = "randomforestkfoldcv", label = "Select a k-fold cross validation",min = 0 , max = 20, value = 10,step = 5))),
                                                                  column(9, box(title = "Parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                                column(3,
                                                                                selectInput(inputId = "randomforesttype",label = "Random forest type", choices = c("Classification", "Regression"), multiple = F),
                                                                                
                                                                                numericInput(inputId = "randomforestntree",label = "Number of trees", 100, min = 1, max = 1000),
                                                                                conditionalPanel(condition = "output.hide_randomforest_classification",
                                                                                numericInput(inputId = "randomforestmtryclass",label = "Random samples", 0)),
                                                                                conditionalPanel(condition = "output.hide_randomforest_classification == false",
                                                                                numericInput(inputId = "randomforestmtryreg",label = "Random samples", 0)),
                                                                                numericInput(inputId = "randomforestsampsize",label = "Sample size", 0),
                                                                                pickerInput(inputId = "randomforeststrata",label = "Stratified sampling on:", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%", selected = FALSE),
                                                                                ),
                                                                                column(3,
                                                                                selectInput(inputId = "randomforestproximity",label = "Proximity",choices = c(FALSE, TRUE), multiple = FALSE),
                                                                                selectInput(inputId = "randomforestoob.prox",label = "Proximity only for OOB",choices = c(FALSE, TRUE), multiple = FALSE),
                                                                               
                                                                                selectInput(inputId = "randomforestreplace",label = "Replace",choices = c(TRUE, FALSE), multiple = FALSE),
                                                                                conditionalPanel(condition = "output.hide_randomforest_classification",
                                                                                numericInput(inputId = "randomforestnodesizeclass",label = "Node size", value = 1, min = 1, max = 1000)),
                                                                                conditionalPanel(condition = "output.hide_randomforest_classification == false",
                                                                                numericInput(inputId = "randomforestnodesizereg",label = "Node size", value = 5, min = 1, max = 1000)),
                                                                                numericInput(inputId = "randomforestmaxnodes",label = "Maximum nodes",value = 5, min = 1, max = 1000)),
                                                                                column(3,
                                                                                conditionalPanel(condition = "output.hide_randomforest_classification",
                                                                                textInput("randomforestcutoff", "Cutoff", placeholder = "Comma separated proportions")),
                                                                                conditionalPanel(condition = "output.hide_randomforest_classification",
                                                                                textInput("randomforestclasswt", "Class weights", placeholder = "Comma separated values")),
                                                                                numericInput(inputId = "randomforestnPerm",label = "nPerm", value = 1, min = 1, max = 1),
                                                                                selectInput(inputId = "randomforestimportance",label = "Importance",choices = FALSE, multiple = FALSE),
                                                                                selectInput(inputId = "randomforestlocalImp",label = "Local importance",choices = FALSE, multiple = FALSE),
                                                                                ),
                                                                                column(3,
                                                                                conditionalPanel(condition = "output.hide_randomforest_classification",
                                                                                selectInput(inputId = "randomforestnorm.votes",label = "Express final votes",choices = FALSE, multiple = FALSE)),
                                                                                selectInput(inputId = "randomforestdo.trace",label = "Do trace",choices = FALSE, multiple = FALSE),
                                                                                selectInput(inputId = "randomforestkeep.forest",label = "Keep forest",choices = TRUE, multiple = FALSE),
                                                                                conditionalPanel(condition = "output.hide_randomforest_classification == false",
                                                                                selectInput(inputId = "randomforestcorr.bias",label = "Bias correction",choices = c(FALSE, TRUE), multiple = FALSE)),
                                                                                selectInput(inputId = "randomforestkeep.inbag",label = "Keep inbag",choices = FALSE, multiple = FALSE)),
                                                                                column(12,br(),br())
                                                                                
                                                                  )),
                                                                  column(12,),
                                                                  
                                                                  column(3, actionButton("randomforestaction","Build model", width = "100%")),
                                                                  column(9,conditionalPanel(condition = "input.randomforestaction",
                                                                                            box(title = "Result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                                                column(12,DTOutput("randomforestoutput"))))),
                                                                  
                                                              )
                                                     ),
                                                     tabPanel(title = "Plot", icon = icon("chart-line"), 
                                                              conditionalPanel(condition = "input.randomforestaction",
                                                                               box(title = "Plot",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                                   conditionalPanel(condition = "output.hide_randomforest_classification",
                                                                                   column(12,
                                                                                          fluidRow(
                                                                                            conditionalPanel(condition = "output.hide_randomforest_choices_pca == false",
                                                                                            column(3), 
                                                                                            column(3, selectInput(inputId = "randomforestplotx", label = "Select x", choices = NULL, multiple = F, selected = NULL)),
                                                                                            column(3, selectInput(inputId = "randomforestploty", label = "Select y", choices = NULL, multiple = F, selected = NULL)),
                                                                                            column(3)),
                                                                                            column(5),
                                                                                            column(2, actionButton(inputId = "randomforestplotview", label = "Display plots", width = "100%")),
                                                                                            column(5)),
                                                                                          column(12, br(),
                                                                                                 column(4,),
                                                                                                 column(6,shinyjs::hidden(p(id = "randomforestplotwait", tags$b("Plots are loading ... please be patient, it taks a while"))),),
                                                                                                 column(2)
                                                                                          )),
                                                                                   ),
                                                                                   conditionalPanel(condition = "output.hide_randomforest_classification == false",
                                                                                                    column(12,
                                                                                                           fluidRow(
                                                                                                             column(12,
                                                                                                                    column(5), 
                                                                                                                    column(2, selectInput(inputId = "randomforestregplotx", label = "Select x", choices = NULL, multiple = F, selected = NULL)),
                                                                                                                    column(5)),
                                                                                                             column(12,
                                                                                                                    column(5),
                                                                                                                    column(2,actionButton(inputId = "randomforestregplotview", label = "Display plot", width = "100%")),
                                                                                                                    column(5))),
                                                                                                    ),
                                                                                                    ),
                                                                                   conditionalPanel(condition = "output.hide_randomforest_classification",
                                                                                   column(12, plotOutput("randomforestplottraining")),
                                                                                   column(12, br()),
                                                                                   
                                                                                   column(12, plotOutput("randomforestplottest"))),
                                                                                   conditionalPanel(condition = "output.hide_randomforest_classification == false", 
                                                                                                    column(12, br(), plotOutput("randomforestplotrgression")))
                                                                                   
                                                                               )                    
                                                              )
                                                              
                                                     ),
                                                     tabPanel(title = "Prediction", icon = icon("binoculars"), 
                                                              conditionalPanel(condition = "input.randomforestaction",
                                                                               box(title = "Prediction on new data based on the designed model", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                   column(3,box(title = "Upload new data",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                                                                
                                                                                                radioButtons(inputId = "datarandomforestfileformat_prediction",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                                                                                radioButtons(inputId = "randomforestheader_prediction",label = "Header",choices = c("Yes", "No")),
                                                                                                fileInput("uploaded_randomforest_prediction","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%"),
                                                                                                selectInput(inputId = "randomforest_prediction_response",label = "Response for comparison (if any)",choices = NULL, selected = FALSE, multiple = FALSE, selectize = FALSE, size = 2),
                                                                                                pickerInput(inputId = "randomforestpredictors_numeric_prediction", label = "Select numeric predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                                                pickerInput(inputId = "randomforestpredictors_categorical_prediction", label = "Select categorical predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                                                actionButton("randomforestprepross_prediction_result", "Predict",width = "100%"),
                                                                                   column(12,br()))),
                                                                                   conditionalPanel(condition = "input.randomforestprepross_prediction_result",
                                                                                                    column(9,box(title = "Prediction result", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                                                 column(6,box(title = "Actual", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("randomforest_actual_response"))),
                                                                                                                 column(6,box(title = "Predicted", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("randomforest_predicted_response"))),
                                                                                                                 column(12,column(7,),column(5,uiOutput("download_prediction_randomforest_table")))
                                                                                                    )))))
                                                     )
                                                     
                                         ),
                                         conditionalPanel(condition = "output.uploaded_randomforestdata_prediction_hide_tabpanel == false",
                                                          infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                        )
 
 ),
 
 
 ############################################## k-Nearest Neighbour  (knn) ###########################################################
 tabItem(
 tabName = "knn",
    tabsetPanel(type = "tabs",
               tabPanel(title = "Classification",
                        fluidRow(
                          column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                                         infoBox(title = "k-Nearest Neighbour (kNN)", value = 9, icon = icon("shapes"), color = "purple", subtitle = "Classification", width = 4, fill = T),
                                         p(style="text-align: justify;","KNN is a supervised machine learning algorithm and calculates the distance between each data point then ranks the neighbors from the nearest (most similar) to the furthest (the least similar). Finally, the algorithm classifies unlabeled data based on the number of k-nearest neighbors.
                    DeepBC uses",tags$a(href= "https://cran.r-project.org/web/packages/class/index.html", target= "_blank", " class") ," package to perform k-nearest neighbour algorithm."))),
                          column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                        radioButtons(inputId = "dataknnclassfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                        radioButtons(inputId = "knnclassheader",label = "Header",choices = c("Yes", "No")),
                                        fileInput("uploaded_knnclass","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                                        actionButton("knnclassprepross","Process", width = "100%"))),
                          column(8,tabBox(width = 100,
                                          tabPanel(title = "K-NN classification plot",
                                                   fluidRow(
                                                     column(12,
                                                            tags$img(src="knnclass.png", height = 300, width = "100%")))))),
                          conditionalPanel(condition = "input.knnclassprepross && output.uploaded_knnclassdata_hide_tabpanel == false",
                                           infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                        ),
                        conditionalPanel(condition = c("input.knnclassprepross && output.uploaded_knnclassdata_hide_tabpanel"),
                                         tabsetPanel(type = "tabs",
                                                     tabPanel(title = "Table", icon = icon("table"),
                                                              box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                  DTOutput("knnclassdatashow"))),
                                                     tabPanel(title = "Model", icon = icon("cogs", lib = "font-awesome"),
                                                              box(title = "Model design and result",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                  column(3,box(title = "Training and test set",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                               selectInput(inputId = "knnclassresponse",label = "Select Response",choices = "", selected = NULL, multiple = FALSE),
                                                                               pickerInput(inputId = "knnclasspredictors", label = "Select predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                               sliderInput(inputId = "knnclassratiosplit", label = "Select training split ratio",min = 0.05, max = 0.95, value = 0.7,step = 0.05),
                                                                               sliderInput(inputId = "knnclasskfoldcv", label = "Select a k-fold cross validation",min = 0 , max = 20, value = 10,step = 5))),
                                                                  column(3, box(title = "Parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                                numericInput(inputId = "knnclassk",label = "Number of neighbours ", 5, min = 0, max = 1000),
                                                                                numericInput(inputId = "knnclassl",label = "Minimum vote", 1, min = 0, max = 1000),
                                                                                selectInput(inputId = "knnclassprob",label = "Prob",choices = c(TRUE, FALSE), multiple = FALSE),
                                                                                selectInput(inputId = "knnclassuseall",label = "Use all",choices = c(TRUE, FALSE), multiple = FALSE),
                                                                                ),actionButton("knnclassaction","Build model", width = "100%")),
                                                                  column(6,conditionalPanel(condition = "input.knnclassaction",
                                                                                            box(title = "Result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                                                column(12,DTOutput("knnclassoutput"))))),


                                                              )
                                                     ),
                                                     tabPanel(title = "Plot", icon = icon("chart-line"),
                                                              conditionalPanel(condition = "input.knnclassaction",
                                                                               box(title = "Plot",status = "primary", collapsible = T, solidHeader = T,width = "100%",

                                                                                                    column(12,
                                                                                                           fluidRow(
                                                                                                             conditionalPanel(condition = "output.hide_knnclass_choices_pca == false",
                                                                                                             column(3),
                                                                                                             column(3, selectInput(inputId = "knnclassplotx", label = "Select x", choices = NULL, multiple = F, selected = NULL)),
                                                                                                             column(3, selectInput(inputId = "knnclassploty", label = "Select y", choices = NULL, multiple = F, selected = NULL)),
                                                                                                             column(3)),
                                                                                                             column(5),
                                                                                                             column(2, actionButton(inputId = "knnclassplotview", label = "Display plots", width = "100%")),
                                                                                                             column(5)),
                                                                                                           column(12, br(),
                                                                                                                  column(4,),
                                                                                                                  column(6,shinyjs::hidden(p(id = "knnclassplotwait", tags$b("Plots are loading ... please be patient, it taks a while"))),),
                                                                                                                  column(2)
                                                                                                                  )),

                                                                                                    column(12, plotOutput("knnclassplottraining")),
                                                                                                    column(12, br()),
                                                                                                    column(12, plotOutput("knnclassplottest")),
                                                                               )
                                                              )

                                                     ),
                                                     tabPanel(title = "Prediction", icon = icon("binoculars"),
                                                              conditionalPanel(condition = "input.knnclassaction",
                                                                               box(title = "Prediction on new data based on the designed model", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                   column(3,box(title = "Upload new data",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                                                                radioButtons(inputId = "dataknnclassfileformat_prediction",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                                                                                radioButtons(inputId = "knnclassheader_prediction",label = "Header",choices = c("Yes", "No")),
                                                                                                fileInput("uploaded_knnclass_prediction","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%"),
                                                                                                selectInput(inputId = "knnclass_prediction_response",label = "Response for comparison (if any)",choices = NULL, selected = FALSE, multiple = FALSE, selectize = FALSE, size = 2),
                                                                                                pickerInput(inputId = "knnclass_prediction_predictors", label = "Select predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                                                actionButton("knnclassprepross_prediction_result", "Predict",width = "100%"),
                                                                                                column(12,br()))),
                                                                                   conditionalPanel(condition = "input.knnclassprepross_prediction_result",
                                                                                                    column(9,box(title = "Prediction result", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                                                 column(6,box(title = "Actual", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("knnclass_actual_response"))),
                                                                                                                 column(6,box(title = "Predicted", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("knnclass_predicted_response"))),
                                                                                                                 column(12,column(7,),column(5,uiOutput("download_prediction_knnclass_table")))
                                                                                                    )))))
                                                     )
                                         ),
                                         conditionalPanel(condition = "output.uploaded_knnclassdata_prediction_hide_tabpanel == false",
                                                          infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                        )
                        ),
               tabPanel(title = "Regression",
                        fluidRow(
                          column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                                         infoBox(title = "k-Nearest Neighbour (KNN)", value = 9, icon = icon("shapes"), color = "purple", subtitle = "Regression", width = 4, fill = T),
                                         p(style="text-align: justify;","The same concept for identifying k-nearest neighbors can be implemented for regressions problems; however, it returns individual values rather than classes.
                    DeepBC uses",tags$a(href= "https://cran.r-project.org/web/packages/FNN/index.html", target= "_blank", " FNN") ," package to perform k-nearest neighbour algorithm on regression problems."))),
                          column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                        radioButtons(inputId = "dataknnregfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                        radioButtons(inputId = "knnregheader",label = "Header",choices = c("Yes", "No")),
                                        fileInput("uploaded_knnreg","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                                        actionButton("knnregprepross","Process", width = "100%"))),
                          column(8,tabBox(width = 100,
                                          tabPanel(title = "K-NN regression plot",
                                                   fluidRow(
                                                     column(12,
                                                            tags$img(src="knnregression.png", height = 300, width = "100%")))))),
                          conditionalPanel(condition = "input.knnregprepross && output.uploaded_knnregdata_hide_tabpanel == false",
                                           infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                        ),
                        conditionalPanel(condition = c("input.knnregprepross && output.uploaded_knnregdata_hide_tabpanel"),
                                         tabsetPanel(type = "tabs",
                                                     tabPanel(title = "Table", icon = icon("table"),
                                                              box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                  DTOutput("knnregdatashow"))),
                                                     tabPanel(title = "Model", icon = icon("cogs", lib = "font-awesome"),
                                                              box(title = "Model design and result",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                  column(3,box(title = "Training and test set",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                               selectInput(inputId = "knnregresponse",label = "Select Response",choices = "", selected = NULL, multiple = FALSE),
                                                                               pickerInput(inputId = "knnregpredictors", label = "Select predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                               sliderInput(inputId = "knnregratiosplit", label = "Select training split ratio",min = 0.05, max = 0.95, value = 0.7,step = 0.05),
                                                                               sliderInput(inputId = "knnregkfoldcv", label = "Select a k-fold cross validation",min = 0 , max = 20, value = 10,step = 5))),
                                                                  column(3, box(title = "Parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                                selectInput(inputId = "knnregalgorithm",label = "Algorithm", choices = c("kd_tree", "cover_tree", "brute")),
                                                                                numericInput(inputId = "knnregk",label = "Number of neighbours ", 5, min = 0, max = 1000),

                                                                  )),
                                                                  column(6,conditionalPanel(condition = "input.knnregaction",
                                                                                            box(title = "Result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                                                column(12,DTOutput("knnregoutput"))))),
                                                                  column(9, br(), br(), br(),br(), br(), br(), br()),
                                                                  column(9, actionButton("knnregaction","Build model", width = "30%"))
                                                              )
                                                     ),
                                                     tabPanel(title = "Plot", icon = icon("chart-line"),
                                                              conditionalPanel(condition = "input.knnregaction",
                                                                               box(title = "Plot",status = "primary", collapsible = T, solidHeader = T,width = "100%",

                                                                                   column(12,
                                                                                          fluidRow(
                                                                                            column(12,
                                                                                            column(5),
                                                                                            column(2, selectInput(inputId = "knnregplotx", label = "Select x", choices = NULL, multiple = F, selected = NULL)),
                                                                                            column(5)),
                                                                                            column(12,
                                                                                            column(5),
                                                                                            column(2, actionButton(inputId = "knnregplotview", label = "Display plot", width = "100%")),
                                                                                            column(5))),
                                                                                         ),
                                                                                   column(12, br(), br()),
                                                                                   column(12, plotOutput("knnregplottest")),
                                                                               )                    #verbatimTextOutput
                                                              )

                                                     ),
                                                     tabPanel(title = "Prediction", icon = icon("binoculars"),
                                                              conditionalPanel(condition = "input.knnregaction",
                                                                               box(title = "Prediction on new data based on the designed model", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                   column(3,box(title = "Upload new data",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                                                                radioButtons(inputId = "dataknnregfileformat_prediction",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                                                                                radioButtons(inputId = "knnregheader_prediction",label = "Header",choices = c("Yes", "No")),
                                                                                                fileInput("uploaded_knnreg_prediction","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%"),
                                                                                                selectInput(inputId = "knnreg_prediction_response",label = "Response for comparison (if any)",choices = NULL, selected = FALSE, multiple = FALSE, selectize = FALSE, size = 2),
                                                                                                pickerInput(inputId = "knnreg_prediction_predictors", label = "Select predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                                                actionButton("knnregprepross_prediction_result", "Predict",width = "100%"),
                                                                                                column(12,br()))),
                                                                                   conditionalPanel(condition = "input.knnregprepross_prediction_result",
                                                                                                    column(9,box(title = "Prediction result", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                                                 column(6,box(title = "Actual", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("knnreg_actual_response"))),
                                                                                                                 column(6,box(title = "Predicted", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("knnreg_predicted_response"))),
                                                                                                                 column(12,column(7,),column(5, uiOutput("download_prediction_knnreg_table")))
                                                                                                    )))))
                                                     )

                                         ),
                                         conditionalPanel(condition = "output.uploaded_knnregdata_prediction_hide_tabpanel == false",
                                                          infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                        )
                        )
                )
 ),
 
 
 ############################################## SVM ###########################################################
 tabItem(
   tabName = "svm",
   fluidRow(
     column(12, 
            
            box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                infoBox(title = "Support vector machine (SVM)", value = 10, icon = icon("divide"), color = "maroon", width = 4, fill = T),
                p(style="text-align: justify;","Support vector machine is a supervised machine learning algorithm that can handel both classification and regression problems. It finds an optimal linear hyperplane that separates the classes by maximizing the margin around itself. 
                The data points that touch the margin are called support vectors. In the case of non-linearly separable data, SVM adds an extra dimetion (kernel) to the data. 
                    DeepBC uses",tags$a(href= "https://cran.r-project.org/web/packages/e1071/index.html", target= "_blank", " e1071") ," package to perform SVM algorithm. "))),
     column(4,
            box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                radioButtons( inputId = "datasvmfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                radioButtons(inputId = "svmheader",label = "Header",choices = c("Yes", "No")
                ),
                fileInput("uploaded_svm","Upload a table",multiple = FALSE, accept = c(".csv", ".tsv"), width = "100%"),
                actionButton("svmprepross","Process", width = "100%")
            )),
     column(8,tabBox(width = 100,
                     tabPanel(title = "SVM classification", 
                              fluidRow(
                                column(12,
                                       tags$img(src="svm_plot.png", height = 300, width = "100%")))),
                     tabPanel(title = "SVM regression", 
                              fluidRow(
                                column(12,
                                       tags$img(src="svm_regression.png", height = 300, width = "100%"))))
                     )),
     conditionalPanel(condition = "input.svmprepross && output.uploaded_svmdata_hide_tabpanel == false",
                      infoBox(title = "Error", subtitle = "It must be header, separation field, or file format.", color = "olive", icon = icon("bug"),fill = TRUE))
   ),
   conditionalPanel(condition = c("input.svmprepross && output.uploaded_svmdata_hide_tabpanel"),
                    
                    tabsetPanel(type = "tabs",
                                tabPanel(title = "Table", icon = icon("table"),
                                         box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                             DTOutput("svmdatashow"))),
                                tabPanel(title = "Model", icon = icon("cogs", lib = "font-awesome"),            
                                         box(title = "Model design",status = "primary", collapsible = T, solidHeader = T, width = "100%", 
                                             column(3, box(title = "Training and test set",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                           selectInput(inputId = "svmresponse",label = "Select Response",choices = "", selected = NULL, multiple = FALSE),
                                                           pickerInput(inputId = "svmpredictors", label = "Select predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                           sliderInput(inputId = "svmratiosplit", label = "Select training split ratio",min = 0.05, max = 0.95, value = 0.7,step = 0.05),
                                                           sliderInput(inputId = "svmkfoldcv", label = "Select a k-fold cross validation",min = 0 , max = 20, value = 10,step = 5)),
                                                           actionButton("svmaction","Build model", width = "100%"),
                                                    ),
                                             column(9, box(title = "SVM type, kernel and parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                           column(3,
                                                           selectInput(inputId = "svmtype",label = "Select SVM type",choices = c("C-classification", "nu-classification", "one-classification", "eps-regression", "nu-regression"), selected = NULL, multiple = FALSE),
                                                           selectInput(inputId = "svmkernel",label = "Select a kernel",choices = c("linear", "polynomial", "radial", "sigmoid"), selected = NULL, multiple = FALSE),
                                                           numericInput(inputId = "svmcost",label = "Cost", 1, min = 0, max = 100),
                                                           ),
                                                           column(3,numericInput(inputId = "svmgamma",label = "Gamma", 0, min = 0, max = 100),
                                                                  numericInput("svmepsilon", "Epsilon", 0.001, min = 0, max = 100),
                                                                  numericInput(inputId = "svmtolerance", label = "Tolerance", 0.001, min = 0, max = 10)),
                                                           column(3, 
                                                                  textInput("svmclassweights", "Class weights", placeholder = "Comma separated values"),
                                                                  selectInput(inputId = "svmshrinking",label = "Shrinking",choices = c(TRUE, FALSE), multiple = FALSE),
                                                                  selectInput(inputId = "svmprobability",label = "Probability",choices = c(FALSE, TRUE), multiple = FALSE)
                                                                  ),
                                                           column(3,
                                                                  conditionalPanel(condition = "output.hide_svm_coefo",
                                                                                   numericInput(inputId = "svmcoef0polynomial_sigmoid",label = "Coef0", 0, min = 0, max = 100)),
                                                                  conditionalPanel(condition = "output.hide_svm_nu",
                                                                                   numericInput(inputId = "svmnu",label = "Nu",0.5, min = 0, max = 100)),
                                                                  conditionalPanel(condition = "output.hide_svm_polynomial_degree",
                                                                                   sliderInput(inputId = "svmpolynomialdegree", label = "Select a degree", min = 1, max = 10, value = 3, step = 1)),
                                                           ),
                                                           )),
                                             conditionalPanel(condition = "input.svmaction",
                                             column(9, fluidRow(
                                                    column(12,
                                                    box(title = "Result", status = "primary", solidHeader = T, collapsible = T, width = "100%",
                                                    column(12,
                                                    DTOutput("svmoutput")
                                                    )))))),
                                             
                                             
                                             
                                            
                                         )),
                                tabPanel(title = "Plot", icon = icon("chart-line"), 
                                         conditionalPanel(condition = "input.svmaction",
                                                          box(title = "Plot",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                              conditionalPanel(condition = "output.hide_svm_plot_xy == false" ,
                                                              column(12,
                                                                     fluidRow(
                                                                       column(3), 
                                                                       column(3, selectInput(inputId = "svmplotx", label = "Select x", choices = NULL, multiple = F, selected = NULL)),
                                                                       column(3, selectInput(inputId = "svmploty", label = "Select y", choices = NULL, multiple = F, selected = NULL)),
                                                                       column(3),column(5),
                                                                       column(2, actionButton(inputId = "svmplotview", label = "Display plot", width = "100%")),
                                                                       column(5)),
                                                                     column(12, br())),
                                                              column(12, plotOutput("svmplot"))),
                                                              conditionalPanel(condition = "output.hide_svm_plot_xy == true",
                                                                               column(5),
                                                                               column(2, actionButton(inputId = "svrplotview", label = "Display plot", width = "100%")),
                                                                               column(5),
                                                              column(12, br(), plotOutput("svrplot")))
                                                          )
                                         )
                                         
                                ),
                                tabPanel(title = "Prediction", icon = icon("binoculars"),
                                         conditionalPanel(condition = "input.svmaction",
                                                          box(title = "Prediction on new data based on the designed model", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                              column(3,box(title = "Upload new data",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                                           radioButtons(inputId = "datasvmfileformat_prediction",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                                                           radioButtons(inputId = "svmheader_prediction",label = "Header",choices = c("Yes", "No")),
                                                                           fileInput("uploaded_svm_prediction","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%"),
                                                                           selectInput(inputId = "svm_prediction_response",label = "Response for comparison (if any)",choices = NULL, selected = FALSE, multiple = FALSE, selectize = FALSE, size = 2),
                                                                           pickerInput(inputId = "svm_prediction_num", label = "Select numeric varaible(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                                           actionButton("svmprepross_prediction_result", "Predict",width = "100%"),
                                                                           column(12,br()))),
                                                              conditionalPanel(condition = "input.svmprepross_prediction_result",
                                                                               column(9,box(title = "Prediction result", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                                                            column(6,box(title = "Actual", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("svm_actual_response"))),
                                                                                            column(6,box(title = "Predicted", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("svm_predicted_response"))),
                                                                                            column(12,column(7,),column(5, uiOutput("download_prediction_svm_table")))
                                                                               )))
                                                          )              
                                         )
                                )
                            
                    ),
                    conditionalPanel(condition = "output.uploaded_svmdata_prediction_hide_tabpanel == false",
                                     infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
   )
 ),
 ############################################## NAIVE bayes ###########################################################
 tabItem(
   tabName = "naivebayes",
   fluidRow(
     column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                infoBox(title = "naive bayes", value = 11, icon = icon("envelope"), color = "navy", width = 4, fill = T),
                p(style="text-align: justify;","Naive bayes is a supervised machine learning algorithm, which uses Bayes’ theorem. Like SVM, it can perform classification; 
                however, unlike SVM, navie bayes allows both continuous and categorical variables as predictors.
                    DeepBC uses",tags$a(href= "https://cran.r-project.org/web/packages/e1071/index.html", target= "_blank", " e1071") ," package to perform naive bayes algorithm."))),
     column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                radioButtons(inputId = "datanaivebayesfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                radioButtons(inputId = "naivebayesheader",label = "Header",choices = c("Yes", "No")),
                fileInput("uploaded_naivebayes","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                actionButton("naivebayesprepross","Process", width = "100%"))),
     column(8,tabBox(width = 100,
                     tabPanel(title = "Model's plot", 
                              fluidRow(
                                column(12,
                                       tags$img(src="naivebase.png", height = 300, width = "100%")))))),
     conditionalPanel(condition = "input.naivebayesprepross && output.uploaded_naivebayesdata_hide_tabpanel == false",
                      infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
   ),
            conditionalPanel(condition = c("input.naivebayesprepross && output.uploaded_naivebayesdata_hide_tabpanel"),
                             tabsetPanel(type = "tabs",
                             tabPanel(title = "Table", icon = icon("table"), 
                             box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                 DTOutput("naivebayesdatashow"))),
                                         tabPanel(title = "Model", icon = icon("cogs", lib = "font-awesome"), 
                                      box(title = "Model design and result",status = "primary", collapsible = T, solidHeader = T, width = "100%", 
                                          column(3,box(title = "Training and test set",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                     selectInput(inputId = "naivebayesresponse",label = "Select Response",choices = "", selected = NULL, multiple = FALSE),
                                                     pickerInput(inputId = "naivebayespredictorsnum", label = "Select numeric predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                     pickerInput(inputId = "naivebayespredictorscat", label = "Select categorical predictor(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                     sliderInput(inputId = "naivebayesratiosplit", label = "Select training split ratio",min = 0.05, max = 0.95, value = 0.7,step = 0.05),
                                                     sliderInput(inputId = "naivebayeskfoldcv", label = "Select a k-fold cross validation",min = 0 , max = 20, value = 10,step = 5))),
                                          column(3, box(title = "Parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                     numericInput(inputId = "naivebayeslaplace",label = "Laplace", 0, min = 0, max = 100),
                                                     hr(),
                                                     numericInput(inputId = "naivebayesthreshold",label = "Threshold", 0.001, min = 0, max = 1),
                                                     numericInput(inputId = "naivebayeseps",label = "Eps", 0, min = 0, max = 10))),
                                          column(6,conditionalPanel(condition = "input.naivebayesaction",
                                                                  box(title = "Result",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                                      column(12,DTOutput("naivebayesoutput"))))),
                                          column(9, br(), br(), br(), br(), br(), br()),
                                          column(9, actionButton("naivebayesaction","Build model", width = "30%"))
                                          )
                               ),
                             tabPanel(title = "Plot", icon = icon("chart-line"), 
                                      conditionalPanel(condition = "input.naivebayesaction",
                                                       box(title = "Plot",status = "primary", collapsible = T, solidHeader = T,width = "100%",
                                                           
                                                           column(12,
                                                                  fluidRow(
                                                                    conditionalPanel(condition = "output.hide_naivebayes_choices_pca == false",
                                                                                     column(3), 
                                                                                     column(3, selectInput(inputId = "naivebayesplotx", label = "Select x", choices = NULL, multiple = F, selected = NULL)),
                                                                                     column(3, selectInput(inputId = "naivebayesploty", label = "Select y", choices = NULL, multiple = F, selected = NULL)),
                                                                                     column(3)),
                                                                    column(5),
                                                                    column(2, actionButton(inputId = "naivebayesplotview", label = "Display plots", width = "100%")),
                                                                    column(5)),
                                                                  column(12, br(),
                                                                         column(4,),
                                                                         column(6,shinyjs::hidden(p(id = "naivebayesplotwait", tags$b("Plots are loading ... please be patient, it taks a while"))),),
                                                                         column(2)
                                                                  )),
                                                           
                                                           column(12, plotOutput("naivebayesplottraining")),
                                                           column(12, br()),
                                                           column(12, plotOutput("naivebayesplottest")),
                                                       )       
                                      )
                                      
                             ),
                                     tabPanel(title = "Prediction", icon = icon("binoculars"), 
                                      conditionalPanel(condition = "input.naivebayesaction",
                                      box(title = "Prediction on new data based on the designed model", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                          column(3,box(title = "Upload new data",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                                                     radioButtons(inputId = "datanaivebayesfileformat_prediction",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                                                     radioButtons(inputId = "naivebayesheader_prediction",label = "Header",choices = c("Yes", "No")),
                                                     fileInput("uploaded_naivebayes_prediction","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%"),
                                                     selectInput(inputId = "naivebayes_prediction_response",label = "Response for comparison (if any)",choices = NULL, selected = FALSE, multiple = FALSE, selectize = FALSE, size = 2),
                                                     pickerInput(inputId = "naivebaye_prediction_num", label = "Select numeric varaible(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                     pickerInput(inputId = "naivebayes_prediction_cat", label = "Select categorical variable(s)", choices = "", options = list(`actions-box`=TRUE), multiple = T,width = "100%"),
                                                     actionButton("naivebayesprepross_prediction_result", "Predict",width = "100%"),
                                                     column(12,br()))),
                                          conditionalPanel(condition = "input.naivebayesprepross_prediction_result",
                                          column(9,box(title = "Prediction result", status = "primary", solidHeader = T, collapsible = T,width = "100%",
                                                 column(6,box(title = "Actual", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("naivebayes_actual_response"))),
                                                 column(6,box(title = "Predicted", status = "primary", solidHeader = T, collapsible = T,width = "100%", DTOutput("naivebayes_predicted_response"))),
                                                 column(12,column(7,),column(5,uiOutput("download_prediction_naivebayes_table")))
                                                 )))))
                               ) 
                               ),
                             conditionalPanel(condition = "output.uploaded_naivebayesdata_prediction_hide_tabpanel == false",
                                              infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
                             )
 ),
 ############################################################ Clustering ###############################################
    tabItem(tabName = "clustering",
            fluidRow(
              column(12, box(title = "Machine learning",status = "info", collapsible = T, solidHeader = T, width = "100%",
                             infoBox(title = "clustering", value = 12, icon = icon("sitemap"), color = "red", width = 4, fill = T),
                             p(style="text-align: justify;","Clustering is an unsupervised machine learning technique that tries to identify clusters of more similar cases in a dataset. DeepBC uses", 
                               tags$a(href= "https://cran.r-project.org/web/packages/cluster/index.html", target= "_blank", "cluster"), " and ",
                               tags$a(href= "https://cran.r-project.org/web/packages/factoextra/index.html", target= "_blank", "factoextra"),"packages to perform cluster analysis."))),
              column(4, box(title = "File uploading",status = "primary", solidHeader = T,collapsible = T, width = "100%",
                            radioButtons(inputId = "dataclusteringfileformat",label = "Select a data format",choices = c(csv = ",", Tab= "\t")),
                            radioButtons(inputId = "clusteringheader",label = "Header",choices = c("Yes", "No")),
                            fileInput("uploaded_clustering","Upload a table",multiple = FALSE,accept = c(".csv", ".tsv"),width = "100%",),
                            actionButton("clusteringprepross","Process", width = "100%"))),
              column(8,tabBox(width = 100,
                              tabPanel(title = "Phylogenetic", 
                                       fluidRow(
                                         column(12,
                                                tags$img(src="hclustering_phlylopng.png", height = 300, width = "100%")))),
                              tabPanel(title = "Circular", 
                                       fluidRow(
                                         column(12,
                                                tags$img(src="hclustering_circular.png", height = 300, width = "100%")))),
                              tabPanel(title = "Rectangular", 
                                       fluidRow(
                                         column(12,
                                                tags$img(src="hclustering_regtangle.png", height = 300, width = "100%")))),
                              tabPanel(title = "Cluster plot", 
                                       fluidRow(
                                         column(12,
                                                tags$img(src="clusterplot.png", height = 300, width = "100%")))),
                              tabPanel(title = "Elbow method", 
                                       fluidRow(
                                         column(12,
                                                tags$img(src="elbowmethod.png", height = 300, width = "100%"))))
                              )),
              
              conditionalPanel(condition = "input.clusteringprepross && output.uploaded_clustering_hide_tabpanel == false",
                               infoBox(title = "Error",subtitle = "It must be header, separation field, or file format.",color = "olive",icon = icon("bug"),fill = TRUE))
            ),
            conditionalPanel(condition = c("input.clusteringprepross && output.uploaded_clustering_hide_tabpanel"),
                             tabsetPanel(type = "tabs",
                                         tabPanel(title = "Table", icon = icon("table"),
                                                  box(title = "Uploaded table",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                      DTOutput("clusteringdatashow"))),
                                         tabPanel(title = "Clustering", icon = icon("sitemap"),
                                                  box(title = "Clustering",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                      column(3,box(title = "Parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                      pickerInput(inputId = "clusteringvariables", label = "Select numeric variables", choices = "", options = list(`actions-box`=TRUE), multiple = TRUE,width = "100%"),
                                                      selectInput(inputId = "clustering_rowname", label = "First column as row names", choices = c(TRUE, FALSE), multiple = F),
                                                      selectInput(inputId = "clusteringtype", label = "Clustering type", choices = c("K-Means", "Hierarchical"), multiple = F),
                                                      conditionalPanel(condition = "output.hide_hierarchy_plot",
                                                      selectInput(inputId = "clusteringmethod", label = "Agglomeration method", choices = c( "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median","centroid"), multiple = F),
                                                      selectInput(inputId = "clusteringdistancemethod", label = "Distance measuring method", choices = c( "euclidean", "maximum", "manhattan", "canberra", "minkowski"), multiple = F)),
                                                      conditionalPanel(condition = "output.hide_hierarchy_plot == false",
                                                      actionButton(inputId = "clustering_elbomethod", label = "Decide the number of clusters", width = "100%")),
                                                      conditionalPanel(condition = c("input.clustering_elbomethod && output.hide_elbow_plot"),
                                                      column(12,br()),
                                                      sliderInput(inputId = "clustering_clusternumber",label = "Select number of clusters",min = 1, max = 10, value = 4,step = 1),
                                                      actionButton(inputId = "clustering_start",label = "Find clusters", width = "100%")),
                                                      conditionalPanel(condition = "output.hide_hierarchy_plot",
                                                      actionButton(inputId = "clustering_hierarchical_plot_show", label = "Find clusters", width = "100%"))),
                                                      
                                                      ),
                                                      conditionalPanel(condition = c("input.clustering_elbomethod && output.hide_elbow_plot"),
                                                      column(9, box(title = "Optimal number of clusters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                      plotOutput("clustering_elbow",  width = "100%", height = 410))),
                                                      conditionalPanel(condition = "input.clustering_start",
                                                      column(12, box(title = "Clusters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                     column(12, plotOutput("clustering_kmean_plot"))
                                                                     
                                                                     )))),
                                                      conditionalPanel(condition = "output.hide_hierarchy_plot",
                                                      column(3,box(title = "Dendrogram parameters",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                      sliderInput(inputId = "clustering_numberofcut",label = "Select number of cuts",min = 1, max = 10, value = 4,step = 1),
                                                      selectInput(inputId = "dendrogramtype", label = "Select dendrogram type", choices = c("rectangle", "circular", "phylogenic"), multiple = F),
                                                      selectInput(inputId = "dendrogramposition", label = "Position", choices = c("Vertical", "Horizental"), multiple = F),
                                                      selectInput(inputId = "dendrogram_phylolayout", label = "Phylogenic layout", choices = c("layout.auto", "layout_with_drl", "layout_as_tree", "layout.gem", "layout.mds", "layout_with_lgl"), multiple = F),
                                                      selectInput(inputId = "clustering_dendrogramborder", label = "Border", choices = c(FALSE, TRUE), multiple = F)
                                                      
                                                      
                                                                   )),
                                                      conditionalPanel(condition = "input.clustering_hierarchical_plot_show",
                                                      column(12, box(title = "Cluster Dendrogram",status = "primary", collapsible = T, solidHeader = T, width = "100%",
                                                                     plotOutput("clustering_hierarchical_plot")))))
                                                      
                                                  ))
                             ))
            ),
    ############################# Deep learning ################################
    tabItem(
      tabName = "dklll",
      h1("Making a prediction using DeepBC model"),
      #tags$img(src="rstudio.png", height = 140, width = 400),
      fluidRow(
        box(title = "File uploading",
            status = "primary", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            width = 5,
            
            radioButtons("fileformat",
                         label = "Select a data format",
                         choices = c(csv = ",", Tab= "\t")
                         
            ),
            fileInput("dlgenepred",
                      "Upload candidate genes for prediction",
                      multiple = FALSE,
                      accept = c(".csv", ".tsv")
                      
            ),
            actionButton("prediction",
                         "Predicion",
                         width = "100%")
            
           
            ),
        tabBox(id = "dlresult",
          title = "Result",side = "right",
          width = 7,
               ),
        
        
            
        )
      )
    )
    
  )
  





dashboardPage(
  header = dashheader,
  sidebar = dashSidebar,
  body = dashBody,
  title = "DeepBC",
  skin = "purple"
 
)