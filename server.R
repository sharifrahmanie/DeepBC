require(tidyverse)
require(DT)
library(BiocManager)
options(repos = BiocManager::repositories())
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
options(shiny.maxRequestSize=400*1024^2) 
options(warn=-1)
graphics.off()
shinyServer(function(input, output, session){
  darkmode_toggle(inputid = 'togglemode')
  observeEvent(input$findmemap, {
    toggleModal(session, "modalExample2", "open")
  })
  output$mylocation <- renderUI({
    bsModal("modalExample2", 
            title = HTML('<span style="color:#083a67; font-size: 20px; font-weight:bold; font-family:LATO ">My location<span>'),
            "btnn_1", size = "large",
            column(12,renderLeaflet({
              leaflet(options = leafletOptions(minZoom = 1, maxZoom = 6)) %>%
                addTiles() %>%
                addMarkers(lat = 36.69,  lng = 45.13)
            })
            )
    )
  })
  ##################Download my CV
  output$download_cv<- renderUI({
    downloadButton('mycv', 'My CV')
  })
  
  output$mycv <- downloadHandler(
    filename = function(){
      paste0("E.SH.Rahmani_CV.pdf")
    },
    content = function(file){
      file.copy("data/Rahmani_CV.pdf",
                file)
      
    }
  )
  
  ########################################## packages list ##############################
  output$packages <- renderDT({
    read_csv("data/packages.csv")
  })
  observeEvent(input$packagesver, { 
    showModal(div(id="modalAutoSaveMenu", modalDialog(
      inputId = "packages",
      title = HTML('<span style="color:#083a67; font-size: 20px; font-weight:bold; font-family:LATO ">DeepBC packages list<span>
                     <button type = "button" class="close" data-dismiss="modal" ">
                     <span style="color:#083a67; ">x <span>
                     </button> '),
      br(),
      DTOutput("packages"),
      br(),
      easyClose = TRUE, 
      footer = NULL )))
  })
  
  onevent('mouseover','Check',{
    
    
    delay(1, 
          showModal(div(id="modalAutoSaveMenu", modalDialog(
            inputId = "packages",
            title = HTML('<span style="color:#083a67; font-size: 20px; font-weight:bold; font-family:LATO ">DeepBC packages list<span>
                       <button type = "button" class="close" data-dismiss="modal" ">
                       <span style="color:#083a67; ">x <span>
                       </button> '),
            br(),
            DTOutput("packages"),
            br(),
            easyClose = TRUE,
            footer = NULL )))) }, T)
###########################################################################################################################################  
  # example
  output$example_show <- renderTable({
      read.csv("data/example.csv")
  })

  ######################################## data preprocessing #######################
  
  #################################### error cheking 2 (- expression) ###############
  output$uploaded_predata_hide_tabpanel <- reactive({
  error_catghing2 <- function(path, sep){
    ext <- str_extract(path, '(?=.)[a-z]{3}$')
    if(sep == ","){
      sepa <- "csv"
    } else {
      sepa <- "tsv"
    }
    table <- !is.null(input$uploaded_predata)
    checknull <- NCOL(uploaded_predata_all()) > 1
    if(table && checknull && sepa == ext){
      TRUE
    } else {
      FALSE
    }
  }
  error_catghing2(input$uploaded_predata$name, input$datapreprofileformat)
  })
  outputOptions(output, 'uploaded_predata_hide_tabpanel', suspendWhenHidden=FALSE)

  uploaded_predata_all <- eventReactive(input$uploaded_predata, {
    if(!is.null(input$uploaded_predata)){

    if(input$datapreprofileformat == "," && input$preprocessheader == "Yes"){
      rawdata <- try(read_csv(input$uploaded_predata$datapath,
                              col_names = T),
                     silent = T)
      rawdata
    }
    else if(input$datapreprofileformat == "," && input$preprocessheader == "No") {
      rawdata <- try(read_csv(input$uploaded_predata$datapath,
                              col_names = F),
                     silent = T)
      rawdata
    }
    else if((input$datapreprofileformat == "\t" && input$preprocessheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_predata$datapath,
                                col_names = T),
                       silent = T)
        rawdata
    }
    else if((input$datapreprofileformat == "\t" && input$preprocessheader == "No")){
          rawdata <- try(read_tsv(input$uploaded_predata$datapath,
                                  col_names = F),
                         silent = T)
          rawdata
    }
  }
  })
  # SHOW FULL TABLE
  output$data_preprocessing <- renderDT({
    if(!is.null(uploaded_predata_all()) && input$prepross){
    uploaded_predata_all <- isolate(uploaded_predata_all())
        dataprep <- uploaded_predata_all %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          editable= T,
          style = "auto",
          rownames = FALSE)
      dataprep
   }
  })

  output$dataprestatistics <- renderDT({
    req(input$uploaded_predata)
    if(!is.null(input$uploaded_predata) && input$prepross) {
      if(input$generaloverview == "All"){
        dignaosis <- uploaded_predata_all() %>%
          diagnose() %>%
          datatable(
            fillContainer = F,
            options = list(scrollX = TRUE, pageLength = 5),
            extensions = "AutoFill",
            style = "auto",
            rownames = FALSE
          )
        dignaosis
      }
      else if(input$generaloverview == "Numeric"){
        dignaosis1 <- uploaded_predata_all() %>%
          diagnose_numeric() %>%
          datatable(
            fillContainer = F,
            options = list(scrollX = TRUE, pageLength = 5),
            extensions = "AutoFill",
            style = "auto",
            rownames = FALSE
          )
        dignaosis1
      }
      else if(input$generaloverview == "Categorical"){
        dignaosis3 <- uploaded_predata_all() %>%
          diagnose_category()%>%
          datatable(
            fillContainer = F,
            options = list(scrollX = TRUE, pageLength = 5),
            extensions = "AutoFill",
            style = "auto",
            rownames = FALSE
          )
        dignaosis3
      }
      else if(input$generaloverview == "Missing"){
        dignaosis <- uploaded_predata_all() %>%
          diagnose() %>%
          dplyr::select(-unique_count, -unique_rate) %>%
          filter(missing_count > 0) %>%
          arrange(desc(missing_count)) %>%
          datatable(
            fillContainer = F,
            options = list(scrollX = TRUE, pageLength = 5),
            extensions = "AutoFill",
            style = "auto",
            rownames = FALSE
          )
        dignaosis
      }
      else if(input$generaloverview == "MissingCategorical"){
        dignaosis <- uploaded_predata_all() %>%
          diagnose_category() %>%
          filter(is.na(levels)) %>%
          datatable(
            fillContainer = F,
            options = list(scrollX = TRUE, pageLength = 5),
            extensions = "AutoFill",
            style = "auto",
            rownames = FALSE
          )
        dignaosis
      }

    }

  })

   ########################### outliers outlierDT  ################################3

  output$outlierDT <- renderDT({
    req(input$uploaded_predata)
    type <- sapply(uploaded_predata_all(), class)
    isnumeric <- length(grep("numeric", type)) > 0
    if(!is.null(input$uploaded_predata) && input$prepross && isnumeric) {
      if(input$outlierdetection == "All"){
        dignaosis <- uploaded_predata_all() %>%
          diagnose_outlier() %>%
          datatable(
            fillContainer = F,
            options = list(scrollX = TRUE, pageLength = 5),
            extensions = "AutoFill",
            style = "auto",
            rownames = FALSE
          )
        dignaosis
      }
      else if(input$outlierdetection == "Outliers") {
        dignaosis <- uploaded_predata_all() %>%
          diagnose_outlier() %>%
          dplyr::filter(outliers_cnt > 0) %>%
          datatable(
            fillContainer = F,
            options = list(scrollX = TRUE, pageLength = 5),
            extensions = "AutoFill",
            style = "auto",
            rownames = FALSE
          )
        dignaosis
      }
      else if(input$outlierdetection == "OutliersRatio") {
        dignaosis <- uploaded_predata_all() %>%
          diagnose_outlier() %>%
          filter(outliers_ratio >= input$outlier_ratio) %>%
          mutate(rate = outliers_mean / with_mean) %>%
          arrange(desc(rate)) %>%
          dplyr::select(-outliers_cnt) %>%
          datatable(
            fillContainer = F,
            options = list(scrollX = TRUE, pageLength = 5),
            extensions = "AutoFill",
            style = "auto",
            rownames = FALSE
          )
        dignaosis
      }
      }
    })

  ###################################### plot NAs ###########################

  output$paretoNA <- renderPlot({
    req(input$uploaded_predata)
    if(!is.null(input$uploaded_predata) && input$prepross && length(grep("TRUE", is.na(uploaded_predata_all()))) > 0) {
      paretoplot <- uploaded_predata_all() %>%
        plot_na_pareto(main = "Pareto Chart for missing values")
      paretoplot
    } else{

    }

  })
  
  outlier_choices_reactive <- reactive({
    if(!is.null(uploaded_predata_all()) && input$prepross){
      choices_outlier <- isolate(uploaded_predata_all())
      type <- sapply(choices_outlier, class)
      isnumeric <- grep("numeric", type)
      len_numeric <- length(isnumeric)
      if(NCOL(choices_outlier) > 1 && len_numeric > 0){
      variables_numeric <- names(choices_outlier)[isnumeric]
      choices_outlier[,variables_numeric] %>%
        diagnose_outlier() %>%
        filter(outliers_ratio >= 0) %>%
        dplyr::select(variables)
      } else{
        return(NULL)
      }
    } else{
      return(NULL)
    }
  })
  ###################################### plot outliers ###########################
  #choosing variables based on the table name
  observeEvent(outlier_choices_reactive(), {
    if(!is.null(outlier_choices_reactive())){
      choices <- outlier_choices_reactive()
      updateSelectInput(
        session,
        "outlierplotvariable",
        choices = choices)
    }
  })

  output$outliersplot <- renderPlot({
    if(!is.null(uploaded_predata_all()) && input$prepross) {
      preproces_df <- isolate(uploaded_predata_all())
      preproces_df %>%
        plot_outlier(input$outlierplotvariable)
    }
  })

# #   ################################### hiding slider for outlier ratio
# #   # output$hide_oulier_ratio_slider <- reactive({
# #   #   hideselect <- function(){
# #   #     if(input$outlierdetection == "OutliersRatio"){
# #   #       return(TRUE)
# #   #     }
# #   #     else{
# #   #       return(FALSE)
# #   #     }
# #   #   }
# #   #   hideselect()
# #   # })
# #   # outputOptions(output, 'hide_oulier_ratio_slider', suspendWhenHidden=FALSE)
# #   
# #   
# #   
# #   
# #   
# #   
   ################################################## missing slectinput choices ######################
  # choices for missing method
  missing_choices_reactive <- reactive({
    if(input$missingtype == "Numeric"){
      choices <- c("mean", "median")
    }
    else if(input$missingtype == "Character"){
      choices <- c("mode")
    }
    else if(input$missingtype == "Numeric-Character"){
      choices <- c("mean", "median")
    }
    else if(input$missingtype == "Remove"){
      choices <- "NULL"
    }
    else if(input$missingtype == "No-action"){
      choices <- "NULL"
    }  
  })

  observeEvent(missing_choices_reactive(), {
    choices <- missing_choices_reactive()
      updateSelectInput(
        session,
        "missingmethod",
        choices = choices)
  })
 ########################################## hiding row and column select inputation
  output$hide_romve_missing_choices <- reactive({
    hideselect <- function(){
      if(input$missingtype == "Remove"){
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }
    hideselect()
 })
  outputOptions(output, 'hide_romve_missing_choices', suspendWhenHidden=FALSE)
  ######################################################
  output$hide_noaction_choices <- reactive({
    hideselect <- function(){
      if(input$missingtype == "No-action"){
        return(FALSE)
      }
      else{
        return(TRUE)
      }
    }
    hideselect()
  })
  outputOptions(output, 'hide_noaction_choices', suspendWhenHidden=FALSE)
  ###############################################################################
  ################################# Action panel ################################
  missing_table_reactive <- reactive({
    if(!is.null(input$uploaded_predata) && input$missinaction){

      if(input$datapreprofileformat == "," && input$preprocessheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_predata$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if(input$datapreprofileformat == "," && input$preprocessheader == "No") {
        rawdata <- try(read_csv(input$uploaded_predata$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datapreprofileformat == "\t" && input$preprocessheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_predata$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datapreprofileformat == "\t" && input$preprocessheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_predata$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }


      rawdata <- as.data.frame(rawdata)
      missing <- function(df, type, method, remove_choice) {
        dftype <- sapply(df, class)
        isnumeric <- grep("numeric", dftype)
        list_nanum <- colnames(df[isnumeric])[apply(df[isnumeric], 2, anyNA) ]
        ischar <- grep("character|logical", dftype)
        list_nachar <- colnames(df[ischar])[apply(df[ischar], 2, anyNA) ]

        if(type == "Numeric" && method == "mean" | type == "Numeric" && method == "median"){
          isnumeric <- grep("numeric", dftype)
          list_na <- colnames(df[isnumeric])[apply(df[isnumeric], 2, anyNA) ]

          if(length(list_na) > 0){
          average_missing <- apply(df[colnames(df) %in% list_na], 2, method, na.rm =  TRUE)
          list_computed_missing_mean_median <- list()
          for(i in 1:length(list_na)){
            cols <- paste0("" , list_na[i])
            numcols <- grep(cols, colnames(df))
            list_computed_missing_mean_median[[i]] <- ifelse(is.na(df[list_na[i]]), average_missing[i], df[, numcols])
          }
          numeric_nomissing <- data.frame(do.call(cbind, list_computed_missing_mean_median))
          chng <- colnames(numeric_nomissing)
          numg <- which(names(df) %in% chng)
          changed_plus_rest <- cbind(numeric_nomissing, df[, -numg])
          changed_plus_rest <- changed_plus_rest[, names(df)]
          return(changed_plus_rest)
        }
        }
        else if(type == "Character" & method == "mode"){
          ischar <- grep("character|logical", dftype)
          list_na <- colnames(df[ischar])[apply(df[ischar], 2, anyNA) ]
          list_computed_missing_mode <- list()
          if(length(list_na) > 0){
          for(i in 1:length(list_na)){
            cols <- paste0("" , list_na[i])
            numcols <- grep(cols, colnames(df))
            uniq <- unique(df[, numcols])
            mode <- uniq[which.max(tabulate(match(df[, numcols], uniq)))]
            list_computed_missing_mode[[i]] <- ifelse(is.na(df[list_na[i]]), mode, df[, numcols])
          }
          char_missing <- data.frame(do.call(cbind, list_computed_missing_mode))
          chng <- colnames(char_missing)
          numg <- which(names(df) %in% chng)
          changed_plus_rest <- cbind(char_missing, df[-numg])
          changed_plus_rest <- changed_plus_rest[, names(df)]
          return(changed_plus_rest)
        }
      }
        # both
        if(type == "Numeric-Character" && method == "median" | type == "Numeric-Character" && method == "mean"){
          isnumeric <- grep("numeric", dftype)
          list_nanum <- colnames(df[isnumeric])[apply(df[isnumeric], 2, anyNA) ]
          average_missing <- apply(df[colnames(df) %in% list_nanum], 2, method, na.rm =  TRUE)
          list_computed_missing_mean_median <- list()

          if(length(list_nanum) > 0){
          for(i in 1:length(list_nanum)){
            colsumn <- paste0("" , list_nanum[i])
            numcolsnum <- grep(colsumn, colnames(df))
            list_computed_missing_mean_median[[i]] <- ifelse(is.na(df[list_nanum[i]]), average_missing[i], df[, numcolsnum])
          }
          numeric_nomissing <- data.frame(do.call(cbind, list_computed_missing_mean_median))
          }else{
            numeric_nomissing <- NULL
          }

          ischar <- grep("character|logical", dftype)
          list_nachar <- colnames(df[ischar])[apply(df[ischar], 2, anyNA) ]
          list_computed_missing_mode <- list()

          if(length(list_nachar) > 0){
          for(i in 1:length(list_nachar)){
            colschar <- paste0("" , list_nachar[i])
            numcolschar <- grep(colschar, colnames(df))
            uniq <- unique(df[, numcolschar])
            mode <- uniq[which.max(tabulate(match(df[, numcolschar], uniq)))]
            list_computed_missing_mode[[i]] <- ifelse(is.na(df[list_nachar[i]]), mode, df[, numcolschar])
          }
          char_missing <- data.frame(do.call(cbind, list_computed_missing_mode))
          }else{
            char_missing <- NULL
          }

          if(!is.null(numeric_nomissing) && !is.null(char_missing)){
          chnum <- colnames(numeric_nomissing)
          chachar <- colnames(char_missing)
          numum <- match(chnum, names(df))
          numchar <- match(chachar, names(df))
          changed_plus_rest <- cbind(numeric_nomissing, char_missing, df[, -c(numum, numchar)])
          changed_plus_rest <- changed_plus_rest[, names(df)]
          return(changed_plus_rest)
          }
        }
        ############
        else if(type == "Remove" && remove_choice == "column" && length(list_nanum) > 0 | type == "Remove" && remove_choice == "column" && length(list_nachar) > 0 ){
          col_nomissing <- na.omit(t(df))
          col_nomissing <- data.frame(t(col_nomissing))
          return(col_nomissing)
        }
        else if(type == "Remove" && remove_choice == "row" && length(list_nanum) > 0 | type == "Remove" && remove_choice == "row" && length(list_nachar) > 0 ){
          row_nomissing <- na.omit(df)
          return(row_nomissing)
        }

        else if(type == "No-action"){
          return(df)
        }
      }
        misstable <- try(missing(df = rawdata, type = input$missingtype, method = input$missingmethod, remove_choice = input$roworcolumn), silent = T)
        misstable
    }
  })

 ################################ showing computed missing table
    output$missingprerocessingaction <- renderDT({
      if(input$missinaction){
      missing_table_reactive <- isolate(missing_table_reactive())
        misstable1 <- missing_table_reactive %>%
          datatable(
            fillContainer = F,
            options = list(scrollX = TRUE, pageLength = 5),
            extensions = "AutoFill",
            style = "auto",
            rownames = FALSE
          )
}
    })
  ###########################################################################################
   ######################## outlier imputing choices  based on missing computed ##############
    reactive_outlier_choices <- reactive({
      if(!is.null(input$uploaded_predata) && input$missinaction){
        uploaded_predata <- isolate(uploaded_predata_all())
        uploaded_predata <- data.frame(uploaded_predata)
        missing <- function(df, type, method, remove_choice) {
          dftype <- sapply(df, class)
          isnumeric <- grep("numeric", dftype)
          list_nanum <- colnames(df[isnumeric])[apply(df[isnumeric], 2, anyNA) ]
          ischar <- grep("character|logical", dftype)
          list_nachar <- colnames(df[ischar])[apply(df[ischar], 2, anyNA) ]

          if(type == "Numeric" && method == "mean" | type == "Numeric" && method == "median"){
            isnumeric <- grep("numeric", dftype)
            list_na <- colnames(df[isnumeric])[apply(df[isnumeric], 2, anyNA) ]

            if(length(list_na) > 0){
              average_missing <- apply(df[colnames(df) %in% list_na], 2, method, na.rm =  TRUE)
              list_computed_missing_mean_median <- list()
              for(i in 1:length(list_na)){
                cols <- paste0("" , list_na[i])
                numcols <- grep(cols, colnames(df))
                list_computed_missing_mean_median[[i]] <- ifelse(is.na(df[list_na[i]]), average_missing[i], df[, numcols])
              }
              numeric_nomissing <- data.frame(do.call(cbind, list_computed_missing_mean_median))
              chng <- colnames(numeric_nomissing)
              numg <- which(names(df) %in% chng)
              changed_plus_rest <- cbind(numeric_nomissing, df[, -numg])
              changed_plus_rest <- changed_plus_rest[, names(df)]
              return(changed_plus_rest)
            }
          }
          else if(type == "Character" & method == "mode"){
            ischar <- grep("character|logical", dftype)
            list_na <- colnames(df[ischar])[apply(df[ischar], 2, anyNA) ]
            list_computed_missing_mode <- list()
            if(length(list_na) > 0){
              for(i in 1:length(list_na)){
                cols <- paste0("" , list_na[i])
                numcols <- grep(cols, colnames(df))
                uniq <- unique(df[, numcols])
                mode <- uniq[which.max(tabulate(match(df[, numcols], uniq)))]
                list_computed_missing_mode[[i]] <- ifelse(is.na(df[list_na[i]]), mode, df[, numcols])
              }
              char_missing <- data.frame(do.call(cbind, list_computed_missing_mode))
              chng <- colnames(char_missing)
              numg <- which(names(df) %in% chng)
              changed_plus_rest <- cbind(char_missing, df[-numg])
              changed_plus_rest <- changed_plus_rest[, names(df)]
              return(changed_plus_rest)
            }
          }
          # both
          if(type == "Numeric-Character" && method == "median" | type == "Numeric-Character" && method == "mean"){
            isnumeric <- grep("numeric", dftype)
            list_nanum <- colnames(df[isnumeric])[apply(df[isnumeric], 2, anyNA) ]
            average_missing <- apply(df[colnames(df) %in% list_nanum], 2, method, na.rm =  TRUE)
            list_computed_missing_mean_median <- list()

            if(length(list_nanum) > 0){
              for(i in 1:length(list_nanum)){
                colsumn <- paste0("" , list_nanum[i])
                numcolsnum <- grep(colsumn, colnames(df))
                list_computed_missing_mean_median[[i]] <- ifelse(is.na(df[list_nanum[i]]), average_missing[i], df[, numcolsnum])
              }
              numeric_nomissing <- data.frame(do.call(cbind, list_computed_missing_mean_median))
            }else{
              numeric_nomissing <- NULL
            }

            ischar <- grep("character|logical", dftype)
            list_nachar <- colnames(df[ischar])[apply(df[ischar], 2, anyNA) ]
            list_computed_missing_mode <- list()

            if(length(list_nachar) > 0){
              for(i in 1:length(list_nachar)){
                colschar <- paste0("" , list_nachar[i])
                numcolschar <- grep(colschar, colnames(df))
                uniq <- unique(df[, numcolschar])
                mode <- uniq[which.max(tabulate(match(df[, numcolschar], uniq)))]
                list_computed_missing_mode[[i]] <- ifelse(is.na(df[list_nachar[i]]), mode, df[, numcolschar])
              }
              char_missing <- data.frame(do.call(cbind, list_computed_missing_mode))
            }else{
              char_missing <- NULL
            }

            if(!is.null(numeric_nomissing) && !is.null(char_missing)){
              chnum <- colnames(numeric_nomissing)
              chachar <- colnames(char_missing)
              numum <- match(chnum, names(df))
              numchar <- match(chachar, names(df))
              changed_plus_rest <- cbind(numeric_nomissing, char_missing, df[, -c(numum, numchar)])
              changed_plus_rest <- changed_plus_rest[, names(df)]
              return(changed_plus_rest)
            }
          }
          ############
          else if(type == "Remove" && remove_choice == "column" && length(list_nanum) > 0 | type == "Remove" && remove_choice == "column" && length(list_nachar) > 0 ){
            dftype <- sapply(df, class)
            ischar <- grep("character|logical", dftype)
            col_nomissing <- na.omit(t(df[,-ischar]))
            col_nomissing <- data.frame(t(col_nomissing))
            nam <- na.omit(t(df[,ischar]))
            nam <- data.frame(t(nam))
            col_nomissing <- cbind(nam, col_nomissing)
            if(length(names(col_nomissing)) == length(names(df))){
              col_nomissing <- col_nomissing[, names(df)]
            }
            return(col_nomissing)
          }
          else if(type == "Remove" && remove_choice == "row" && length(list_nanum) > 0 | type == "Remove" && remove_choice == "row" && length(list_nachar) > 0 ){
            row_nomissing <- na.omit(df)
            return(row_nomissing)
          }

          else if(type == "No-action"){
            return(df)
          }
        }
        misstablereactive <- try(missing(df = uploaded_predata, type = input$missingtype, method = input$missingmethod, remove_choice = input$roworcolumn), silent = T)
        dftype <- sapply(misstablereactive, class)
        isnumeric <- grep("numeric", dftype)
        len_numeric <- length(isnumeric)
        if(!is.null(misstablereactive) && len_numeric > 0){
        misstablereactive <- data.frame(misstablereactive)[isnumeric]
          outliers_vars <- misstablereactive %>%
            diagnose_outlier() %>%
            filter(outliers_ratio >= input$outlier_ratio_imputation) %>%
            dplyr::select(variables)
          if(nrow(outliers_vars) == 1){
            names(outliers_vars) <- outliers_vars[1,1]
            outliers_vars
          }else {
            outliers_vars
          }
        }else{
          return(NULL)
        }
      }
    })

  #############################################################################
  #outlier choices
  observeEvent(reactive_outlier_choices(), {
  if(!is.null(reactive_outlier_choices())){
      updatePickerInput(
        session,
        "outliertransformation",
        choices = reactive_outlier_choices())
  }
  })


  # #############################################################################################
  # ######################### making available the outliers data for table of imputation #########
    table_outliers_imputation <- reactive({
      if(!is.null(input$uploaded_predata) && input$oulieractionaction){
        uploaded_predata <- as.data.frame(uploaded_predata_all())
        missing <- function(df, type, method, remove_choice) {
          dftype <- sapply(df, class)
          isnumeric <- grep("numeric", dftype)
          list_nanum <- colnames(df[isnumeric])[apply(df[isnumeric], 2, anyNA) ]
          ischar <- grep("character|logical", dftype)
          list_nachar <- colnames(df[ischar])[apply(df[ischar], 2, anyNA) ]

          if(type == "Numeric" && method == "mean" | type == "Numeric" && method == "median"){
            isnumeric <- grep("numeric", dftype)
            list_na <- colnames(df[isnumeric])[apply(df[isnumeric], 2, anyNA) ]

            if(length(list_na) > 0){
              average_missing <- apply(df[colnames(df) %in% list_na], 2, method, na.rm =  TRUE)
              list_computed_missing_mean_median <- list()
              for(i in 1:length(list_na)){
                cols <- paste0("" , list_na[i])
                numcols <- grep(cols, colnames(df))
                list_computed_missing_mean_median[[i]] <- ifelse(is.na(df[list_na[i]]), average_missing[i], df[, numcols])
              }
              numeric_nomissing <- data.frame(do.call(cbind, list_computed_missing_mean_median))
              chng <- colnames(numeric_nomissing)
              numg <- which(names(df) %in% chng)
              changed_plus_rest <- cbind(numeric_nomissing, df[, -numg])
              changed_plus_rest <- changed_plus_rest[, names(df)]
              return(changed_plus_rest)
            }
          }
          else if(type == "Character" & method == "mode"){
            ischar <- grep("character|logical", dftype)
            list_na <- colnames(df[ischar])[apply(df[ischar], 2, anyNA) ]
            list_computed_missing_mode <- list()
            if(length(list_na) > 0){
              for(i in 1:length(list_na)){
                cols <- paste0("" , list_na[i])
                numcols <- grep(cols, colnames(df))
                uniq <- unique(df[, numcols])
                mode <- uniq[which.max(tabulate(match(df[, numcols], uniq)))]
                list_computed_missing_mode[[i]] <- ifelse(is.na(df[list_na[i]]), mode, df[, numcols])
              }
              char_missing <- data.frame(do.call(cbind, list_computed_missing_mode))
              chng <- colnames(char_missing)
              numg <- which(names(df) %in% chng)
              changed_plus_rest <- cbind(char_missing, df[-numg])
              changed_plus_rest <- changed_plus_rest[, names(df)]
              return(changed_plus_rest)
            }
          }
          # both
          if(type == "Numeric-Character" && method == "median" | type == "Numeric-Character" && method == "mean"){
            isnumeric <- grep("numeric", dftype)
            list_nanum <- colnames(df[isnumeric])[apply(df[isnumeric], 2, anyNA) ]
            average_missing <- apply(df[colnames(df) %in% list_nanum], 2, method, na.rm =  TRUE)
            list_computed_missing_mean_median <- list()

            if(length(list_nanum) > 0){
              for(i in 1:length(list_nanum)){
                colsumn <- paste0("" , list_nanum[i])
                numcolsnum <- grep(colsumn, colnames(df))
                list_computed_missing_mean_median[[i]] <- ifelse(is.na(df[list_nanum[i]]), average_missing[i], df[, numcolsnum])
              }
              numeric_nomissing <- data.frame(do.call(cbind, list_computed_missing_mean_median))
            }else{
              numeric_nomissing <- NULL
            }

            ischar <- grep("character|logical", dftype)
            list_nachar <- colnames(df[ischar])[apply(df[ischar], 2, anyNA) ]
            list_computed_missing_mode <- list()

            if(length(list_nachar) > 0){
              for(i in 1:length(list_nachar)){
                colschar <- paste0("" , list_nachar[i])
                numcolschar <- grep(colschar, colnames(df))
                uniq <- unique(df[, numcolschar])
                mode <- uniq[which.max(tabulate(match(df[, numcolschar], uniq)))]
                list_computed_missing_mode[[i]] <- ifelse(is.na(df[list_nachar[i]]), mode, df[, numcolschar])
              }
              char_missing <- data.frame(do.call(cbind, list_computed_missing_mode))
            }else{
              char_missing <- NULL
            }

            if(!is.null(numeric_nomissing) && !is.null(char_missing)){
              chnum <- colnames(numeric_nomissing)
              chachar <- colnames(char_missing)
              numum <- match(chnum, names(df))
              numchar <- match(chachar, names(df))
              changed_plus_rest <- cbind(numeric_nomissing, char_missing, df[, -c(numum, numchar)])
              changed_plus_rest <- changed_plus_rest[, names(df)]
              return(changed_plus_rest)
            }
          }
          ############
          else if(type == "Remove" && remove_choice == "column" && length(list_nanum) > 0 | type == "Remove" && remove_choice == "column" && length(list_nachar) > 0 ){
            dftype <- sapply(df, class)
            ischar <- grep("character|logical", dftype)
            col_nomissing <- na.omit(t(df[,-ischar]))
            col_nomissing <- data.frame(t(col_nomissing))
            nam <- na.omit(t(df[,ischar]))
            nam <- data.frame(t(nam))
            col_nomissing <- cbind(nam, col_nomissing)
            if(length(names(col_nomissing)) == length(names(df))){
              col_nomissing <- col_nomissing[, names(df)]
            }
            return(col_nomissing)
          }
          else if(type == "Remove" && remove_choice == "row" && length(list_nanum) > 0 | type == "Remove" && remove_choice == "row" && length(list_nachar) > 0 ){
            row_nomissing <- na.omit(df)
            return(row_nomissing)
          }

          else if(type == "No-action"){
            return(df)
          }
        }
        misstablereactive <- missing(df = uploaded_predata, type = input$missingtype, method = input$missingmethod, remove_choice = input$roworcolumn)
        misstablereactive <- data.frame(misstablereactive)
        misstablereactive
      }
    })
    #####################################################################
    outlier_imputation <- reactive({
      if(input$oulieractionaction && !is.null(table_outliers_imputation()) && !is.null(input$outliertransformation) && NCOL(table_outliers_imputation()) > 1){

        outliers <- input$outliertransformation
        table_outliers_imputation <- isolate(table_outliers_imputation())
        table_outliers_imputation <- data.frame(table_outliers_imputation)

        common_names <- which(names(table_outliers_imputation) %in% outliers)
         if(length(common_names) > 0 && input$outliermethod != "No-action"){
          table_outliers <- data.frame(table_outliers_imputation[, outliers])
          colnames(table_outliers) <- outliers
          new_varaiable <- list()
          for(i in 1:length(outliers)){
            imputed_outlier <- imputate_outlier(table_outliers, outliers[i], method = input$outliermethod)
            new_varaiable[[i]] <- imputed_outlier[1:nrow(table_outliers)]
          }

          impu_outl <- data.frame(do.call(cbind, new_varaiable))
          colnames(impu_outl) <- outliers
          if(length(names(table_outliers_imputation())) > length(outliers)) {
            colnum <- which(names(table_outliers_imputation()) %in% outliers)
            remained <- names(table_outliers_imputation())[-colnum]
            impu_outl <- cbind(table_outliers_imputation()[, remained], impu_outl)
            names(impu_outl)[1] <- remained
            impu_outl <- impu_outl[, names(table_outliers_imputation())]
          }
         } 
        else if(!is.null(table_outliers_imputation) && input$outliermethod == "No-action"){
          return(table_outliers_imputation)
        }
        else if(length(common_names) == 0){
          return(NULL)
        }
    

    }
  })
  ############################ outlier imputing table ########################################
  output$outlierprerocessingaction <- renderDT({
    if(!is.null(table_outliers_imputation()) && input$oulieractionaction){
      outlier_imputation <- isolate(outlier_imputation())
      outlier_imputation %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE, pageLength = 5),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE
        )
    }
  })


   ############################# hide x^number degree panel
  output$hide_timesnumber_degree <- reactive({
    hideselect <- function(){
      if(input$datatransformation == "x^number"){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    hideselect()
  })
  outputOptions(output, 'hide_timesnumber_degree', suspendWhenHidden=FALSE)
 
  ###############################################################################################
  ######################## making a reactive for transformation numeric #########################
  datatransformation_reactive_numeric <- reactive({
    if(!is.null(outlier_imputation()) && input$numerictransformationaction){
      numeric_imputation <- isolate(outlier_imputation())
      dftype <- sapply(numeric_imputation, class)
      isnumeric <- grep("numeric", dftype)
      ischar <- grep("character|logical", dftype)
      num_var <- numeric_imputation[, isnumeric]
      num_var <- as.matrix(num_var)
      transform_num <- function(df, method){
        if(!is.null(df)){

          if(method == "log"){
            result <- log(df)
            return(result)
          }
          else if(method == "log2"){
            result <- log2(df)
            return(result)
          }
          else if(method == "log10"){
            result <- log10(df)
            return(result)
          }
          else if(method == "log+1"){
            result <- log(df + abs(min(df, na.rm = T)) + 1)
            return(result)
          }
          else if(method == "zscore"){
            result <- (df - mean(df, na.rm = TRUE))/sd(df, na.rm = TRUE)
            return(result)
          }
          else if(method == "minmax"){
            result <- (df - min(df, na.rm = TRUE))/(max(df, na.rm = TRUE) - min(df, na.rm = TRUE))
            return(result)
          }
          else if(method == "sqrt"){
            result <- sqrt(df)
            return(result)
          }
          else if(method == "x^number"){
            result <- df^input$timenumbwer
            return(result)
          }
          else if(method == "No-action"){
            return(df)
          }
        }
      }
      num_trans <- transform_num(df= num_var, method = input$datatransformation)
      if(length(ischar) > 0 && !is.null(num_trans)) {
        combined <- cbind(outlier_imputation()[ischar], num_trans)
        combined <- combined[, names(outlier_imputation())]
      } else{
        num_trans
      }
    }
  })
  ############# numeric transformation table ################
  output$numerictransformationtable <- renderDT({
    if(input$numerictransformationaction){
      datatransformation_reactive_numeric <- isolate(datatransformation_reactive_numeric())
      datatransformation_reactive_numeric %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE, pageLength = 5),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE
        )
    }
  })


   ############################################### character transformation choices ################################
  datatransformation_reactive_char_choices <- reactive({
    if(!is.null(datatransformation_reactive_numeric()) && input$numerictransformationaction){
      data_Tnumeric <- isolate(datatransformation_reactive_numeric())
      dftype <- sapply(data_Tnumeric, class)
      ischar <- grep("character|logical", dftype)
      char_var <- data.frame(data_Tnumeric[, ischar])
      char_var_names <- names(char_var)
      #char_var_names <- levels(char_var)

      if(length(char_var_names) == 1){
        char_var_names <- names(data_Tnumeric)[ischar]
        char_var_names
      }else {
        char_var_names
      }
    }
  })

 #################################################### choices for character selection ############
  observeEvent(datatransformation_reactive_char_choices(),{
    if(!is.null(datatransformation_reactive_char_choices())){

      updatePickerInput(
        session,
        "charvariablestotransform",
        choices = datatransformation_reactive_char_choices() )
    }
  })

  ###################################### displaying the level of choices
  char_transformation <- reactive({
    if(!is.null(datatransformation_reactive_numeric()) && input$chartransformationaction){
      data_Tnumeric <- isolate(datatransformation_reactive_numeric())
      char_list <- input$charvariablestotransform
      if(length(char_list) > 0) {
        char_factor_list <- list()
        for(i in 1:length(char_list)){
          levels <- levels(factor(data_Tnumeric[,char_list[i]]))
          char_factor_list[[i]] <- factor(data_Tnumeric[,char_list[i]],
                                          levels = levels,
                                          labels = c(1:length(levels)))
        }
        char_factor <- data.frame(do.call(cbind, char_factor_list))
        names(char_factor) <- char_list
        colnum <- which(names(data_Tnumeric) %in% char_list)
        remained <- names(data_Tnumeric)[-colnum]
        char_factor <- cbind(data_Tnumeric[, remained], char_factor)
        char_factor <- char_factor[, names(data_Tnumeric)]
        char_factor
      }
      else if(length(char_list) == 0 && !is.null(data_Tnumeric) && input$chartransformationaction){
        return(data_Tnumeric)
      }

    }
  })


  output$chartransformationtofactor<- renderDT({
    if(input$chartransformationaction){
      char_transformation <- isolate(char_transformation())
      char_transformation %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE, pageLength = 5),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE
        )
    }
  })

 ####################### download the final table 

  output$download_preprocess_table <- renderUI({
    if(input$chartransformationaction) {
      downloadButton('fpreprocess_table', 'Download table')
    }
  })

  output$fpreprocess_table <- downloadHandler(

    filename = function(){
      paste0(Sys.Date(),"_final_preprocessing_table.csv")
    },
    content = function(file){
      char_transformation() %>%
        write_csv(file)
    }
  )

  ##################Download example data 
  output$download_expression_exmp <- renderUI({
    # if(input$anaylse_data_display) {
      downloadButton('ex_exampledata', 'Download example')
  })
  
  output$ex_exampledata <- downloadHandler(
    filename = function(){
      paste0("GSE26927_series.csv")
    },
      content = function(file){
        file.copy("data/GSE26927_series.csv",
                  file)
        
      }
  )
  
  
  
 ##################################################### Expression analysis ############################## 
  uploaded_expression_table <- reactive({
    if(is.null(input$uploaded_expression_table))
    {
      return(NULL)
    }
    expression_table <- try(read.table(input$uploaded_expression_table$datapath,
                                            sep= input$data_t,
                                            row.names = 1,
                                            header = TRUE),silent = TRUE)

    groups <- apply(expression_table[1,], 2, as.factor)
    expression_table <- data.frame(sapply(expression_table[-1,], function(x) as.numeric(as.character(x))),
                                   check.names=F, row.names = rownames(expression_table[-1,]))
    group1 <- levels(groups)[1]
    group2 <- levels(groups)[2]
    ng1 <- length(which(str_count(groups, group1) == 1))
    ng2 <- length(which(str_count(groups, group2) == 1))
    # check for normality
    qx <- expression_table %>%
      quantile(c(0., 0.25, 0.5, 0.75, 0.99, 1.0),
               na.rm=T)
    LogC <- (qx[5] > 100) || (qx[6]-qx[1] > 50 && qx[2] > 0)
    if(LogC) {
      expression_table <- log2(expression_table)
    }
    
    # building design matrix and fit the model
    disease <- factor(rep(c(0,1),c(ng1, ng2)))
    design <- model.matrix(~disease+ 0)
    colnames(design) <- c(group1, group2)
    fit1 <- lmFit(expression_table, design)
    cts <- paste(group2, group1, sep="-")
    cont.matrix <- makeContrasts(contrasts=cts, levels=design)
    fit2 <- contrasts.fit(fit1, cont.matrix)
    fit2 <- eBayes(fit2, 0.01)
    diffT <- topTable(fit2, adjust = input$adjpvalmethod, number=Inf)
    diffT$ID <- rownames(diffT)
    diffT <- diffT[, c("ID", "logFC", "adj.P.Val", "P.Value", "AveExpr", "t", "B")]
    diffT
    
  })
  
  ############################################### Gene Network ################
  output$genenetwork_analysis <- renderPlot({
    if(!is.null(uploaded_expression_table()) && !is.null(groupchecking()) && input$genenetworkeaction){
      ex_set <- isolate(groupchecking())
      ex_set <- ex_set[-1,]
      ex_degs <- isolate(uploaded_expression_table())
      ex_degs <- ex_degs[,c(1, 2 ,3, 4)]
      
      if(input$genenetworkpvalue == "Pvalue"){
        pvalue_method <- ex_degs %>%
          mutate(Pvalue = -log10(P.Value)) %>%
          mutate(DEG = "Not") %>%
          mutate(DEG = ifelse(Pvalue > input$genenetworksignificance & logFC > input$genenetworklogfcup , "UpRegulated", DEG)) %>%
          mutate(DEG = ifelse(Pvalue > input$genenetworksignificance & logFC < input$genenetworklogfcdown , "DownRegulated", DEG))
        up <- pvalue_method[which(pvalue_method$DEG == "UpRegulated"),]
        up <-  up[order(up$logFC, decreasing = T),1][1:input$genenetwork_degsnumber]
        down <- pvalue_method[which(pvalue_method$DEG == "UpRegulated"),]
        down <-  down[order(down$logFC, decreasing = T),1][1:input$genenetwork_degsnumber]
        genelist <- melt(cbind(up, down))[,3]
        num <- which(rownames(ex_set) %in% genelist)
        network_df <- data.frame(ex_set[num,])
        rows <- rownames(network_df)
        network_df <- as.data.frame(sapply(network_df, as.numeric))
        rownames(network_df) <- rows
        network_df <- data.frame(network_df)
        network_df
      }
      else if(input$genenetworkpvalue == "AdjustedPvalue"){
        pdjvalue_method <- ex_degs %>%
          mutate(Pdjvalue = -log10(adj.P.Val)) %>%
          mutate(DEG = "Not") %>%
          mutate(DEG = ifelse(Pdjvalue > input$genenetworksignificance & logFC > input$genenetworklogfcup , "UpRegulated", DEG)) %>%
          mutate(DEG = ifelse(Pdjvalue > input$genenetworksignificance & logFC < input$genenetworklogfcdown , "DownRegulated", DEG))
        up <- pdjvalue_method[which(pdjvalue_method$DEG == "UpRegulated"),]
        up <-  up[order(up$logFC, decreasing = T),1][1:input$genenetwork_degsnumber]
        down <- pdjvalue_method[which(pdjvalue_method$DEG == "UpRegulated"),]
        down <-  down[order(down$logFC, decreasing = T),1][1:input$genenetwork_degsnumber]
        genelist <- melt(cbind(up, down))[,3]
        num <- which(rownames(ex_set) %in% genelist)
        network_df <- data.frame(ex_set[num,])
        rows <- rownames(network_df)
        network_df <- as.data.frame(sapply(network_df, as.numeric))
        rownames(network_df) <- rows
        network_df <- data.frame(network_df)
        network_df
      }
      if(NROW(network_df) > 0){
        set.seed(123)
        g <- graph.adjacency(
          as.matrix(as.dist(cor(t(network_df), method=input$genenetworkcormethod))),
          mode=input$genenetworkmode,
          weighted = TRUE,
          diag= FALSE
        )
        
        g <- simplify(g,
                      remove.multiple = TRUE,
                      remove.loops = TRUE)
        # Colour negative correlation edges as blue
        E(g)[which(E(g)$weight<0)]$color <- "darkblue"
        # Colour positive correlation edges as red
        E(g)[which(E(g)$weight>0)]$color <- "darkred"
        E(g)$weight <- abs(E(g)$weight)
        # Remove edges below absolute Pearson correlation 0.8
        g <- delete_edges(g, E(g)[which(E(g)$weight< input$genenetworkcordegree)])
        g <- delete.vertices(g, degree(g)==0)
        V(g)$name <- V(g)$name
        V(g)$shape <- "sphere"
        V(g)$color <- "skyblue"
        V(g)$vertex.frame.color <- "white"
        
        # Scale the size of the vertices to be proportional to the level of expression of each gene represented by each vertex
        # Multiply scaled vales by a factor of 10
        scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
        vSizes <- (scale01(apply(network_df, 1, mean)) + 1.0) * 10
        # Amplify or decrease the width of the edges
        edgeweights <- E(g)$weight * 2
        # Convert the graph adjacency object into a minimum spanning tree based on Prim's algorithm
        mst <- mst(g, algorithm="prim")
        mst.communities <- edge.betweenness.community(mst, weights=NULL, directed=FALSE)
        mst.clustering <- make_clusters(mst, membership=mst.communities$membership)
        V(mst)$color <- mst.communities$membership + 1
        if(input$networklayout == "layout.auto"){
          layout <- layout.auto
        }
        else if(input$networklayout == "layout.random"){
          layout <- layout.random
        }
        else if(input$networklayout == "layout.circle"){
          layout <- layout.circle
        }
        else if(input$networklayout == "layout.fruchterman.reingold"){
          layout <- layout.fruchterman.reingold
        }
        else if(input$networklayout == "layout.kamada.kawai"){
          layout <- layout.kamada.kawai
        }
        else if(input$networklayout == "layout.spring"){
          layout <- layout.spring
        }
        else if(input$networklayout == "layout.reingold.tilford"){
          layout <- layout.reingold.tilford
        }
        else if(input$networklayout == "layout.fruchterman.reingold.grid"){
          layout <- layout.fruchterman.reingold.grid
        }
        else if(input$networklayout == "layout.lgl"){
          layout <- layout.lgl
        }
        else if(input$networklayout == "layout.graphopt"){
          layout <- layout.graphopt
        }
         else if(input$networklayout == "layout.mds"){
           layout <- layout.mds
         }
         else if(input$networklayout == "layout.svd"){
           layout <- layout.svd
         }
        
        plot(
          mst.clustering, mst,
          layout=layout,
          edge.curved= TRUE,
          vertex.size=vSizes,
          vertex.label.dist= - input$genenetworklabledist,
          vertex.label.color="black",
          asp=FALSE,
          vertex.label.cex= input$genenetworklablesize,
          edge.width=edgeweights,
          edge.arrow.mode=0,
          main="Gene network")
      }

    }
  })
  
########################################################### Heatmap ##############################
  output$heatmap_analysis <- renderPlot({
    if(!is.null(uploaded_expression_table()) && !is.null(groupchecking()) && input$heatmapaction){
      ex_set <- isolate(groupchecking())
      ex_set <- ex_set[-1,]
      ex_degs <- isolate(uploaded_expression_table())
      ex_degs <- ex_degs[,c(1, 2 ,3, 4)]
      
      if(input$heatmappvalue == "Pvalue"){
        pvalue_method <- ex_degs %>%
          mutate(Pvalue = -log10(P.Value)) %>%
          mutate(DEG = "Not") %>%
          mutate(DEG = ifelse(Pvalue > input$heatmapsignificance & logFC > input$heatmaplogfcup , "UpRegulated", DEG)) %>%
          mutate(DEG = ifelse(Pvalue > input$heatmapsignificance & logFC < input$heatmaplogfcdown , "DownRegulated", DEG))
        up <- pvalue_method[which(pvalue_method$DEG == "UpRegulated"),]
        up <-  up[order(up$logFC, decreasing = T),1][1:input$heatmap_degsnumber]
        down <- pvalue_method[which(pvalue_method$DEG == "UpRegulated"),]
        down <-  down[order(down$logFC, decreasing = T),1][1:input$heatmap_degsnumber]
        genelist <- melt(cbind(up, down))[,3]
        num <- which(rownames(ex_set) %in% genelist)
        network_df <- data.frame(ex_set[num,])
        rows <- rownames(network_df)
        network_df <- as.data.frame(sapply(network_df, as.numeric))
        network_df <- as.matrix(network_df)
        rownames(network_df) <- rows
        network_df
      }
      else if(input$heatmappvalue == "AdjustedPvalue"){
        pdjvalue_method <- ex_degs %>%
          mutate(Pdjvalue = -log10(adj.P.Val)) %>%
          mutate(DEG = "Not") %>%
          mutate(DEG = ifelse(Pdjvalue > input$heatmapsignificance & logFC > input$heatmaplogfcup , "UpRegulated", DEG)) %>%
          mutate(DEG = ifelse(Pdjvalue > input$heatmapsignificance & logFC < input$heatmaplogfcdown , "DownRegulated", DEG))
        up <- pdjvalue_method[which(pdjvalue_method$DEG == "UpRegulated"),]
        up <-  up[order(up$logFC, decreasing = T),1][1:input$heatmap_degsnumber]
        down <- pdjvalue_method[which(pdjvalue_method$DEG == "UpRegulated"),]
        down <-  down[order(down$logFC, decreasing = T),1][1:input$heatmap_degsnumber]
        genelist <- melt(cbind(up, down))[,3]
        num <- which(rownames(ex_set) %in% genelist)
        network_df <- data.frame(ex_set[num,])
        rows <- rownames(network_df)
        network_df <- as.data.frame(sapply(network_df, as.numeric))
        network_df <- as.matrix(network_df)
        rownames(network_df) <- rows
        network_df
      }
      type <- colnames(network_df)
      ha = HeatmapAnnotation(
        df = data.frame(Samples = type),
        annotation_height = unit(4, "mm")
      )
      par(mar= c(4,4,5,5))
      qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
      col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
      col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
      Heatmap(network_df, name = "Expression", km = input$heatmapclusternumber,
              bottom_annotation= ha,
              show_row_names = TRUE,
              row_title = "DEGs",
              row_title_side = "left",
              show_heatmap_legend = TRUE,
              heatmap_legend_param = list(legend_direction = "vertical",
                                          legend_height = unit(50, "mm"),
                                          grid_width = unit(6, "mm"),
                                          grid_height = unit(50, "cm")
              ),
              col = col_vector,
              border_gp = gpar(col = "black"),
              row_names_gp = gpar(fontsize = input$heatmapgenelabesize),
              show_column_names = FALSE) 
    }
    })      
  ######################################### group checking ################
  groupchecking <- reactive({
    # Ensure that value are available
    req(input$uploaded_expression_table)
    #eror_cautgh(input$uploaded_expression_table$name, input$data_t)
    groupchecking <- try(read.table(input$uploaded_expression_table$datapath,
                        sep= input$data_t,
                        row.names = 1,
                        header = TRUE),silent = TRUE)
    groupchecking
  })
  
  ###################################### show processing massage ###########################
  
  # processing <- reactiveValues(ok = FALSE)
  # 
  # observeEvent(input$anaylse_data_display, {
  #   shinyjs::disable("anaylse_data_display")
  #   shinyjs::show("process")
  #   processing$ok <- FALSE
  #   # do some cool and complex stuff
  #   Sys.sleep(3)
  #   processing$ok <- TRUE
  #   shinyjs::enable("anaylse_data_display")
  #   shinyjs::hide("process")
  # })
  
  # output$plot <-renderPlot({
  #   if (processing$ok) {
  #     shinyjs::enable("anaylse_data_display")
  #     shinyjs::hide("process")
  #     hist(rnorm(100, 4, 1),breaks = 50)
  #   }
  # })
  
  
  
  
  ########################################### Hiding table box  and producing error ############
  output$fileUploaded <- reactive({
    error_catghing <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      checkfile <- function(){
        if(sepa == ext){
          lngfile <- function(){
            if(!NCOL(groupchecking()) > 1){
              return(FALSE)
            } else {
              lg <- length(unique(unlist(groupchecking()[1,])))
              eg <- grepl("Group|group", rownames(groupchecking())[1]) ==TRUE
              return(list(lg, eg))
            } 
          }
          result <- lngfile()
          return(result )
        } else{
          return(FALSE)
        }
      }
       table <- !is.null(input$uploaded_expression_table)
      if(table && checkfile() == 2 && unlist(checkfile()[2]) == TRUE && unlist(checkfile()[1]) == 2){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing(input$uploaded_expression_table$name, input$data_t)
  })
  # 
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  ################################ TABLE ######################################
  # download top table
  output$download_diff_table <- renderUI({
    if(input$anaylse_data_display) {
      downloadButton('diff_table', 'Download table')
    }
  })
  
  output$diff_table <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_diff_table.csv")
    },
    content = function(file){
      uploaded_expression_table() %>%
        write_csv(file)
    }
  )
  
  # Top table
  output$DeepBC <- renderDT({
    req(input$uploaded_expression_table)
    #eror_cautgh(input$uploaded_expression_table$name, input$data_t)
    # show table when user clicked display result
    # if(input$uploaded_expression_table()) { 
      diff_table <- uploaded_expression_table() %>%
        datatable(extensions = "Buttons",
                  options = list(autoWidth = FALSE,scrollX = TRUE,
                                 buttons =c("excel", "csv"),
                                 dom = "lBfrtip"),
                  filter = list(position = 'top', clear = FALSE),
                  rownames = FALSE)
      diff_table
    # }
  })
  
  
  ####################################################### Box plot #######################
  output$download_box_plot <- renderUI({
    if(input$anaylse_data_display) {
      downloadButton('box_plot', 'Download box plot')
    }
  })
  output$box_plot <- downloadHandler(

    filename = function(){
      paste0(Sys.Date(),"_boxplot.jpeg")
    },
    content = function(file){
      file.copy("data/boxplot.jpeg",
                file)

    }
  )

  ex_ta <- reactive({
  # Ensure that value are available
  req(input$uploaded_expression_table)
  #eror_cautgh(input$uploaded_expression_table$name, input$data_t)
  ex_ta <- read.table(input$uploaded_expression_table$datapath,
                      sep= input$data_t,
                      row.names = 1,
                      header = TRUE)
  
  # eg <- grepl("Group|group", rownames(ex_ta)[1]) ==TRUE
  # lg <- length(unique(unlist(ex_ta[1,])))
  # if(eg == TRUE && lg == 2){
  # }else{stop("Please define groups")}
  #defining groups
  groups <- apply(ex_ta[1,], 2, as.factor)
  group1 <- levels(groups)[1]
  group2 <- levels(groups)[2]
  G1 <- colnames(ex_ta)[which(str_count(head(ex_ta,1), group1) == 1)]
  ex_ta <- data.frame(sapply(ex_ta[-1,], function(x) as.numeric(as.character(x))),
                      check.names=F, row.names = rownames(ex_ta[-1,]))
  ex_ta$ID <- rownames(ex_ta)

  ex_ta <- ex_ta %>%
    melt(id.vars="ID")
  ex_ta$Group <- ifelse(ex_ta$variable %in%  G1, group1,group2)
  ex_ta
})



  output$box_plot_plot <- renderPlot({
    req(input$uploaded_expression_table)
    if (input$boxplotaction && !is.null(ex_ta)) {
    boxp <- ex_ta() %>%
      ggplot(aes(x = variable, y = log(value))) +
      geom_boxplot(aes(color=Group, fill=Group)) +
      labs(x= 'SampleID', y= 'Values') + scale_fill_manual(values = c("#BB8FCE", "#BA4A00")) +
      theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                              axis.text = element_text(colour = "black",angle = 90),
                              axis.text.y = element_text(colour = "black")) +
      theme(axis.text = element_text(family = "Times",size = 13 , colour = "black"),
            axis.text.x = element_text(family = "Times",colour = "black", size = 13),
            axis.text.y = element_text(family = "Times",colour = "black"),
            plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
            axis.title.y = element_text(family = "Times", size = rel(1.4), angle = 90),
            axis.title.x = element_text(family = "Times", size = rel(1.4), angle = 00))+
      labs(subtitle = "     Gene expression boxplot")
    ggsave(filename = "boxplot.jpeg",
           path = "data/",
           width = 13,
           height = 9,
    )
    boxp
    }
  })
#################################################### MD plot ############################
  #download volcano plot under if conditions
  output$download_intensity_plot <- renderUI({
    if(input$anaylse_data_display) {
      downloadButton('intens_plot', 'Download MD plot')
    }
  })
  output$intens_plot <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_MD_plot.jpeg")
    },
    content = function(file){
      file.copy("data/MD_plot.jpeg",
                file)
      
    }
  )
  
  
  
  
  
  # Volcano plot
  output$intensity_plot <- renderPlot({
        req(input$uploaded_expression_table)
    if(input$intensityplotaction && !is.null(uploaded_expression_table)) {
      
        intensity <- uploaded_expression_table() %>%
          mutate(DEGs = "Not") %>%
          mutate(DEGs = ifelse(logFC > 1, "UpRegulated", DEGs)) %>%
          mutate(DEGs = ifelse(logFC < -1, "DownRegulated", DEGs)) %>%
          ggplot(aes(AveExpr, logFC)) +
          labs(x= 'log(mean intensity)', y= 'log2FC') +
          geom_point(aes(color=DEGs, fill=DEGs)) + scale_color_manual(values = c("#B03A2E", "#B2BABB", "#27AE60")) +
          theme_classic() + theme(axis.text = element_text(family = "Times",size = 13 , colour = "black"),
              axis.text.x = element_text(family = "Times",colour = "black", size = 13),
              axis.text.y = element_text(family = "Times",colour = "black"),
              plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
              axis.title.y = element_text(family = "Times", size = rel(1.4), angle = 90),
              axis.title.x = element_text(family = "Times", size = rel(1.4), angle = 00))+
        labs(subtitle = "            Mean difference (MD) plot")
      # save volcano
      ggsave(filename = "MD_plot.jpeg",
             path = "data/",
             width = 13,
             height = 9,
      )
      #ggplotly(intensity)
      intensity
      
    }
  })
################################################################################################
  #download volcano plot under if conditions
  output$download_volcano_plot <- renderUI({
    if(input$anaylse_data_display) {
      downloadButton('volc_plot', 'Download volcano plot')
    }
  })
  output$volc_plot <- downloadHandler(

    filename = function(){
      paste0(Sys.Date(),"_volcano.jpeg")
    },
    content = function(file){
      file.copy("data/volcano.jpeg",
                file)

    }
  )

  # Volcano plot
  output$volcano_plot <- renderPlot({
    req(input$uploaded_expression_table)
    if(input$volcanoaction && !is.null(uploaded_expression_table)) {
      
      volcano <- uploaded_expression_table() %>%
        mutate(Pvalue = -log10(P.Value)) %>%
        mutate(AdjustedPvalue = -log10(adj.P.Val)) %>%
        mutate(DEGs = "Not") %>%
        mutate(DEGs = ifelse(AdjustedPvalue > input$significance & logFC > input$logfcup, "UpRegulated", DEGs)) %>%
        mutate(DEGs = ifelse(AdjustedPvalue > input$significance & logFC < input$logfcdown, "DownRegulated", DEGs)) %>%
        mutate(labels = ifelse(DEGs != "Not", ID, "")) %>%
        mutate(DEG = "Not") %>%
        mutate(DEG = ifelse(Pvalue > input$significance & logFC > input$logfcup , "UpRegulated", DEG)) %>%
        mutate(DEG = ifelse(Pvalue > input$significance & logFC < input$logfcdown , "DownRegulated", DEG)) %>%
        mutate(label = ifelse(DEG != "Not", ID, "")) %>%
        ggplot(aes_string(x = "logFC", y= input$pval_adjpval, label=ifelse(input$pval_adjpval == "Pvalue", "label", "labels"))) +
        labs(x= 'log2FC', y= ifelse(input$pval_adjpval == "Pvalue", "-log10(P-value)","-log10(Adjusted P-value)")) +
        geom_point(aes_string(color=ifelse(input$pval_adjpval == "Pvalue", "DEG", "DEGs"),
                              fill=ifelse(input$pval_adjpval == "Pvalue", "DEG", "DEGs"))) + scale_color_manual(values = c("#B03A2E", "#B2BABB", "#27AE60")) +
        theme_classic() +
        geom_text(check_overlap = TRUE, vjust = 0, nudge_y = 0.1) +
        theme(axis.text = element_text(family = "Times",size = 13 , colour = "black"),
              axis.text.x = element_text(family = "Times",colour = "black", size = 13),
              axis.text.y = element_text(family = "Times",colour = "black"),
              plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
              axis.title.y = element_text(family = "Times", size = rel(1.4), angle = 90),
              axis.title.x = element_text(family = "Times", size = rel(1.4), angle = 00))+
      ggsave(filename = "volcano.jpeg",
             path = "data/",
             width = 13,
             height = 9,
      )
      volcano

    }
  })
  
  
  ######################################### confusion matrix #############################
  confusion_matrix <- function(y_true, y_pred){
    if(!is.null(y_true) && !is.null(y_pred)){
      cm <- table(y_true, y_pred)
      if(dim(cm)[1] == 2){
        Accuracy <- (cm[1,1] + cm[2,2])/(cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
        Precision <- (cm[1,1])/(cm[1,1] + cm[1,2])
        Sensitivity <- (cm[1,1])/(cm[1,1] + cm[2,1])
        Specificity <- (cm[2,2])/ (cm[2,2] + cm[1,2])
        AUC <- roc(as.numeric(y_true) ~ as.numeric(y_pred), quiet = T)$auc[1]
        result <- round(data.frame(Accuracy = Accuracy, Precision = Precision, Sensitivity = Sensitivity , Specificity = Specificity, AUC = AUC),3)
        return(result)
      }
      else if (NROW(cm) > 2){
        TP <- list()
        for(i in 1:NROW(cm)){
          TP[[i]] <- cm[i,i]
        }
        TP <- data.frame(do.call(rbind,TP))
        FN <- rowSums(cm) - TP[,1]
        FP <- colSums(cm) - TP[,1]
        TN <- list()
        for(i in 1:NROW(cm)){
          TN[[i]] <- sum(cm) - sum(cm[i,]) - sum(cm[,i]) + cm[i,i]
        }
        TN <- data.frame(do.call(rbind, TN))
        con <- cbind(TP, FN, FP, TN)
        colnames(con) <- c("TP", "FN", "FP", "TN")
        rownames(con) <- rownames(cm)
        a <- list()
        for (i in 1:NROW(con)) {
          a[[i]] <- (con$TP[i] + con$TN[i])/(con$TP[i] + con$TN[i] + con$FN[i] + con$FP[i])
        }
        p <- list()
        for (i in 1:NROW(con)) {
          p[[i]] <- con$TP[i]/(con$TP[i] + con$FP[i])
        }  
        se <- list()
        for (i in 1:NROW(con)) {
          se[[i]] <- con$TP[i]/(con$TP[i] + con$FN[i])
        }  
        sp <- list()
        for (i in 1:NROW(con)) {
          sp[[i]] <- con$TN[i]/(con$TN[i] + con$FP[i])
        }  
        a <- do.call(rbind, a)
        p <- do.call(rbind, p)
        se <- do.call(rbind, se)
        sp <- do.call(rbind, sp)
        au <- multiclass.roc(as.numeric(y_true) ~ as.numeric(y_pred), quiet = T)$auc[1]
        au <- rep(au, length.out= NROW(con))
        result <- round(cbind(a, p, se, sp, au),3)
        colnames(result) <- c("Accuracy", "Precision", "Sensitivity", "Specificity", "AUC_average")
        rownames(result) <- rownames(cm)
        
        return(result)
      }
    }
  }
  ################################## averaging for multi-class ####################
  multiclass_con_av <- function(cv){
    mlm <- do.call(cbind, cv)
    colnames(mlm) <- gsub("Fold[0-9]{1,2}.", "", colnames(mlm))
    acc <- list()
    for(i in 1:5){
      name <- unique(colnames(mlm))[i]
      num <- grep(name, colnames(mlm))
      acc[[i]] <- rowMeans(mlm[,num])
      
    }
    result <- round(do.call(cbind, acc),3)
    colnames(result) <- unique(colnames(mlm))
    return(result)
  }
  ################################################################# Feature selection #######################################
  #### error checking
  output$uploaded_featuersel_hide_tabpanel <- reactive({
    error_catghing_featuersel <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_featuersel)
      checknull <- NCOL(uploaded_featuersel_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_featuersel(input$uploaded_featuersel$name, input$datafeatuerselfileformat)
  })
  outputOptions(output, 'uploaded_featuersel_hide_tabpanel', suspendWhenHidden=FALSE)
  ################################### uploaded featuersel table ######################################################
  uploaded_featuersel_data <- eventReactive(input$uploaded_featuersel, {
    if(!is.null(input$uploaded_featuersel)){
      if(input$datafeatuerselfileformat == "," && input$featuerselheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_featuersel$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$datafeatuerselfileformat == "," && input$featuerselheader == "No") {
        rawdata <- try(read_csv(input$uploaded_featuersel$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datafeatuerselfileformat == "\t" && input$featuerselheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_featuersel$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datafeatuerselfileformat == "\t" && input$featuerselheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_featuersel$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ########################################### featuersel table
  output$featuerseldatashow <- renderDT({
    if(!is.null(input$uploaded_featuersel) && input$featuerselprepross) {
      uploaded_featuersel_table <- uploaded_featuersel_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE)
      uploaded_featuersel_table
    }
  })
  ############################################# Boruta reactive ################################
  # hide ui panel for boruta
  output$hide_boruta_panel <- reactive({
      hideboruta <- function(){
        if(input$featureselemethods == "Boruta"){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
      hideboruta()
    })
    outputOptions(output, 'hide_boruta_panel', suspendWhenHidden=FALSE)
    #### for random forest
    output$hide_featureselectionrandomforest_panel <- reactive({
      hiderandom <- function(){
        if(input$featureselemethods == "Random forest"){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
      hiderandom()
    })
    outputOptions(output, 'hide_featureselectionrandomforest_panel', suspendWhenHidden=FALSE)
    
    #### for LASSO
    output$hide_featureselectionlasso_panel <- reactive({
      hidelasso <- function(){
        if(input$featureselemethods == "LASSO"){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
      hidelasso()
    })
    outputOptions(output, 'hide_featureselectionlasso_panel', suspendWhenHidden=FALSE)
    ################ hide stepwise regression ##############################
    output$hide_stepwiseregression_panel <- reactive({
      hidestepwise <- function(){
        if(input$featureselemethods == "Stepwise regression"){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
      hidestepwise()
    })
    outputOptions(output, 'hide_stepwiseregression_panel', suspendWhenHidden=FALSE)
    #######################################################################
    ##################################### boruta choices
    # response
    observeEvent(uploaded_featuersel_data(),{
      if(!is.null(uploaded_featuersel_data())){
        response <- names(uploaded_featuersel_data())
        updateSelectInput(
          session,
          "boruta_response",
          choices = response)
      }
    })
    ### predictors
    observeEvent(uploaded_featuersel_data(),{
      if(!is.null(uploaded_featuersel_data())){
        dftype <- sapply(uploaded_featuersel_data(), class)
        isnumeric <- grep("numeric", dftype)
        predictors <- names(uploaded_featuersel_data())[isnumeric]
        updatePickerInput(
          session,
          "boruta_predictors",
          choices = predictors)
      }
    })
  boruta_reactive <- reactive({
    if(!is.null(uploaded_featuersel_data())){
      uploaded_featuersel_data <- isolate(uploaded_featuersel_data())
      uploaded_featuersel_data <- data.frame(uploaded_featuersel_data)
      response <- input$boruta_response
      predictors <- input$boruta_predictors
      res_name <- which(names(uploaded_featuersel_data) %in% response)
      pred_name <- which(names(uploaded_featuersel_data) %in% predictors)
      if(length(res_name) > 0 & length(pred_name) > 0 ){
        boruta_df <- data.frame(uploaded_featuersel_data[, c(response, predictors)])
        boruta_df[,1] <- factor(boruta_df[,1])
        boruta_df[-1] <- scale(boruta_df[-1])
        boruta_df <- data.frame(boruta_df)
      }
    }
  })

  boruta_analysis <- reactive({
    if(!is.null(boruta_reactive()) && input$boruta_start){
      boruta_df <- isolate(boruta_reactive())
      if(input$boruta_mcAdj == "TRUE"){
        mcAdj <- TRUE
      } else{
        mcAdj <- FALSE
      }
      formula <- as.formula(paste0(names(boruta_df)[1], " ~ ."))
      boruta_output <- Boruta(formula = formula,
                              data=na.omit(boruta_df),
                              pValue = input$boruta_pvalue,
                              mcAdj = mcAdj,
                              maxRuns = input$boruta_maxrun,
                              doTrace=1)
      roughFixMod <- TentativeRoughFix(boruta_output)
      imps <- attStats(roughFixMod)
      imps2 <- imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
      imps2 <- data.frame(imps2[order(-imps2$meanImp), ])
      
      result <- list(Boruta_Output = boruta_output , Boruta_Significant_FixedTentative = imps2)
      result
    }
  })
  output$boruta_output <- renderPrint({
    print(boruta_analysis()[[1]])
  })
  output$boruta_output1 <- renderDT({
    if(!is.null(boruta_analysis())){
      imp <- boruta_analysis()[[2]]
      imp %>%
      datatable(
        options = list(autoWidth = TRUE,scrollX = TRUE, dom = "rltip", pageLength=5),
        fillContainer = F,
        rownames = TRUE)
    }
  })
  ############################# download boruta 
  output$download_boruta_result <- renderUI({
    if(input$boruta_start) {
      downloadButton('borutaimp', 'Download importance variables (fixed tentative)')
    }
  })
  output$borutaimp <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_boruta_importance.csv")
    },
    content = function(file){
      boruta_analysis()[[2]] %>%
        write.csv(file, row.names = T, quote = F)
    }
  )
  ######################################################## Feature selection LASSO
  output$hide_lassotype_panel <- reactive({
    hidelassotype <- function(){
      if(input$lasso_type == "Classification"){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    hidelassotype()
  })
  outputOptions(output, 'hide_lassotype_panel', suspendWhenHidden=FALSE)
  
  
  
  ################################## Feature selection- Random forest ############################################
  ################################## hiding panels
  output$hide_featureselrandomforest_classification <- reactive({
    hidefeatureselrandomforestification <- function(){
      if(input$featureselrandomforesttype == "Classification"){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    hidefeatureselrandomforestification()
  })
  outputOptions(output, 'hide_featureselrandomforest_classification', suspendWhenHidden=FALSE)
  
  # response and predictor choices
  observeEvent(uploaded_featuersel_data(), {
    if(!is.null(uploaded_featuersel_data())){
      response_var <- names(uploaded_featuersel_data())
      updateSelectInput(
        session,
        "featureselrandomforestresponse",
        choices = response_var)
    }
  })
  # numeric
  observeEvent(uploaded_featuersel_data(), {
    if(!is.null(uploaded_featuersel_data())){
      dftype <- sapply(uploaded_featuersel_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_vars <- names(uploaded_featuersel_data())[isnumeric]
      updatePickerInput(
        session,
        "featureselrandomforestpredictors_numeric",
        choices = predictor_vars)
    }
  })
  # categorical
  observeEvent(uploaded_featuersel_data(), {
    if(!is.null(uploaded_featuersel_data())){
      dftype <- sapply(uploaded_featuersel_data(), class)
      ischar <- grep("character|logical", dftype)
      predictor_vars <- names(uploaded_featuersel_data())[ischar]
      updatePickerInput(
        session,
        "featureselrandomforestpredictors_categorical",
        choices = predictor_vars)
    }
  })
  
  ################################################ random forest parameters hidin
  output$hide_featureselrandomforest_classification <- reactive({
    hidefeatureselrandomforestification <- function(){
      if(input$featureselrandomforesttype == "Classification"){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    hidefeatureselrandomforestification()
  })
  outputOptions(output, 'hide_featureselrandomforest_classification', suspendWhenHidden=FALSE)
  #################################### reactive of variables
  featureselrandomforest_reactive <- reactive({
    if(!is.null(uploaded_featuersel_data())){
      uploaded_featuersel_data <- isolate(uploaded_featuersel_data())
      response <- input$featureselrandomforestresponse
      predictors_numeric <- input$featureselrandomforestpredictors_numeric
      predictors_categorical <- input$featureselrandomforestpredictors_categorical
      res_name <- which(names(uploaded_featuersel_data) %in% response)
      predic_num_name <- which(names(uploaded_featuersel_data) %in% predictors_numeric)
      predic_cat_name <- which(names(uploaded_featuersel_data) %in% predictors_categorical)
      if(length(res_name) > 0 | length(predic_num_name) > 0 | length(predic_cat_name) > 0 ){
        featureselrandomforest_df <- data.frame(uploaded_featuersel_data[, c(predictors_numeric, predictors_categorical, response)])        
      } else{
        return(NULL)
      }    
    }
  })
  ######################################## mtry number ##################################
  # classification
  observeEvent(featureselrandomforest_reactive(),{
    if(!is.null(featureselrandomforest_reactive())){
      df_length <- length(featureselrandomforest_reactive()) -1
      valuemtry <- round(sqrt(df_length))
      minmtry <- valuemtry
      maxmtry <- df_length
      updateNumericInput(session = session,
                         inputId = "featureselrandomforestmtryclass",
                         value = valuemtry,
                         min = valuemtry,
                         max = maxmtry, step = 1
      )
    }
  })
  # regression
  observeEvent(featureselrandomforest_reactive(),{
    if(!is.null(featureselrandomforest_reactive())){
      df_length <- length(featureselrandomforest_reactive()) -1
      valuemtry <- round(df_length/3)
      if(valuemtry > 0){
        minmtry <- valuemtry
        maxmtry <- df_length
      } else{
        valuemtry <- 1
        maxmtry <- 1
      }
      updateNumericInput(session = session,
                         inputId = "featureselrandomforestmtryreg",
                         value = valuemtry,
                         min = valuemtry,
                         max = maxmtry, step = 1
      )
    }
  })
  ################################### choice for strata
  observeEvent(featureselrandomforest_reactive(), {
    if(!is.null(featureselrandomforest_reactive())){
      dftype <- sapply(featureselrandomforest_reactive(), class)
      ischar <- grep("character|logical", dftype)
      strat <- names(featureselrandomforest_reactive())[ischar]
      updatePickerInput(
        session,
        "featureselrandomforeststrata",
        choices = strat)
    }
  })
  ################################# sample size number
  observeEvent(featureselrandomforest_reactive(),{
    if(!is.null(featureselrandomforest_reactive())){
      df_NROW <- NROW(featureselrandomforest_reactive()) 
      updateNumericInput(session = session,
                         inputId = "featureselrandomforestsampsize",
                         value = round(.3*df_NROW),
                         min = 1,
                         max = df_NROW, step = 1
      )
    }
  })
  ################ featureselrandomforest analysis 
  featureselrandomforest_analysis <- reactive({
    if(!is.null(uploaded_featuersel_data()) && input$featureselrandomforestaction){
      if(is.null(input$featureselrandomforestresponse) | is.null(input$featureselrandomforestpredictors_categorical) && is.null(input$featureselrandomforestpredictors_numeric)){
        return(NULL)
      }
      uploaded_featuersel_data <- isolate(uploaded_featuersel_data())
      response <- input$featureselrandomforestresponse
      predictors_numeric <- input$featureselrandomforestpredictors_numeric
      predictors_categorical <- input$featureselrandomforestpredictors_categorical
      res_name <- which(names(uploaded_featuersel_data) %in% response)
      predic_num_name <- which(names(uploaded_featuersel_data) %in% predictors_numeric)
      predic_cat_name <- which(names(uploaded_featuersel_data) %in% predictors_categorical)
      if(length(res_name) > 0 | length(predic_num_name) > 0 | length(predic_cat_name) > 0 ){
        featureselrandomforest_df <- data.frame(uploaded_featuersel_data[, c(predictors_numeric, predictors_categorical, response)])
        fac <- length(featureselrandomforest_df)
      } else{
        return(NULL)
      }
      ############################################## Classification ##############################
      
      if(input$featureselrandomforesttype == "Classification"){
        featureselrandomforest_df[,fac] <- factor(featureselrandomforest_df[,fac])
        cat_variable <- which(names(featureselrandomforest_df) %in% predictors_categorical)
        if(length(cat_variable) > 0){
          for(i in predictors_categorical){
            featureselrandomforest_df[, i] <- factor(featureselrandomforest_df[, i])
          }
        }
        factors <- which(names(featureselrandomforest_df) %in% predictors_categorical)
        if(length(factors) > 0){
          featureselrandomforest_df[c(-fac, -factors)] <- scale(featureselrandomforest_df[c(-fac, -factors)])
        }
        else{
          featureselrandomforest_df[-fac] <- scale(featureselrandomforest_df[-fac])
        }
        ########## class weights
        userclass <- input$featureselrandomforestclasswt
        nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
        nu_u_c <- na.omit(nu_u_c)
        classwt <- table(featureselrandomforest_df[,fac])
        if(length(nu_u_c) == length(classwt)){
          for(i in 1:length(nu_u_c)){
            classwt[i] <- nu_u_c[i]
          }
          
        } else{
          classwt <- NULL
          
        }
        ############## cutoff
        usercutoff <- input$featureselrandomforestcutoff
        pro_u_cu <- as.numeric(unlist(str_split(usercutoff, ",")))
        pro_u_cu <- na.omit(pro_u_cu)
        cutoff <- table(featureselrandomforest_df[,fac])
        if(length(pro_u_cu) == length(cutoff)){
          for(i in 1:length(pro_u_cu)){
            cutoff[i] <- pro_u_cu[i]
          }
        } else{
          class <- length(levels(factor(featureselrandomforest_df[,fac])))
          for(i in 1:class){
            cutoff[i] <- 1/class
          }
        }
        
        if(input$featureselrandomforestreplace == "FALSE"){
          replace <- FALSE
        }else{
          replace <- TRUE
        }
        if(input$featureselrandomforestproximity == "FALSE"){
          proximity <- FALSE
        } else {
          proximity <- TRUE
        }
        if(input$featureselrandomforestoob.prox == "FALSE"){
          oob.prox <- FALSE
        } else {
          oob.prox <- TRUE
        }
        if(input$featureselrandomforestcorr.bias == "FALSE"){
          corr.bias <- FALSE
        } else {
          corr.bias <- TRUE
        }
        if(input$featureselrandomforestimportance == "FALSE"){
          importance <- FALSE
        } else{
          importance <- TRUE
        }
        if(input$featureselrandomforestlocalImp == "FALSE"){
          localImp <- FALSE
        } else{
          localImp <- TRUE
        }  
        if(input$featureselrandomforestnorm.votes == "FALSE"){
          norm.votes <- FALSE
        } else{
          norm.votes <- TRUE
        }
        set.seed(123)
        fsrandvarimp <- randomForest(x = featureselrandomforest_df[-fac],
                                     y = featureselrandomforest_df[, fac],
                                     ntree = input$featureselrandomforestntree,
                                     mtry = input$featureselrandomforestmtryclass,
                                     replace = replace,
                                     classwt = classwt,
                                     cutoff = cutoff,
                                     strata = input$featureselrandomforeststrata,
                                     sampsize = input$featureselrandomforestsampsize,
                                     nodesize = input$featureselrandomforestnodesizeclass,
                                     maxnodes = input$featureselrandomforestmaxnodes,
                                     importance = importance,
                                     localImp = localImp,
                                     nPerm = input$featureselrandomforestnPerm,
                                     proximity = proximity,
                                     oob.prox = oob.prox,
                                     norm.votes = norm.votes,
                                     do.trace = FALSE,
                                     keep.forest = TRUE,
                                     corr.bias = corr.bias,
                                     keep.inbag = FALSE,
                                     na.action = na.omit)
        varimp <- round(data.frame(varImp(fsrandvarimp)),3)
        #colnames(varimp) <- levels(featureselrandomforest_df[,fac])
        varimp
      }
      else{
        ######################################################## Regression ############################
        if(length(predic_cat_name) > 0){
          for(i in predictors_categorical){
            featureselrandomforest_df[, i] <- factor(featureselrandomforest_df[, i])
          }
        }
        ############################# parameters
        if(input$featureselrandomforestreplace == "FALSE"){
          replace <- FALSE
        }else{
          replace <- TRUE
        }
        if(input$featureselrandomforestproximity == "FALSE"){
          proximity <- FALSE
        } else {
          proximity <- TRUE
        }
        if(input$featureselrandomforestoob.prox == "FALSE"){
          oob.prox <- FALSE
        } else {
          oob.prox <- TRUE
        }
        if(input$featureselrandomforestcorr.bias == "FALSE"){
          corr.bias <- FALSE
        } else {
          corr.bias <- TRUE
        }
        if(input$featureselrandomforestimportance == "FALSE"){
          importance <- FALSE
        } else{
          importance <- TRUE
        }
        if(input$featureselrandomforestlocalImp == "FALSE"){
          localImp <- FALSE
        } else{
          localImp <- TRUE
        }  
        if(input$featureselrandomforestnorm.votes == "FALSE"){
          norm.votes <- FALSE
        } else{
          norm.votes <- TRUE
        }
        set.seed(123)
        fsrandvarimp <- randomForest(x  = featureselrandomforest_df[-fac],
                                     y = featureselrandomforest_df[, fac],
                                     ntree = input$featureselrandomforestntree,
                                     mtry = input$featureselrandomforestmtryreg,
                                     replace = replace,
                                     strata = input$featureselrandomforeststrata,
                                     sampsize = input$featureselrandomforestsampsize,
                                     nodesize = input$featureselrandomforestnodesizereg,
                                     maxnodes = input$featureselrandomforestmaxnodes,
                                     importance = importance,
                                     localImp = localImp,
                                     nPerm = input$featureselrandomforestnPerm,
                                     proximity = proximity,
                                     oob.prox = oob.prox,
                                     norm.votes = norm.votes,
                                     do.trace = FALSE,
                                     keep.forest = TRUE,
                                     corr.bias = corr.bias,
                                     keep.inbag = FALSE,
                                     na.action = na.omit)
        varimp <- round(data.frame(varImp(fsrandvarimp)),3)
        varimp
        
      }
    }
  })
  ######################## featureselrandomforest result
  output$featureselrandomforestoutput <- renderDT({
    if(!is.null(featureselrandomforest_analysis()) && input$featureselrandomforestaction) {
      featureselrandomforest_analysis <- isolate(featureselrandomforest_analysis())
      featureselrandomforest_output <- featureselrandomforest_analysis %>%
        datatable(
          options = list(autoWidth = TRUE, dom = "rltip", pageLength=5),
          fillContainer = F,
          rownames = TRUE)
      featureselrandomforest_output
    }
  })
  ################### download random table
  output$download_featuerselection_randomforest <- renderUI({
    if(input$featureselrandomforestaction) {
      downloadButton('randomforestimp', 'Download importance variables')
    }
  })
  output$randomforestimp <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_randomforest_important.csv")
    },
    content = function(file){
      featureselrandomforest_analysis() %>%
        write.csv(file, row.names = T, quote = F)
    }
  )
  #################################### lasso analysis
  lasso_classtype_choices <- reactive({
    if(input$lasso_type == "Classification" && input$lasso_classtype == "Binomial"){
      choices <- c("deviance", "class", "auc" , "mse" , "mae")
    }
    else if(input$lasso_type == "Classification" && input$lasso_classtype == "Multinomial"){
      choices <- c("deviance", "class", "mse", "mae")
    }
    else if(input$lasso_type == "Regression"){
      choices <- c("deviance", "mse", "mae")
      
    }
    
  })
  # lasso_type.measure choices 
  observeEvent(lasso_classtype_choices(),{
    choices <- lasso_classtype_choices()
    updateSelectInput(
      session,
      "lasso_type.measure",
      choices = choices
    )
  })
  ########################### lasso choices
  observeEvent(uploaded_featuersel_data(),{
    if(!is.null(uploaded_featuersel_data())){
      response <- names(uploaded_featuersel_data())
      updateSelectInput(
        session,
        "lasso_response",
        choices = response)
    }
  })
  observeEvent(uploaded_featuersel_data(),{
    if(!is.null(uploaded_featuersel_data())){
      dftype <- sapply(uploaded_featuersel_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictors <- names(uploaded_featuersel_data())[isnumeric]
      updatePickerInput(
        session,
        "lasso_predictors",
        choices = predictors)
    }
  })
  
  
  
  lasso_output_reactive <- reactive({
    if(!is.null(uploaded_featuersel_data()) && input$lassoaction){
      if(is.null(input$lasso_response) | is.null(input$lasso_predictors)){
        return(NULL)
      }
      uploaded_featuersel_data <- isolate(uploaded_featuersel_data())
      uploaded_featuersel_data <- data.frame(uploaded_featuersel_data)
      response <- input$lasso_response
      predictors <- input$lasso_predictors
      res_name <- which(names(uploaded_featuersel_data) %in% response) 
      pred_name <- which(names(uploaded_featuersel_data) %in% predictors) 
      if(length(res_name) > 0 && length(pred_name) > 0 ){
        lasso_df <- data.frame(uploaded_featuersel_data[,c(response, predictors)])
      }

      if(input$lasso_type == "Classification" && input$lasso_classtype == "Binomial"){
      x <- as.matrix(lasso_df[,-1])
      y <- as.double(as.matrix(lasso_df[,1]))
      ########## class weights
      userclass <- input$lassoweights
      if(userclass == ""){
        weights <- NULL
      } else{
        nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
        nu_u_c <- na.omit(nu_u_c)
        classes <- levels(factor(lasso_df[,1]))
        if(length(nu_u_c) == length(classes)){
          weights <- data.frame(weights =rep(NA, length.out = length(lasso_df[,1]))) 
          for(i in 1:length(classes)){
            wn <- which(lasso_df[,1] == as.numeric(classes[i]))
            weights[wn,1] <- as.numeric(nu_u_c[i])
          }
        } else{
          weights <- NULL
        } 
      }
      set.seed(123)
      cv.lasso <- cv.glmnet(x,
                            y,
                            alpha=1,
                            family = "binomial",
                            weights = weights[,1],
                            nfolds = input$lassokfoldcv,
                            parallel=TRUE,
                            standardize=TRUE,
                            type.measure= input$lasso_type.measure)
      cv.lasso
      }
      else if(input$lasso_type == "Classification" && input$lasso_classtype == "Multinomial"){
        x <- as.matrix(lasso_df[,-1])
        y <- as.double(as.matrix(lasso_df[,1]))
        ########## class weights
        userclass <- input$lassoweights
        if(userclass == ""){
          weights <- NULL
        } else{
          nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
          nu_u_c <- na.omit(nu_u_c)
          classes <- levels(factor(lasso_df[,1]))
          if(length(nu_u_c) == length(classes)){
            weights <- data.frame(weights =rep(NA, length.out = length(lasso_df[,1]))) 
            for(i in 1:length(classes)){
              wn <- which(lasso_df[,1] == as.numeric(classes[i]))
              weights[wn,1] <- as.numeric(nu_u_c[i])
            }
          } else{
            weights <- NULL
          } 
        }
        set.seed(123)
        cv.lasso <- cv.glmnet(x,
                              y,
                              alpha=1,
                              family = "multinomial",
                              weights = weights[,1],
                              nfolds = input$lassokfoldcv,
                              parallel=TRUE,
                              standardize=TRUE,
                              type.measure= input$lasso_type.measure)
      cv.lasso
      }
      else if(input$lasso_type == "Regression"){
        x <- as.matrix(lasso_df[,-1])
        y <- as.matrix(lasso_df[,1])
        set.seed(123)
        cv.lasso <- cv.glmnet(x,
                              y,
                              alpha=1,
                              nfolds = input$lassokfoldcv,
                              parallel=TRUE,
                              standardize=TRUE,
                              type.measure= input$lasso_type.measure)
        cv.lasso
      }
    }
  })
  ############################ lasso plot
  output$lassoplot <- renderPlot({
    if(!is.null(lasso_output_reactive()) && input$lassoaction){
      plot(lasso_output_reactive())
    }
  })

  # lsso df reactive
  lasso_data_reactive_output <- reactive({
    if(!is.null(uploaded_featuersel_data()) && input$lassoaction){
      if(is.null(input$lasso_response) | is.null(input$lasso_predictors)){
        return(NULL)
      }
      uploaded_featuersel_data <- isolate(uploaded_featuersel_data())
      uploaded_featuersel_data <- data.frame(uploaded_featuersel_data)
      response <- input$lasso_response
      predictors <- input$lasso_predictors
      res_name <- which(names(uploaded_featuersel_data) %in% response) 
      pred_name <- which(names(uploaded_featuersel_data) %in% predictors) 
      if(length(res_name) > 0 && length(pred_name) > 0 ){
        lasso_df <- data.frame(uploaded_featuersel_data[,c(response, predictors)])
      }
    }
  })
  
  
  
  lasso_output <- reactive({
    if(!is.null(lasso_output_reactive()) && input$lassoaction){
      if(input$lasso_type == "Classification" && input$lasso_classtype == "Binomial"){
        df_coef <- round(as.matrix(coef(lasso_output_reactive(), s = lasso_output_reactive()$lambda.min)), 2)
        df_coef
      }
      else if(input$lasso_type == "Classification" && input$lasso_classtype == "Multinomial"){
        co <- do.call(cbind,coef(lasso_output_reactive(), s=lasso_output_reactive()$lambda.min))
        name <- levels(factor(lasso_data_reactive_output()[,1]))
        colnames(co) <- name
        co_matrix <- round(as.matrix(co),2)
        co_matrix
      }
      else if(input$lasso_type == "Regression"){
        df_coef <- round(as.matrix(coef(lasso_output_reactive(), s = lasso_output_reactive()$lambda.min)), 2)
        df_coef 
      }
    }
  })
  output$lasso_output_table <- renderDT({
    if(!is.null(lasso_output()) && input$lassoaction)
  lasso_output() %>%  
    datatable(
      options = list(autoWidth = TRUE, dom = "rltip", pageLength=6),
      fillContainer = F,
      rownames = TRUE)
  })
  ################### download lasso table
  output$download_lasso_output_table <- renderUI({
    if(input$lassoaction) {
      downloadButton('lassoimp', 'Download importance variables')
    }
  })
  output$lassoimp <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_LASSO_important.csv")
    },
    content = function(file){
      lasso_output() %>%
        write.csv(file, row.names = T, quote = F)
    }
  )
  ################################################################### stepwise regression ########################
  # stepwise chices
  observeEvent(uploaded_featuersel_data(),{
    if(!is.null(uploaded_featuersel_data())){
      dftype <- sapply(uploaded_featuersel_data(), class)
      isnumeric <- grep("numeric", dftype)
      response <- names(uploaded_featuersel_data())[isnumeric]
      updateSelectInput(
        session,
        "stepwisereg_response",
        choices = response)
    }
  })
  observeEvent(uploaded_featuersel_data(),{
    if(!is.null(uploaded_featuersel_data())){
      dftype <- sapply(uploaded_featuersel_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictors <- names(uploaded_featuersel_data())[isnumeric]
      updatePickerInput(
        session,
        "stepwisereg_predictors",
        choices = predictors)
    }
  })
  ######################### stepwise data reactive 
  stepwisereg_reactive <- reactive({
    if(!is.null(uploaded_featuersel_data()) && input$stepwiseaction){
      if(is.null(input$stepwisereg_response) & is.null(input$stepwisereg_predictors)){
        return(NULL)
      }
      uploaded_featuersel_data <- isolate(uploaded_featuersel_data())
      uploaded_featuersel_data <- data.frame(uploaded_featuersel_data)
      response <- input$stepwisereg_response
      predictors <- input$stepwisereg_predictors
      res_name <- which(names(uploaded_featuersel_data) %in% response) 
      pred_name <- which(names(uploaded_featuersel_data) %in% predictors) 
      if(length(res_name) > 0 && length(pred_name) > 0 ){
        stepwisereg_df <- data.frame(uploaded_featuersel_data[,c(response, predictors)])
      }
    }
  })
  observeEvent(stepwisereg_reactive(),{
    if(!is.null(stepwisereg_reactive())){
      len_df <- length(stepwisereg_reactive()) - 1
      updateSliderInput(
        session,
        "stepwiseregnvmax", 
        value = len_df, min = 1, max = len_df, step = 1
      )
    }
  })
  ####################################### stepwise analysis
  stepwisereg_analysis <- reactive({
    if(!is.null(stepwisereg_reactive()) && input$stepwiseaction){
     stepwisereg_df <- isolate(stepwisereg_reactive())
     if(input$stepwisereg_method == "Backward selection"){
       methods <- "leapBackward"
     }
     else if(input$stepwisereg_method == "Forward selection"){
       methods <- "leapForward"
     }
     else if(input$stepwisereg_method == "Stepwise selection"){
       methods <- "leapSeq"
     }
     set.seed(123)
     train.control <- trainControl(method = "cv",
                                   number = input$stepwiseregkfoldcv)
     formula <- as.formula(paste0(names(stepwisereg_df[1]), " ~ ."))
     step.model <- train(formula,
                         data = stepwisereg_df,
                         method = methods, 
                         tuneGrid = data.frame(nvmax = 1:input$stepwiseregnvmax),
                         trControl = train.control)
     step.model
     
    }
  })
  ##########  result
  output$stepwisereg_ouput_result <- renderDT({
    if(!is.null(stepwisereg_analysis()) && input$stepwiseaction){
      swr_df <- data.frame(stepwisereg_analysis()$results)
      colnames(swr_df)[1] <- "N.Predictors"
      swr_df <- round(swr_df[order(swr_df$RMSE), ],2)
      swr_df %>%
      datatable(
        options = list(autoWidth = TRUE, dom = "rltip", pageLength=5),
        fillContainer = F,
        rownames = FALSE) %>% formatStyle (
          "RMSE", target = 'row',
          backgroundColor = styleInterval(min(swr_df$RMSE), c('#2ECC71', '')))
    }
  })
  
  ######## best predictors
  stepwisereg_output_predictors_reactive <- reactive({
    if(!is.null(stepwisereg_analysis())){
      bestpreds <- summary(stepwisereg_analysis()$finalModel)
      bestpreds <- bestpreds$outmat
      bestpreds <- data.frame(bestpreds)
      rownames(bestpreds) <- paste0("Model_", 1:NROW(bestpreds))
      bestpreds
    }
  })
  
  output$stepwisereg_output_predictors <- renderDT({
    if(!is.null(stepwisereg_output_predictors_reactive())){
      stepwise_pred <- isolate(stepwisereg_output_predictors_reactive())
      datatable(stepwise_pred, options = list(
        columnDefs = list(list(className = 'dt-center', targets = 1:NCOL(stepwise_pred))),
        pageLength = 5, scrollX = TRUE, dom = "rltip"
      ))
    }
  })
  ################### download best predictors table
  output$download_stepwise_bestpredictors_table <- renderUI({
    if(input$stepwiseaction) {
      downloadButton('stepwisebestimp', 'Download best predictors')
    }
  })
  output$stepwisebestimp <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_StepwiseRegression_BestPredictors.csv")
    },
    content = function(file){
      stepwisereg_output_predictors_reactive() %>%
        write.csv(file, row.names = T, quote = F)
    }
  )
  stepwisereg_output_predictors_coef_reactive <- reactive({
    if(!is.null(stepwisereg_analysis())){
      swr_df <- data.frame(stepwisereg_analysis()$results)
      swr_df <- swr_df[order(swr_df$RMSE), ]
      coef_df <- round(data.frame(coef(stepwisereg_analysis()$finalModel, swr_df[1,1])),2)
      colnames(coef_df) <- "Coefficients"
      coef_df$Variables <- rownames(coef_df)
      coef_df <- coef_df[2:1]
      rownames(coef_df) <- NULL
      coef_df
    }
  })

  ######## coefficients of best tune
  output$stepwisereg_output_predictors_coef <- renderDT({
    if(!is.null(stepwisereg_output_predictors_coef_reactive())){
      stepwisereg_output_predictors_coef_reactive() %>%
        datatable(
          options = list(autoWidth = TRUE, dom = "rltip", pageLength=5),
          fillContainer = F,
          rownames = FALSE)
    }
  })
  ################### download coef table
  output$download_stepwise_bestpredictors_coef_table <- renderUI({
    if(input$stepwiseaction) {
      downloadButton('stepwisecoefimp', 'Download coefficients')
    }
  })
  output$stepwisecoefimp <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_StepwiseRegression_BestPredictors_Coefficient.csv")
    },
    content = function(file){
      stepwisereg_output_predictors_coef_reactive() %>%
        write.csv(file, row.names = F, quote = F)
    }
  )
  ################################################################# Dimentionality reduction #######################################
  #### error checking
  output$uploaded_dimred_hide_tabpanel <- reactive({
    error_catghing_dimred <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_dimred)
      checknull <- NCOL(uploaded_dimred_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_dimred(input$uploaded_dimred$name, input$datadimredfileformat)
  })
  outputOptions(output, 'uploaded_dimred_hide_tabpanel', suspendWhenHidden=FALSE)
  ################################### uploaded dimred table ######################################################
  uploaded_dimred_data <- eventReactive(input$uploaded_dimred, {
    if(!is.null(input$uploaded_dimred)){
      if(input$datadimredfileformat == "," && input$dimredheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_dimred$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$datadimredfileformat == "," && input$dimredheader == "No") {
        rawdata <- try(read_csv(input$uploaded_dimred$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datadimredfileformat == "\t" && input$dimredheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_dimred$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datadimredfileformat == "\t" && input$dimredheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_dimred$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ########################################### dimred table
  output$dimreddatashow <- renderDT({
    if(!is.null(input$uploaded_dimred) && input$dimredprepross) {
      uploaded_dimred_table <- uploaded_dimred_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE)
      uploaded_dimred_table
    }
  })
  
  ######################### dimred numeric choices
  observeEvent(uploaded_dimred_data(),{
    if(!is.null(uploaded_dimred_data())){
      dftype <- sapply(uploaded_dimred_data(), class)
      isnumeric <- grep("numeric", dftype)
      variables <- names(uploaded_dimred_data())[isnumeric]
      updatePickerInput(
        session,
        "dimred_pca_variables",
        choices = variables)
    }
  })
  ###########################dimred_pca hide panel ###################
  output$hide_dimred_pca <- reactive({
    hidedimredpca <- function(){
      if(input$dimred_method == "PCA"){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    hidedimredpca()
  })
  outputOptions(output, 'hide_dimred_pca', suspendWhenHidden=FALSE)
  ###################### dimred reactive
  dimred_pca_reactive <- reactive({
   if(!is.null(uploaded_dimred_data())){
     uploaded_dimred_data <- isolate(uploaded_dimred_data())
     vars <- input$dimred_pca_variables
     vars_names <- which(names(uploaded_dimred_data) %in% vars)
     if(length(vars_names) > 0){
       dimred_df <- data.frame(uploaded_dimred_data[, vars])
       dimred_df <- scale(dimred_df)
       dimred_df <- data.frame(dimred_df)
     }
   } 
  })
  
  output$pca_scree_plot <- renderPlot({
    if(input$dimred_method == "PCA"){
    if(!is.null(dimred_pca_reactive()) && input$dimred_pca_action){
      pca_df <- isolate(dimred_pca_reactive())
      if(input$dimred_pca_cor == "Correlation matrix"){
        cor <- TRUE
      } else {
        cor <- FALSE
      }
      pca <- princomp(x = pca_df,
                      cor = cor,
                      scores = FALSE,
                      covmat = NULL,
                      na.action = na.omit)
      var_expdf <- data.frame(PC= paste0("PC",1:NCOL(pca_df)),
                              var_exp=(pca$sdev)^2/sum((pca$sdev)^2))
      var_expdf$PC <- factor(var_expdf$PC, levels=unique(var_expdf$PC))
      var_expdf$var_exp <- var_expdf$var_exp*100
      var_expdf$label <- round(var_expdf$var_exp, 2)
      label <- (pca$sdev)^2/sum((pca$sdev)^2)
      qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
      col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
      col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
     p <- var_expdf %>%
        ggplot(aes(x=PC,y=var_exp, fill= PC))+
        geom_col()+ geom_text(aes(label = sprintf("%2.2f%%", label)), vjust = -0.5) +
        scale_color_manual(values=c(col_vector))  +
        xlab('Components') + ylab('Explained variation %') +
        theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                                axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                                axis.text.y = element_text(family = "Times",colour = "black"),
                                plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                                axis.title.y = element_text(family = "Times", size = rel(1.4)),
                                axis.title.x = element_text(family = "Times", size = rel(1.4)),
                                legend.position='none') +
        labs(subtitle = 'Scree plot')
      p
    }
    }
  })
  
  ####################### observe PCA components keeping
  observeEvent(dimred_pca_reactive(),{
    if(!is.null(dimred_pca_reactive())){
      len <- length(dimred_pca_reactive())
      updateSliderInput(
        session,
        "dimred_pcacomponents", 
        value = len,
        min = 2,
        max = len,
        step = 1
      )
    }
  })
  pca_analysis <- reactive({
    if(input$dimred_method == "PCA"){
      if(!is.null(dimred_pca_reactive()) && input$dimred_pca_action){
        pca_df <- isolate(dimred_pca_reactive())
        if(input$dimred_pca_cor == "Correlation matrix"){
          cor <- TRUE
        } else {
          cor <- FALSE
        }
        pca <- princomp(x = pca_df,
                        cor = cor,
                        scores = FALSE,
                        covmat = NULL,
                        na.action = na.omit)
        pca_df <- predict(pca, pca_df)
        pca_df <- data.frame(pca_df)
        colnames(pca_df) <- paste0("PC",1:NCOL(pca_df))
        pca_df <- pca_df[,c(1:input$dimred_pcacomponents)]
      }
    }
  })
  output$pca_components_table <- renderDT({
    if(input$dimred_method == "PCA"){
    if(!is.null(pca_analysis()) && input$dimred_pca_action){
      pca_df <- isolate(pca_analysis())
      pca_df %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE, dom = "lBrtip"),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE)
    }
    }
  })
  ########################### download components table
  output$download_pcacomponents_table <- renderUI({
    if(input$dimred_pca_action) {
      downloadButton('pcacomponents_table', 'Download PCA table')
    }
  })
  
  output$pcacomponents_table <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_PCA_result.csv")
    },
    content = function(file){
      pca_analysis() %>%
        write_csv(file)
    }
  )
  ############################### LDA analysis 
  # lda chocies
  observeEvent(uploaded_dimred_data(),{
    if(!is.null(uploaded_dimred_data())){
      response <- names(uploaded_dimred_data())
      updateSelectInput(
        session,
        "dimred_lda_response",
        choices = response)
    }
  })
  observeEvent(uploaded_dimred_data(),{
    if(!is.null(uploaded_dimred_data())){
      dftype <- sapply(uploaded_dimred_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictors <- names(uploaded_dimred_data())[isnumeric]
      updatePickerInput(
        session,
        "dimred_lda_predictors",
        choices = predictors)
    }
  })
  
  
  dimred_lda_reactive <- reactive({
    if(!is.null(uploaded_dimred_data())){
      uploaded_dimred_data <- isolate(uploaded_dimred_data())
      response <- input$dimred_lda_response
      predictors <- input$dimred_lda_predictors
      res_names <- which(names(uploaded_dimred_data) %in% response)
      pred_names <- which(names(uploaded_dimred_data) %in% predictors)
      if(length(res_names) > 0 & length(pred_names) > 0){
        lda_df <- data.frame(uploaded_dimred_data[, c(response, predictors)])
        if(length(levels(factor(lda_df[,1]))) > 10){
          return(NULL)
        } else{
          return(lda_df)
        }
      }
    } 
  })
  
  ########################## lda pair plot
  output$dimred_lda_pairplot <- renderPlot({
    if(input$dimred_lda_action){
      if(is.null(dimred_lda_reactive())){
        return(NULL)
      }
      lda_df <- isolate(dimred_lda_reactive())
      lda_len <- length(levels(factor(lda_df))) 
      my_pal <- c("#1B9E77", "#6E2C00","#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#9A7D0A")
      
      qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
      col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
      col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
      if(length(lda_df) > 3){
      pairs.panels(lda_df[2:length(lda_df)],
                   hist.col = "#A6761D",
                   stars = TRUE,
                   ci= TRUE,
                   gap = 0,
                   bg = c(my_pal)[lda_df[,1]],
                   pch = 21)
      }
    }
  })
 
  ################## lda fitting model reactive
  dimreg_lda_fitmodel <- reactive({
    if(input$dimred_lda_action){
      if(is.null(dimred_lda_reactive())){
        return(NULL)
      }
      lda_df <- isolate(dimred_lda_reactive())
      lda_df <- data.frame(lda_df)
      lda_df[,1] <- factor(lda_df[,1])
    
      library(caTools)
      set.seed(123)
      split <- sample.split(lda_df[,1], SplitRatio = input$dimreg_lda_ratiosplit)
      training_set <- subset(lda_df, split == TRUE)
      test_set <- subset(lda_df, split == FALSE)
      training_set[-1] <- scale(training_set[-1])
      test_set[-1] <- scale(test_set[-1])
      if(input$dimreg_lda_kfoldcv > 0) {
        folds <- createFolds(lda_df[,1] , k = input$dimreg_lda_kfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          formula <- as.formula(paste0(names(lda_df)[1], " ~ ."))
          ldamodel <- lda(formula = formula,
                          data = training_fold,
                          method = input$dimred_lda_method,
                          na.action = na.omit)
          y_pred <- predict(ldamodel, newdata = test_fold)$class
          result <- confusion_matrix(test_fold[,1], y_pred)
          return(result)
        })
        
        multiclass_con_av(cv)
      }else{
        formula <- as.formula(paste0(names(lda_df)[1], " ~ ."))
        ldamodel <- lda(formula = formula,
                        data = training_set,
                        method = input$dimred_lda_method,
                        na.action = na.omit)
        y_pred <- predict(ldamodel, newdata = test_set)$class
        confusion_matrix(test_set[,1], y_pred)
        
      }
    }
  })
  ###### confution matrix table
  output$dimred_ldaoutput <- renderDT({
    if(!is.null(dimreg_lda_fitmodel())  && input$dimred_lda_action) {
      dimreg_lda_fitmodel <- isolate(dimreg_lda_fitmodel())
      lda_output <- dimreg_lda_fitmodel %>%
        datatable(
          options = list(scrollX = TRUE, dom = "t"),
          rownames = TRUE)
      lda_output
    }
  })
  ################## LDA histogram choices 
  dimreg_lda_plots_choice <- reactive({
    if(!is.null(dimred_lda_reactive()) && input$dimred_lda_action){
      lda_df <- isolate(dimred_lda_reactive())
      lda_df[,1] <- factor(lda_df[,1])
      library(caTools)
      set.seed(123)
      split <- sample.split(lda_df[,1], SplitRatio = input$dimreg_lda_ratiosplit)
      training_set <- subset(lda_df, split == TRUE)
      test_set <- subset(lda_df, split == FALSE)
      training_set[-1] <- scale(training_set[-1])
      test_set[-1] <- scale(test_set[-1])
      if(input$dimreg_lda_kfoldcv > 0) {
        folds <- createFolds(lda_df[,1] , k = input$dimreg_lda_kfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          formula <- as.formula(paste0(names(lda_df)[1], " ~ ."))
          ldamodel <- lda(formula = formula,
                          data = training_fold,
                          method = input$dimred_lda_method,
                          na.action = na.omit)
          y_pred <- predict(ldamodel, newdata = test_fold)$x
          return(y_pred)
        })
        cv[[1]]
      }else{
        formula <- as.formula(paste0(names(lda_df)[1], " ~ ."))
        ldamodel <- lda(formula = formula,
                        data = training_set,
                        method = input$dimred_lda_method,
                        na.action = na.omit)
        y_pred <- predict(ldamodel, newdata = test_set)$x
        y_pred
      }
    }
  })
  ###################### histogram choice
  observeEvent(dimreg_lda_plots_choice(),{
    if(!is.null(dimreg_lda_plots_choice())){
      names <- colnames(dimreg_lda_plots_choice())
      updateSelectInput(
        session,
        "dimred_lda_histplot_choices",
        choices = names
      )
    }
  })
  ########################### lda hitogram
  output$dimreg_lda_histplot <- renderPlot({
    if(!is.null(dimred_lda_reactive()) && input$dimred_lda_hitplotview){
      lda_df <- isolate(dimred_lda_reactive())
      lda_df[,1] <- factor(lda_df[,1])
      library(caTools)
      set.seed(123)
      split <- sample.split(lda_df[,1], SplitRatio = input$dimreg_lda_ratiosplit)
      training_set <- subset(lda_df, split == TRUE)
      test_set <- subset(lda_df, split == FALSE)
      training_set[-1] <- scale(training_set[-1])
      test_set[-1] <- scale(test_set[-1])
      if(input$dimreg_lda_kfoldcv > 0) {
        folds <- createFolds(lda_df[,1] , k = input$dimreg_lda_kfoldcv)
        cv <- lapply(folds, function(x){
       
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          
          if(input$dimred_lda_histplot_dataset == "Trianing set"){
            dataset <- training_fold[,1]
          } else{
            dataset <- test_fold[,1]
          }
          formula <- as.formula(paste0(names(lda_df)[1], " ~ ."))
          ldamodel <- lda(formula = formula,
                          data = training_fold,
                          method = input$dimred_lda_method,
                          na.action = na.omit)
          y_pred <- predict(ldamodel, newdata = test_fold)
          hit <- ldahist(data = y_pred$x[,input$dimred_lda_histplot_choices], g = dataset, col = "#E6AB02")
          return(hit)
        })
        cv
      }else{
        if(input$dimred_lda_histplot_dataset == "Trianing set"){
          dataset <- training_set[,1]
        } else{
          dataset <- test_set[,1]
        }
        formula <- as.formula(paste0(names(lda_df)[1], " ~ ."))
        ldamodel <- lda(formula = formula,
                        data = training_set,
                        method = input$dimred_lda_method,
                        na.action = na.omit)
        y_pred <- predict(ldamodel, newdata = test_set)
        ldahist(data = y_pred$x[,input$dimred_lda_histplot_choices], g = dataset, col = "#E6AB02")

      }
    }
  })
  ################### ldas chices
  observeEvent(dimreg_lda_plots_choice(),{
    if(!is.null(dimreg_lda_plots_choice())){
      namex <- colnames(dimreg_lda_plots_choice())
      if(length(namex) > 1){
      updateSelectInput(
        session,
        "dimred_lda_ldas_choicex",
        choices = namex
      )
      }
      }
  })
  observeEvent(dimreg_lda_plots_choice(),{
    if(!is.null(dimreg_lda_plots_choice())){
      namey <- colnames(dimreg_lda_plots_choice())
      len_namey <- length(namey)
      if(len_namey > 1){
      namey <- namey[len_namey:1]
      updateSelectInput(
        session,
        "dimred_lda_ldas_choicey",
        choices = namey
      )
      }
    }
  })
  
  
  ############################### plot LDs objects
  output$dimreg_lda_ldas_plot <- renderPlot({
    if(!is.null(dimred_lda_reactive()) && input$dimred_lda_ldas_plotview){
      lda_df <- isolate(dimred_lda_reactive())
      lda_df[,1] <- factor(lda_df[,1])
      library(caTools)
      set.seed(123)
      split <- sample.split(lda_df[,1], SplitRatio = input$dimreg_lda_ratiosplit)
      training_set <- subset(lda_df, split == TRUE)
      test_set <- subset(lda_df, split == FALSE)
      training_set[-1] <- scale(training_set[-1])
      test_set[-1] <- scale(test_set[-1])
        formula <- as.formula(paste0(names(lda_df)[1], " ~ ."))
        ldamodel <- lda(formula = formula,
                        data = training_set,
                        method = input$dimred_lda_method,
                        na.action = na.omit)
        y_pred <- predict(ldamodel, newdata = lda_df)
        len_fac <- length(levels(factor(lda_df)))
        if(len_fac > 2){
          plot_df <- data.frame(Groups = lda_df[,1], lda = y_pred$x)
          names(plot_df)[-1] <- paste0("LD", 1:(length(plot_df) - 1))
          prop <- ldamodel$svd^2/sum(ldamodel$svd^2)
          names(prop) <- paste0("LD", 1:length(ldamodel$svd))
          my_pal <- c("#1B9E77", "#6E2C00","#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#9A7D0A")
          p <- ggplot(aes(x = plot_df[,input$dimred_lda_ldas_choicex],
                          y = plot_df[,input$dimred_lda_ldas_choicey], 
                          colour = Groups, fill = Groups), data = plot_df) + 
            geom_point(size = 4, shape = 21) +
            xlab(paste0(input$dimred_lda_ldas_choicex,sprintf(" (%2.2f%%)", prop[input$dimred_lda_ldas_choicex]*100))) + 
            ylab(paste0(input$dimred_lda_ldas_choicey,sprintf(" (%2.2f%%)", prop[input$dimred_lda_ldas_choicey]*100))) +
            scale_color_manual(values=c(my_pal)) + stat_ellipse() +
            scale_fill_manual(values=c(paste(my_pal, "66", sep = ""))) +
            theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                    axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                                    axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                                    axis.text.y = element_text(family = "Times",colour = "black"),
                                    plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                                    axis.title.y = element_text(family = "Times", size = rel(1.4)),
                                    axis.title.x = element_text(family = "Times", size = rel(1.4))) +
            labs(subtitle = '         Biplot')
          p
        }
    }
  })
  ############################### LDA prediction on the dataset ##########################
  dimreg_lda_lds_reactive <- reactive({
    if(!is.null(dimred_lda_reactive()) && input$dimred_lda_predictiondata){
      lda_df <- isolate(dimred_lda_reactive())
      lda_df[,1] <- factor(lda_df[,1])
      library(caTools)
      set.seed(123)
      split <- sample.split(lda_df[,1], SplitRatio = input$dimreg_lda_ratiosplit)
      training_set <- subset(lda_df, split == TRUE)
      test_set <- subset(lda_df, split == FALSE)
      training_set[-1] <- scale(training_set[-1])
      test_set[-1] <- scale(test_set[-1])
      if(input$dimreg_lda_kfoldcv > 0) {
        folds <- createFolds(lda_df[,1] , k = input$dimreg_lda_kfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          formula <- as.formula(paste0(names(lda_df)[1], " ~ ."))
          ldamodel <- lda(formula = formula,
                          data = training_fold,
                          method = input$dimred_lda_method,
                          na.action = na.omit)
          y_pred <- predict(ldamodel, newdata = lda_df)$x
          return(y_pred)
        })
        ld_cv <- data.frame(do.call(cbind, cv,))
        col <- list()
        for(i in 1:NCOL(cv[[1]])){
          patern <- paste0("LD", i)
          num <- grep(patern, names(ld_cv))
          col[[i]] <- rowMeans(ld_cv[num])
        }
        LDs <- data.frame(do.call(cbind, col))
        colnames(LDs) <- paste0("LD", 1:NCOL(cv[[1]]))
        LDs
      }else{
        formula <- as.formula(paste0(names(lda_df)[1], " ~ ."))
        ldamodel <- lda(formula = formula,
                        data = training_set,
                        method = input$dimred_lda_method,
                        na.action = na.omit)
        y_pred <- predict(ldamodel, newdata = lda_df)$x
        y_pred
      }
    }
  })
  ########### PREDICTION TABLE
  output$dimred_lda_prediction <- renderDT({
    if(!is.null(dimreg_lda_lds_reactive()) && input$dimred_lda_predictiondata) {
      lda_comp <- isolate(dimreg_lda_lds_reactive())
      lda_comp %>%
        datatable(
          fillContainer = F,
          options = list(autoWidth = FALSE,
                         dom = "lBrtip",
                         columnDefs = list(list(width = '200px', targets = "_all")),
                         pageLength = 10),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE)
    }
  })
  ########################### download LDA table
  output$download_lda_table <- renderUI({
    if(input$dimred_lda_predictiondata) {
      downloadButton('lda_table', 'Download LDA table')
    }
  })
  
  output$lda_table <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_LDA_result.csv")
    },
    content = function(file){
      dimreg_lda_lds_reactive() %>%
        write_csv(file)
    }
  )
  
  
  ########################################################## ML-Regression-Simple  ######################
  #### erorr checking
  output$uploaded_slreg_hide_tabpanel <- reactive({
    error_catghing_slreg <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_slreg)
      checknull <- NCOL(uploaded_slreg_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_slreg(input$uploaded_slreg$name, input$dataslregfileformat)
  })
  outputOptions(output, 'uploaded_slreg_hide_tabpanel', suspendWhenHidden=FALSE)
  ################################### uploaded slreg table ######################################################
  uploaded_slreg_data <- eventReactive(input$uploaded_slreg, {
    if(!is.null(input$uploaded_slreg)){
      if(input$dataslregfileformat == "," && input$slregheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_slreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$dataslregfileformat == "," && input$slregheader == "No") {
        rawdata <- try(read_csv(input$uploaded_slreg$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$dataslregfileformat == "\t" && input$slregheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_slreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$dataslregfileformat == "\t" && input$slregheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_slreg$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ########################################### slreg table
  output$slregdatashow <- renderDT({
    req(input$uploaded_slreg)
    if(input$slregprepross) {
      uploaded_slreg_table <- uploaded_slreg_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE)
      uploaded_slreg_table
    }
  })
  ########################### response and predictor choices #####################
  # response 
  observeEvent(uploaded_slreg_data(), {
    if(!is.null(uploaded_slreg_data())){
      dftype <- sapply(uploaded_slreg_data(), class)
      isnumeric <- grep("numeric", dftype)
      response_var <- names(uploaded_slreg_data())[isnumeric]
      updateSelectInput(
        session,
        "slregresponse",
        choices = response_var)
    }
  })
  # numeric
  observeEvent(uploaded_slreg_data(), {
    if(!is.null(uploaded_slreg_data())){
      dftype <- sapply(uploaded_slreg_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_var <- names(uploaded_slreg_data())[isnumeric]
      updateSelectInput(
        session,
        "slregpredictor",
        choices = predictor_var)
    }
  })
  # Simple regression result
  slreg_analysis <- reactive({
    if(!is.null(uploaded_slreg_data()) && input$slregaction){
      if(is.null(input$slregresponse) && is.null(input$slregpredictor)){
        return(NULL)
      }
      uploaded_slreg_data <- isolate(uploaded_slreg_data())
      response <- input$slregresponse
      predictor <- input$slregpredictor
      res_name <- which(names(uploaded_slreg_data) %in% response)
      pred_name <- which(names(uploaded_slreg_data) %in% predictor)
      if(length(res_name) > 0 && length(pred_name) > 0){
        slreg_df <- data.frame(uploaded_slreg_data[, c(response, predictor)])
      } else{
        return(NULL)
      }
      
      set.seed(123)
      split <- sample.split(slreg_df[,1], SplitRatio = input$slregratiosplit)
      training_set <- subset(slreg_df, split == TRUE)
      test_set <- subset(slreg_df, split == FALSE)
      if(input$slregkfoldcv > 0) {
        folds <- createFolds(slreg_df[,1] , k = input$slregkfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          formula <- as.formula(paste0(names(slreg_df)[1], " ~ ."))
          slregmodel <- lm(formula = formula,
                          data = training_fold,
                          na.action = na.exclude)
          y_pred <- predict(slregmodel, newdata = test_fold[-1])
          mse <- MLmetrics::MSE(y_pred, test_fold[,1])
          mae <- MLmetrics::MAE(y_pred, test_fold[,1])
          rmse <- MLmetrics::RMSE(y_pred, test_fold[,1])
          r2 <- MLmetrics::R2_Score(y_pred, test_fold[,1])
          accuracy <- data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2)
          return(accuracy)
        })
        Accuracy <- round(colMeans(do.call(rbind, cv)), 3)
        Accuracy <- data.frame(t(Accuracy))
        Accuracy
      } else{
        formula <- as.formula(paste0(names(slreg_df)[1], " ~ ."))
        slregmodel <- lm(formula = formula,
                         data = training_set,
                         na.action = na.exclude)
        y_pred <- predict(slregmodel, newdata = test_set[-1])
        mse <- MLmetrics::MSE(y_pred, test_set[,1])
        mae <- MLmetrics::MAE(y_pred, test_set[,1])
        rmse <- MLmetrics::RMSE(y_pred, test_set[,1])
        r2 <- MLmetrics::R2_Score(y_pred, test_set[,1])
        accuracy <- round(data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2),3)
        accuracy
      }
    }
  })
  ##################### slreg result table 
  output$slregoutput <- renderDT({
    if(!is.null(slreg_analysis()) && input$slregaction) {
      slreg_analysis <- isolate(slreg_analysis())
      slreg_output <- slreg_analysis %>%
        datatable(
          options = list(scrollX = TRUE, dom = "t"),
          rownames = FALSE)
      slreg_output
    }
  })
  ######################################### slreg plot ###################################
  slreg_reactive <- reactive({
    if(!is.null(uploaded_slreg_data()) && input$slregaction){
      if(is.null(input$slregresponse) && is.null(input$slregpredictor)){
        return(NULL)
      }
      uploaded_slreg_data <- isolate(uploaded_slreg_data())
      response <- input$slregresponse
      predictor <- input$slregpredictor
      res_name <- which(names(uploaded_slreg_data) %in% response)
      pred_name <- which(names(uploaded_slreg_data) %in% predictor)
      if(length(res_name) > 0 && length(pred_name) > 0){
        slreg_df <- data.frame(uploaded_slreg_data[, c(response, predictor)])
      }else {
        return(NULL)
      }
    }
    })
  
  output$slregplottraining <- renderPlot({
    if(!is.null(uploaded_slreg_data()) && input$slregplotview){
      if(is.null(input$slregresponse) && is.null(input$slregpredictor)){
        return(NULL)
      }
      uploaded_slreg_data <- isolate(uploaded_slreg_data())
      response <- input$slregresponse
      predictor <- input$slregpredictor
      res_name <- which(names(uploaded_slreg_data) %in% response)
      pred_name <- which(names(uploaded_slreg_data) %in% predictor)
      if(length(res_name) > 0 && length(pred_name) > 0){
        slreg_df <- data.frame(uploaded_slreg_data[, c(response, predictor)])
      } else{
        return(NULL)
      }
      
      set.seed(123)
      split <- sample.split(slreg_df[,1], SplitRatio = input$slregratiosplit)
      training_set <- subset(slreg_df, split == TRUE)
      test_set <- subset(slreg_df, split == FALSE)
        formula <- as.formula(paste0(names(slreg_df)[1], " ~ ."))
        slregmodel <- lm(formula = formula,
                         data = training_set,
                         na.action = na.exclude)
        y <- predict(slregmodel, newdata = training_set)
        p <- ggplot() + geom_point(aes(x = training_set[,2], y = training_set[,1]),colour = '#145A32') +
          geom_line(aes(x = training_set[,2], y= y), colour = '#B7950B', size= 1) +
          xlab(names(training_set)[2]) + ylab(names(training_set)[1]) + 
          theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                  axis.text = element_text(colour = "black"),
                                  axis.text.y = element_text(colour = "black")) +
          theme(axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                axis.text.y = element_text(family = "Times",colour = "black"),
                plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                axis.title.y = element_text(family = "Times", size = rel(1.4)),
                axis.title.x = element_text(family = "Times", size = rel(1.4)))+
          labs(subtitle = "Simple linear regression (Training set)")
         p
      }
  })
  ################################## slreg test plot
  output$slregplottest <- renderPlot({
    if(!is.null(uploaded_slreg_data()) && input$slregplotview){
      if(is.null(input$slregresponse) && is.null(input$slregpredictor)){
        return(NULL)
      }
      uploaded_slreg_data <- isolate(uploaded_slreg_data())
      response <- input$slregresponse
      predictor <- input$slregpredictor
      res_name <- which(names(uploaded_slreg_data) %in% response)
      pred_name <- which(names(uploaded_slreg_data) %in% predictor)
      if(length(res_name) > 0 && length(pred_name) > 0){
        slreg_df <- data.frame(uploaded_slreg_data[, c(response, predictor)])
      } else{
        return(NULL)
      }
      set.seed(123)
      split <- sample.split(slreg_df[,1], SplitRatio = input$slregratiosplit)
      training_set <- subset(slreg_df, split == TRUE)
      test_set <- subset(slreg_df, split == FALSE)
        formula <- as.formula(paste0(names(slreg_df)[1], " ~ ."))
        slregmodel <- lm(formula = formula,
                         data = training_set,
                         na.action = na.exclude)
        y <- predict(slregmodel, newdata = test_set)
        p <- ggplot() + geom_point(aes(x = test_set[,2], y = test_set[,1]),colour = '#145A32') +
          geom_line(aes(x = test_set[,2], y= y), colour = '#B7950B', size= 1) +
          xlab(names(test_set)[2]) + ylab(names(test_set)[1]) + 
          theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                  axis.text = element_text(colour = "black"),
                                  axis.text.y = element_text(colour = "black")) +
          theme(axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                axis.text.y = element_text(family = "Times",colour = "black"),
                plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                axis.title.y = element_text(family = "Times", size = rel(1.4)),
                axis.title.x = element_text(family = "Times", size = rel(1.4)))+
          labs(subtitle = "Simple linear regression (Test set)")
        p
      }
  })
  ############################################## slreg prediction on new data ##############################
  #### error checking
  output$uploaded_slregdata_prediction_hide_tabpanel <- reactive({
    error_catghing_slreg_prediction <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_slreg_prediction)
      checknull <- NCOL(uploaded_slreg_data_prediction()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_slreg_prediction(input$uploaded_slreg_prediction$name, input$dataslregfileformat_prediction)
  })
  outputOptions(output, 'uploaded_slregdata_prediction_hide_tabpanel', suspendWhenHidden=FALSE)
  ############### uploaded prediction table
  uploaded_slreg_data_prediction <- eventReactive(input$uploaded_slreg_prediction, {
    if(!is.null(input$uploaded_slreg_prediction)){
      if(input$dataslregfileformat == "," && input$slregheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_slreg_prediction$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if(input$dataslregfileformat == "," && input$slregheader == "No") {
        rawdata <- try(read_csv(input$uploaded_slreg_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$dataslregfileformat == "\t" && input$slregheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_slreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$dataslregfileformat == "\t" && input$slregheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_slreg_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ############### new table for prediction choices
  # response and predictor choices
  observeEvent(uploaded_slreg_data_prediction(), {
    if(!is.null(uploaded_slreg_data_prediction())){
      dftype <- sapply(uploaded_slreg_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      response_vars <- names(uploaded_slreg_data_prediction())[isnumeric]
      updateSelectInput(
        session,
        "slreg_prediction_response",
        choices = response_vars)
    }
  })
  # numeric
  observeEvent(uploaded_slreg_data_prediction(), {
    if(!is.null(uploaded_slreg_data_prediction())){
      dftype <- sapply(uploaded_slreg_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_var <- names(uploaded_slreg_data_prediction())[isnumeric]
      updatePickerInput(
        session,
        "slreg_prediction_predictor",
        choices = predictor_var)
    }
  })
  ##################################################### slreg prediction result ######################
  slreg_analysis_prediction <- reactive({
    if(!is.null(uploaded_slreg_data()) && input$slregprepross_prediction_result){
      if(is.null(input$slregresponse) && is.null(input$slregpredictor)){
        return(NULL)
      }
      ############ for slreg prediction
      uploaded_slreg_data_prediction <- isolate(uploaded_slreg_data_prediction())
      slreg_prediction_predictor <- input$slreg_prediction_predictor
      num_predict <- which(names(uploaded_slreg_data_prediction) %in% slreg_prediction_predictor)
      if(length(num_predict) > 0 ){
        slreg_df_prediction <- data.frame(uploaded_slreg_data_prediction[, slreg_prediction_predictor])
        slreg_df_prediction
      } else{
        return(NULL)
      }
      uploaded_slreg_data <- isolate(uploaded_slreg_data())
      response <- input$slregresponse
      predictor <- input$slregpredictor
      res_name <- which(names(uploaded_slreg_data) %in% response)
      pred_name <- which(names(uploaded_slreg_data) %in% predictor)
      if(length(res_name) > 0 && length(pred_name) > 0){
        slreg_df <- data.frame(uploaded_slreg_data[, c(response, predictor)])
      }
      ############## check both file to be almost the same ##########################
      similar_name <- which(names(slreg_df) %in% names(slreg_df_prediction))
      if(length(similar_name) < 1){
        return(NULL)
      }
      #####################################################################################
      set.seed(123)
      split <- sample.split(slreg_df[,1], SplitRatio = input$slregratiosplit)
      training_set <- subset(slreg_df, split == TRUE)
      test_set <- subset(slreg_df, split == FALSE)
      if(input$slregkfoldcv > 0) {
        folds <- createFolds(slreg_df[,1] , k = input$slregkfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          formula <- as.formula(paste0(names(slreg_df)[1], " ~ ."))
          slregmodel <- lm(formula = formula,
                           data = training_fold,
                           na.action = na.exclude)
          y_pred <- predict(slregmodel, newdata = slreg_df_prediction)
          return(y_pred)
        })
        Prediction <- round(colMeans(do.call(rbind, cv)), 3)
        Prediction <- data.frame(Prediction)
        colnames(Prediction) <- response
        Prediction
      } else{
        formula <- as.formula(paste0(names(slreg_df)[1], " ~ ."))
        slregmodel <- lm(formula = formula,
                         data = training_set,
                         na.action = na.exclude)
        y_pred <- predict(slregmodel, newdata = slreg_df_prediction)
        y_pred <- round(data.frame(y_pred),3)
        colnames(y_pred) <- response
        y_pred
      }
    }
  })
  ############ table of actual value for new data
  output$slreg_actual_response <- renderDT({
    if(!is.null(input$slreg_prediction_response) && input$slregprepross_prediction_result) {
      uploaded_slreg_data_prediction <- isolate(uploaded_slreg_data_prediction())
      response_prediction <- input$slreg_prediction_response
      slreg_df_prediction <- data.frame(uploaded_slreg_data_prediction[, response_prediction])
      slreg_df_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 7),
          rownames = T)
    }
  })
  ############ table of predicted value for new data
  output$slreg_predicted_response <- renderDT({
    req(slreg_analysis_prediction())
    if(input$slregprepross_prediction_result) {
      slreg_analysis_prediction <- isolate(slreg_analysis_prediction())
      slreg_analysis_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 7),
          rownames = T)
    }
  })
  
  ### Download slreg prediction table
  output$download_prediction_slreg_table <- renderUI({
    if(input$slregprepross_prediction_result) {
      downloadButton('slregprediction_table', 'Download prediction table')
    }
  })
  
  output$slregprediction_table <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_prediction_table.csv")
    },
    content = function(file){
      slreg_analysis_prediction() %>%
        write_csv(file)
    }
  )
  ########################################################## ML-Regression-Multiple ######################
  #### erorr checking
  output$uploaded_mlreg_hide_tabpanel <- reactive({
    error_catghing_mlreg <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_mlreg)
      checknull <- NCOL(uploaded_mlreg_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_mlreg(input$uploaded_mlreg$name, input$datamlregfileformat)
  })
  outputOptions(output, 'uploaded_mlreg_hide_tabpanel', suspendWhenHidden=FALSE)
  ################################### uploaded mlreg table ######################################################
  uploaded_mlreg_data <- eventReactive(input$uploaded_mlreg,{
    if(!is.null(input$uploaded_mlreg)){
      if(input$datamlregfileformat == "," && input$mlregheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_mlreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$datamlregfileformat == "," && input$mlregheader == "No") {
        rawdata <- try(read_csv(input$uploaded_mlreg$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datamlregfileformat == "\t" && input$mlregheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_mlreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datamlregfileformat == "\t" && input$mlregheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_mlreg$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ########################################### mlreg table
  output$mlregdatashow <- renderDT({
    req(input$uploaded_mlreg)
    if(input$mlregprepross) {
      uploaded_mlreg_table <- uploaded_mlreg_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE)
      uploaded_mlreg_table
    }
  })
  ########################### response and predictor choices #####################
  # response 
  observeEvent(uploaded_mlreg_data(), {
    if(!is.null(uploaded_mlreg_data())){
      dftype <- sapply(uploaded_mlreg_data(), class)
      isnumeric <- grep("numeric", dftype)
      response_var <- names(uploaded_mlreg_data())[isnumeric]
      updateSelectInput(
        session,
        "mlregresponse",
        choices = response_var)
    }
  })
  # numeric
  observeEvent(uploaded_mlreg_data(), {
    if(!is.null(uploaded_mlreg_data())){
      dftype <- sapply(uploaded_mlreg_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_var <- names(uploaded_mlreg_data())[isnumeric]
      updatePickerInput(
        session,
        "mlregpredictors_numeric",
        choices = predictor_var)
    }
  })
  # categorical
  observeEvent(uploaded_mlreg_data(), {
    if(!is.null(uploaded_mlreg_data())){
      dftype <- sapply(uploaded_mlreg_data(), class)
      ischar <- grep("character|logical", dftype)
      predictor_var <- names(uploaded_mlreg_data())[ischar]
      updatePickerInput(
        session,
        "mlregpredictors_categorical",
        choices = predictor_var)
    }
  })
  # multiple regression result
  mlreg_analysis <- reactive({
    if(!is.null(uploaded_mlreg_data()) && input$mlregaction){
      if(is.null(input$mlregresponse) | is.null(input$mlregpredictors_numeric) && is.null(input$mlregpredictors_categorical)){
        return(NULL)
      }
      uploaded_mlreg_data <- isolate(uploaded_mlreg_data())
      response <- input$mlregresponse
      predictors_numeric <- input$mlregpredictors_numeric
      predictors_categori <- input$mlregpredictors_categorical
      res_name <- which(names(uploaded_mlreg_data) %in% response)
      pred_name_num <- which(names(uploaded_mlreg_data) %in% predictors_numeric)
      pred_name_cat <- which(names(uploaded_mlreg_data) %in% predictors_categori)
      if(length(res_name) > 0 | length(pred_name_num) > 0 | length(pred_name_cat) > 0){
        mlreg_df <- data.frame(uploaded_mlreg_data[, c(response, predictors_numeric, predictors_categori)])
      } else{
        return(NULL)
      }
      
      if(length(pred_name_cat) > 0){
        for(i in predictors_categori){
          mlreg_df[, i] <- factor(mlreg_df[, i])
        }
      }
      set.seed(123)
      split <- sample.split(mlreg_df[,1], SplitRatio = input$mlregratiosplit)
      training_set <- subset(mlreg_df, split == TRUE)
      test_set <- subset(mlreg_df, split == FALSE)
      if(input$mlregkfoldcv > 0) {
        folds <- createFolds(mlreg_df[,1] , k = input$mlregkfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          formula <- as.formula(paste0(names(mlreg_df)[1], " ~ ."))
          mlregmodel <- lm(formula = formula,
                           data = training_fold,
                           na.action = na.exclude)
          y_pred <- predict(mlregmodel, newdata = test_fold[-1])
          mse <- MLmetrics::MSE(y_pred, test_fold[,1])
          mae <- MLmetrics::MAE(y_pred, test_fold[,1])
          rmse <- MLmetrics::RMSE(y_pred, test_fold[,1])
          r2 <- MLmetrics::R2_Score(y_pred, test_fold[,1])
          accuracy <- data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2)
          return(accuracy)
        })
        Accuracy <- round(colMeans(do.call(rbind, cv)), 3)
        Accuracy <- data.frame(t(Accuracy))
        Accuracy
      } else{
        formula <- as.formula(paste0(names(mlreg_df)[1], " ~ ."))
        mlregmodel <- lm(formula = formula,
                         data = training_set,
                         na.action = na.exclude)
        y_pred <- predict(mlregmodel, newdata = test_set[-1])
        mse <- MLmetrics::MSE(y_pred, test_set[,1])
        mae <- MLmetrics::MAE(y_pred, test_set[,1])
        rmse <- MLmetrics::RMSE(y_pred, test_set[,1])
        r2 <- MLmetrics::R2_Score(y_pred, test_set[,1])
        accuracy <- round(data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2),3)
        accuracy
      }
    }
  })
  ##################### mlreg result table 
  output$mlregoutput <- renderDT({
    if(!is.null(mlreg_analysis()) && input$mlregaction) {
      mlreg_analysis <- isolate(mlreg_analysis())
      mlreg_output <- mlreg_analysis %>%
        datatable(
          options = list(scrollX = TRUE, dom = "t"),
          rownames = FALSE)
      mlreg_output
    }
  })
  ######################################### mlreg plot ###################################
  output$mlregplottraining <- renderPlot({
    if(!is.null(uploaded_mlreg_data()) && input$mlregplotview){
      if(is.null(input$mlregresponse) | is.null(input$mlregpredictors_numeric) && is.null(input$mlregpredictors_categorical)){
        return(NULL)
      }
      uploaded_mlreg_data <- isolate(uploaded_mlreg_data())
      response <- input$mlregresponse
      predictors_numeric <- input$mlregpredictors_numeric
      predictors_categori <- input$mlregpredictors_categorical
      res_name <- which(names(uploaded_mlreg_data) %in% response)
      pred_name_num <- which(names(uploaded_mlreg_data) %in% predictors_numeric)
      pred_name_cat <- which(names(uploaded_mlreg_data) %in% predictors_categori)
      if(length(res_name) > 0 | length(pred_name_num) > 0 | length(pred_name_cat) > 0){
        mlreg_df <- data.frame(uploaded_mlreg_data[, c(response, predictors_numeric, predictors_categori)])
      } else{
        return(NULL)
      }
      
      if(length(pred_name_cat) > 0){
        for(i in predictors_categori){
          mlreg_df[, i] <- factor(mlreg_df[, i])
        }
      }
      set.seed(123)
      split <- sample.split(mlreg_df[,1], SplitRatio = input$mlregratiosplit)
      training_set <- subset(mlreg_df, split == TRUE)
      test_set <- subset(mlreg_df, split == FALSE)
        formula <- as.formula(paste0(names(mlreg_df)[1], " ~ ."))
        mlregmodel <- lm(formula = formula,
                         data = training_set,
                         na.action = na.exclude)
        qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
        col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
        col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
        p <- avPlots(mlregmodel, col = col_vector,
                     col.lines = "#9A7D0A", 
                     pch = 16,
                     main = "Multiple regression (Training set)",
                     cex = 1.3)
        p
      }
  })
  ############################################## mlreg prediction on new data ##############################
  #### error checking
  output$uploaded_mlregdata_prediction_hide_tabpanel <- reactive({
    error_catghing_mlreg_prediction <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_mlreg_prediction)
      checknull <- NCOL(uploaded_mlreg_data_prediction()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_mlreg_prediction(input$uploaded_mlreg_prediction$name, input$datamlregfileformat_prediction)
  })
  outputOptions(output, 'uploaded_mlregdata_prediction_hide_tabpanel', suspendWhenHidden=FALSE)
  ############### uploaded prediction table
 uploaded_mlreg_data_prediction <- eventReactive(input$uploaded_mlreg_prediction, {
    if(!is.null(input$uploaded_mlreg_prediction)){
      if(input$datamlregfileformat == "," && input$mlregheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_mlreg_prediction$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if(input$datamlregfileformat == "," && input$mlregheader == "No") {
        rawdata <- try(read_csv(input$uploaded_mlreg_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datamlregfileformat == "\t" && input$mlregheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_mlreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datamlregfileformat == "\t" && input$mlregheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_mlreg_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ############### new table for prediction choices
  # response and predictor choices
  observeEvent(uploaded_mlreg_data_prediction(), {
    if(!is.null(uploaded_mlreg_data_prediction())){
      dftype <- sapply(uploaded_mlreg_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      response_vars <- names(uploaded_mlreg_data_prediction())[isnumeric]
      updateSelectInput(
        session,
        "mlreg_prediction_response",
        choices = response_vars)
    }
  })
  # numeric
  observeEvent(uploaded_mlreg_data_prediction(), {
    if(!is.null(uploaded_mlreg_data_prediction())){
      dftype <- sapply(uploaded_mlreg_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_var <- names(uploaded_mlreg_data_prediction())[isnumeric]
      updatePickerInput(
        session,
        "mlreg_prediction_predictor_numeric",
        choices = predictor_var)
    }
  })
  ## categoric
  observeEvent(uploaded_mlreg_data_prediction(), {
    if(!is.null(uploaded_mlreg_data_prediction())){
      dftype <- sapply(uploaded_mlreg_data_prediction(), class)
      ischar <- grep("character|logical", dftype)
      predictor_var <- names(uploaded_mlreg_data_prediction())[ischar]
      updatePickerInput(
        session,
        "mlreg_prediction_predictor_categorical",
        choices = predictor_var)
    }
  })
  mlreg_analysis_prediction <- reactive({
    if(!is.null(uploaded_mlreg_data()) && input$mlregaction){
      if(is.null(input$mlregresponse) | is.null(input$mlregpredictors_numeric) && is.null(input$mlregpredictors_categorical)){
        return(NULL)
      }
      ############ for mlreg prediction
      uploaded_mlreg_data_prediction <- isolate(uploaded_mlreg_data_prediction())
      mlreg_prediction_predictor_numeric <- input$mlreg_prediction_predictor_numeric
      mlreg_prediction_predictor_categori <- input$mlreg_prediction_predictor_categorical
      num_predict_num <- which(names(uploaded_mlreg_data_prediction) %in% mlreg_prediction_predictor_numeric)
      num_predict_cat <- which(names(uploaded_mlreg_data_prediction) %in% mlreg_prediction_predictor_categori)
      if(length(num_predict_num) > 0 | length(num_predict_cat) > 0){
        mlreg_df_prediction <- data.frame(uploaded_mlreg_data_prediction[, c(mlreg_prediction_predictor_numeric, mlreg_prediction_predictor_categori)])
        mlreg_df_prediction
      } else{
        return(NULL)
      }
      if(length(num_predict_cat) > 0){
        for(i in mlreg_prediction_predictor_categori){
          mlreg_df_prediction[, i] <- factor(mlreg_df_prediction[, i])
        }
      }
      uploaded_mlreg_data <- isolate(uploaded_mlreg_data())
      response <- input$mlregresponse
      predictors_numeric <- input$mlregpredictors_numeric
      predictors_categori <- input$mlregpredictors_categorical
      res_name <- which(names(uploaded_mlreg_data) %in% response)
      pred_name_num <- which(names(uploaded_mlreg_data) %in% predictors_numeric)
      pred_name_cat <- which(names(uploaded_mlreg_data) %in% predictors_categori)
      if(length(res_name) > 0 | length(pred_name_num) > 0 | length(pred_name_cat) > 0){
        mlreg_df <- data.frame(uploaded_mlreg_data[, c(response, predictors_numeric, predictors_categori)])
      } else{
        return(NULL)
      }
      
      if(length(pred_name_cat) > 0){
        for(i in predictors_categori){
          mlreg_df[, i] <- factor(mlreg_df[, i])
        }
      }
      ############## check both file to be almost the same ##########################
      similar_name <- which(names(mlreg_df) %in% names(mlreg_df_prediction))
      if(length(similar_name) < 1){
        return(NULL)
      }
      #####################################################################################
      set.seed(123)
      split <- sample.split(mlreg_df[,1], SplitRatio = input$mlregratiosplit)
      training_set <- subset(mlreg_df, split == TRUE)
      test_set <- subset(mlreg_df, split == FALSE)
      if(input$mlregkfoldcv > 0) {
        folds <- createFolds(mlreg_df[,1] , k = input$mlregkfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          formula <- as.formula(paste0(names(mlreg_df)[1], " ~ ."))
          mlregmodel <- lm(formula = formula,
                           data = training_fold,
                           na.action = na.exclude)
          y_pred <- predict(mlregmodel, newdata = mlreg_df_prediction)
          return(y_pred)
        })
        Prediction <- round(colMeans(do.call(rbind, cv)), 3)
        Prediction <- data.frame(Prediction)
        colnames(Prediction) <- response
        Prediction
      } else{
        formula <- as.formula(paste0(names(mlreg_df)[1], " ~ ."))
        mlregmodel <- lm(formula = formula,
                         data = training_set,
                         na.action = na.exclude)
        y_pred <- predict(mlregmodel, newdata = mlreg_df_prediction)
        y_pred <- round(data.frame(y_pred),3)
        colnames(y_pred) <- response
        y_pred
      }
    }
  })
  
  ############ table of actual value for new data
  output$mlreg_actual_response <- renderDT({
    if(!is.null(input$mlreg_prediction_response) && input$mlregprepross_prediction_result) {
      uploaded_mlreg_data_prediction <- isolate(uploaded_mlreg_data_prediction())
      response_prediction <- input$mlreg_prediction_response
      mlreg_df_prediction <- data.frame(uploaded_mlreg_data_prediction[, response_prediction])
      mlreg_df_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 9),
          rownames = T)
    }
  })
  ############ table of predicted value for new data
  output$mlreg_predicted_response <- renderDT({
    req(mlreg_analysis_prediction())
    if(input$mlregprepross_prediction_result) {
      mlreg_analysis_prediction <- isolate(mlreg_analysis_prediction())
      mlreg_analysis_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 9),
          rownames = T)
    }
  })
  
  ### Download mlreg prediction table
  output$download_prediction_mlreg_table <- renderUI({
    if(input$mlregprepross_prediction_result) {
      downloadButton('mlregprediction_table', 'Download prediction table')
    }
  })
  
  output$mlregprediction_table <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_prediction_table.csv")
    },
    content = function(file){
      mlreg_analysis_prediction() %>%
        write_csv(file)
    }
  )
  ########################################################## ML-Regression-Polynomial  ######################
  #### erorr checking
  output$uploaded_polyreg_hide_tabpanel <- reactive({
    error_catghing_polyreg <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_polyreg)
      checknull <- NCOL(uploaded_polyreg_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_polyreg(input$uploaded_polyreg$name, input$datapolyregfileformat)
  })
  outputOptions(output, 'uploaded_polyreg_hide_tabpanel', suspendWhenHidden=FALSE)
  ################################### uploaded polyreg table ######################################################
  uploaded_polyreg_data <- eventReactive(input$uploaded_polyreg, {
    if(!is.null(input$uploaded_polyreg)){
      if(input$datapolyregfileformat == "," && input$polyregheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_polyreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$datapolyregfileformat == "," && input$polyregheader == "No") {
        rawdata <- try(read_csv(input$uploaded_polyreg$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datapolyregfileformat == "\t" && input$polyregheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_polyreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datapolyregfileformat == "\t" && input$polyregheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_polyreg$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ########################################### polyreg table
  output$polyregdatashow <- renderDT({
    req(input$uploaded_polyreg)
    if(input$polyregprepross) {
      uploaded_polyreg_table <- uploaded_polyreg_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE)
      uploaded_polyreg_table
    }
  })
  ########################### response and predictor choices #####################
  # response 
  observeEvent(uploaded_polyreg_data(), {
    if(!is.null(uploaded_polyreg_data())){
      dftype <- sapply(uploaded_polyreg_data(), class)
      isnumeric <- grep("numeric", dftype)
      response_var <- names(uploaded_polyreg_data())[isnumeric]
      updateSelectInput(
        session,
        "polyregresponse",
        choices = response_var)
    }
  })
  # numeric
  observeEvent(uploaded_polyreg_data(), {
    if(!is.null(uploaded_polyreg_data())){
      dftype <- sapply(uploaded_polyreg_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_var <- names(uploaded_polyreg_data())[isnumeric]
      updateSelectInput(
        session,
        "polyregpredictor",
        choices = predictor_var)
    }
  })
  # Polynomial regression result
  polyreg_analysis <- reactive({
    if(!is.null(uploaded_polyreg_data()) && input$polyregaction){
      if(is.null(input$polyregresponse) && is.null(input$polyregpredictor)){
        return(NULL)
      }
      uploaded_polyreg_data <- isolate(uploaded_polyreg_data())
      response <- input$polyregresponse
      predictor <- input$polyregpredictor
      res_name <- which(names(uploaded_polyreg_data) %in% response)
      pred_name <- which(names(uploaded_polyreg_data) %in% predictor)
      if(length(res_name) > 0 && length(pred_name) > 0){
        polyreg_df <- data.frame(uploaded_polyreg_data[, c(response, predictor)])
        colnames(polyreg_df) <- c("response", "predictor")
        polyreg_df
      } else{
        return(NULL)
      }
      
      set.seed(123)
      split <- sample.split(polyreg_df[,1], SplitRatio = input$polyregratiosplit)
      training_set <- subset(polyreg_df, split == TRUE)
      test_set <- subset(polyreg_df, split == FALSE)
      if(input$polyregkfoldcv > 0) {
        folds <- createFolds(polyreg_df[,1] , k = input$polyregkfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          polyregmodel <- lm(response ~ poly(predictor, input$polyregdegree, raw = TRUE),
                             data = training_fold,
                             na.action = na.exclude)
          y_pred <- predict(polyregmodel, newdata = test_fold)
          mse <- MLmetrics::MSE(y_pred, test_fold[,1])
          mae <- MLmetrics::MAE(y_pred, test_fold[,1])
          rmse <- MLmetrics::RMSE(y_pred, test_fold[,1])
          r2 <- MLmetrics::R2_Score(y_pred, test_fold[,1])
          accuracy <- data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2)
          return(accuracy)
        })
        Accuracy <- round(colMeans(do.call(rbind, cv)), 3)
        Accuracy <- data.frame(t(Accuracy))
        Accuracy
      } else{
        polyregmodel <- lm(response ~ poly(predictor, input$polyregdegree, raw = TRUE),
                           data = training_set,
                           na.action = na.exclude)
        y_pred <- predict(polyregmodel, newdata = test_set)
        mse <- MLmetrics::MSE(y_pred, test_set[,1])
        mae <- MLmetrics::MAE(y_pred, test_set[,1])
        rmse <- MLmetrics::RMSE(y_pred, test_set[,1])
        r2 <- MLmetrics::R2_Score(y_pred, test_set[,1])
        accuracy <- round(data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2),3)
        accuracy
      }
    }
  })
  ##################### polyreg result table 
  output$polyregoutput <- renderDT({
    if(!is.null(polyreg_analysis()) && input$polyregaction) {
      polyreg_analysis <- isolate(polyreg_analysis())
      polyreg_output <- polyreg_analysis %>%
        datatable(
          options = list(scrollX = TRUE, dom = "t"),
          rownames = FALSE)
      polyreg_output
    }
  })
  ######################################### polyreg plot ###################################
  polyreg_reactive <- reactive({
    if(!is.null(uploaded_polyreg_data()) && input$polyregaction){
      if(is.null(input$polyregresponse) && is.null(input$polyregpredictor)){
        return(NULL)
      }
      uploaded_polyreg_data <- isolate(uploaded_polyreg_data())
      response <- input$polyregresponse
      predictor <- input$polyregpredictor
      res_name <- which(names(uploaded_polyreg_data) %in% response)
      pred_name <- which(names(uploaded_polyreg_data) %in% predictor)
      if(length(res_name) > 0 && length(pred_name) > 0){
        polyreg_df <- data.frame(uploaded_polyreg_data[, c(response, predictor)])
        colnames(polyreg_df) <- c("response", "predictor")
        polyreg_df
      }else {
        return(NULL)
      }
    }
  })
  
  output$polyregplottraining <- renderPlot({
    if(!is.null(uploaded_polyreg_data()) && input$polyregplotview){
      if(is.null(input$polyregresponse) && is.null(input$polyregpredictor)){
        return(NULL)
      }
      uploaded_polyreg_data <- isolate(uploaded_polyreg_data())
      response <- input$polyregresponse
      predictor <- input$polyregpredictor
      res_name <- which(names(uploaded_polyreg_data) %in% response)
      pred_name <- which(names(uploaded_polyreg_data) %in% predictor)
      if(length(res_name) > 0 && length(pred_name) > 0){
        polyreg_df <- data.frame(uploaded_polyreg_data[, c(response, predictor)])
        namesyx <- names(polyreg_df)
        colnames(polyreg_df) <- c("response", "predictor")
        polyreg_df
      } else{
        return(NULL)
      }
      
      set.seed(123)
      split <- sample.split(polyreg_df[,1], SplitRatio = input$polyregratiosplit)
      training_set <- subset(polyreg_df, split == TRUE)
      test_set <- subset(polyreg_df, split == FALSE)
        polyregmodel <- lm(response ~ poly(predictor, input$polyregdegree, raw = TRUE),
                           data = training_set,
                           na.action = na.exclude)
        y <- predict(polyregmodel, newdata = test_set)
        p <- ggplot(training_set, aes(training_set[,2], training_set[,1]) ) + geom_point(colour = '#145A32') +
          stat_smooth(method = lm, formula = y ~ poly(x, input$polyregdegree, raw = TRUE), colour = "#9A7D0A") +
          xlab(namesyx[2]) + ylab(namesyx[1]) +
          theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                  axis.text = element_text(colour = "black"),
                                  axis.text.y = element_text(colour = "black")) +
          theme(axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                axis.text.y = element_text(family = "Times",colour = "black"),
                plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                axis.title.y = element_text(family = "Times", size = rel(1.4)),
                axis.title.x = element_text(family = "Times", size = rel(1.4)))+
          labs(subtitle = sprintf("Training set - polynomial degree = %d", input$polyregdegree))
        
        p
      }
  })
  ################################## polyreg test plot
  output$polyregplottest <- renderPlot({
    if(!is.null(uploaded_polyreg_data()) && input$polyregplotview){
      if(is.null(input$polyregresponse) && is.null(input$polyregpredictor)){
        return(NULL)
      }
      uploaded_polyreg_data <- isolate(uploaded_polyreg_data())
      response <- input$polyregresponse
      predictor <- input$polyregpredictor
      res_name <- which(names(uploaded_polyreg_data) %in% response)
      pred_name <- which(names(uploaded_polyreg_data) %in% predictor)
      if(length(res_name) > 0 && length(pred_name) > 0){
        polyreg_df <- data.frame(uploaded_polyreg_data[, c(response, predictor)])
        namesyx <- names(polyreg_df)
        colnames(polyreg_df) <- c("response", "predictor")
        polyreg_df
      } else{
        return(NULL)
      }
      set.seed(123)
      split <- sample.split(polyreg_df[,1], SplitRatio = input$polyregratiosplit)
      training_set <- subset(polyreg_df, split == TRUE)
      test_set <- subset(polyreg_df, split == FALSE)
        polyregmodel <- lm(response ~ poly(predictor, input$polyregdegree, raw = TRUE),
                           data = training_set,
                           na.action = na.exclude)
        y <- predict(polyregmodel, newdata = test_set)
        p <- ggplot(test_set, aes(test_set[,2], test_set[,1])) + geom_point(colour = '#145A32') +
          stat_smooth(method = lm, formula = y ~ poly(x, input$polyregdegree, raw = TRUE), colour = "#9A7D0A") +
          xlab(namesyx[2]) + ylab(namesyx[1]) +
          theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                  axis.text = element_text(colour = "black"),
                                  axis.text.y = element_text(colour = "black")) +
          theme(axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                axis.text.y = element_text(family = "Times",colour = "black"),
                plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                axis.title.y = element_text(family = "Times", size = rel(1.4)),
                axis.title.x = element_text(family = "Times", size = rel(1.4)))+
          labs(subtitle = sprintf("Test set - polynomial degree = %d", input$polyregdegree))
        p
      }
  })
  ############################################## polyreg prediction on new data ##############################
  #### error checking
  output$uploaded_polyregdata_prediction_hide_tabpanel <- reactive({
    error_catghing_polyreg_prediction <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_polyreg_prediction)
      checknull <- NCOL(uploaded_polyreg_data_prediction()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_polyreg_prediction(input$uploaded_polyreg_prediction$name, input$datapolyregfileformat_prediction)
  })
  outputOptions(output, 'uploaded_polyregdata_prediction_hide_tabpanel', suspendWhenHidden=FALSE)
  ############### uploaded prediction table
  uploaded_polyreg_data_prediction <- eventReactive(input$uploaded_polyreg_prediction, {
    if(!is.null(input$uploaded_polyreg_prediction)){
      if(input$datapolyregfileformat == "," && input$polyregheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_polyreg_prediction$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if(input$datapolyregfileformat == "," && input$polyregheader == "No") {
        rawdata <- try(read_csv(input$uploaded_polyreg_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datapolyregfileformat == "\t" && input$polyregheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_polyreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datapolyregfileformat == "\t" && input$polyregheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_polyreg_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ############### new table for prediction choices
  # response and predictor choices
  observeEvent(uploaded_polyreg_data_prediction(), {
    if(!is.null(uploaded_polyreg_data_prediction())){
      dftype <- sapply(uploaded_polyreg_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      response_vars <- names(uploaded_polyreg_data_prediction())[isnumeric]
      updateSelectInput(
        session,
        "polyreg_prediction_response",
        choices = response_vars)
    }
  })
  # numeric
  observeEvent(uploaded_polyreg_data_prediction(), {
    if(!is.null(uploaded_polyreg_data_prediction())){
      dftype <- sapply(uploaded_polyreg_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_var <- names(uploaded_polyreg_data_prediction())[isnumeric]
      updatePickerInput(
        session,
        "polyreg_prediction_predictor",
        choices = predictor_var)
    }
  })
  ##################################################### polyreg prediction result ######################
  polyreg_analysis_prediction <- reactive({
    if(!is.null(uploaded_polyreg_data()) && input$polyregprepross_prediction_result){
      if(is.null(input$polyregresponse) && is.null(input$polyregpredictor)){
        return(NULL)
      }
      ############ for polyreg prediction
      uploaded_polyreg_data_prediction <- isolate(uploaded_polyreg_data_prediction())
      polyreg_prediction_predictor <- input$polyreg_prediction_predictor
      num_predict <- which(names(uploaded_polyreg_data_prediction) %in% polyreg_prediction_predictor)
      if(length(num_predict) > 0 ){
        polyreg_df_prediction <- data.frame(uploaded_polyreg_data_prediction[, polyreg_prediction_predictor])
        colnames(polyreg_df_prediction) <- "predictor"
        polyreg_df_prediction
      } else{
        return(NULL)
      }
      uploaded_polyreg_data <- isolate(uploaded_polyreg_data())
      response <- input$polyregresponse
      predictor <- input$polyregpredictor
      res_name <- which(names(uploaded_polyreg_data) %in% response)
      pred_name <- which(names(uploaded_polyreg_data) %in% predictor)
      if(length(res_name) > 0 && length(pred_name) > 0){
        polyreg_df <- data.frame(uploaded_polyreg_data[, c(response, predictor)])
        colnames(polyreg_df) <- c("response", "predictor")
        polyreg_df
      }
      ############## check both file to be almost the same ##########################
      similar_name <- which(names(polyreg_df) %in% names(polyreg_df_prediction))
      if(length(similar_name) < 1){
        return(NULL)
      }
      #####################################################################################
      set.seed(123)
      split <- sample.split(polyreg_df[,1], SplitRatio = input$polyregratiosplit)
      training_set <- subset(polyreg_df, split == TRUE)
      test_set <- subset(polyreg_df, split == FALSE)
      if(input$polyregkfoldcv > 0) {
        folds <- createFolds(polyreg_df[,1] , k = input$polyregkfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          polyregmodel <- lm(response ~ poly(predictor, input$polyregdegree, raw = TRUE),
                             data = training_fold,
                             na.action = na.exclude)
          y_pred <- predict(polyregmodel, newdata = polyreg_df_prediction)
          return(y_pred)
        })
        Prediction <- round(colMeans(do.call(rbind, cv)), 3)
        Prediction <- data.frame(Prediction)
        colnames(Prediction) <- response
        Prediction
      } else{
        polyregmodel <- lm(response ~ poly(predictor, input$polyregdegree, raw = TRUE),
                           data = training_set,
                           na.action = na.exclude)
        y_pred <- predict(polyregmodel, newdata = polyreg_df_prediction)
        y_pred <- round(data.frame(y_pred),3)
        colnames(y_pred) <- response
        y_pred
      }
    }
  })
  ############ table of actual value for new data
  output$polyreg_actual_response <- renderDT({
    if(!is.null(input$polyreg_prediction_response) && input$polyregprepross_prediction_result) {
      uploaded_polyreg_data_prediction <- isolate(uploaded_polyreg_data_prediction())
      response_prediction <- input$polyreg_prediction_response
      polyreg_df_prediction <- data.frame(uploaded_polyreg_data_prediction[, response_prediction])
      polyreg_df_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 7),
          rownames = T)
    }
  })
  ############ table of predicted value for new data
  output$polyreg_predicted_response <- renderDT({
    req(polyreg_analysis_prediction())
    if(input$polyregprepross_prediction_result) {
      polyreg_analysis_prediction <- isolate(polyreg_analysis_prediction())
      polyreg_analysis_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 7),
          rownames = T)
    }
  })
  
  ### Download polyreg prediction table
  output$download_prediction_polyreg_table <- renderUI({
    if(input$polyregprepross_prediction_result) {
      downloadButton('polyregprediction_table', 'Download prediction table')
    }
  })
  
  output$polyregprediction_table <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_prediction_table.csv")
    },
    content = function(file){
      polyreg_analysis_prediction() %>%
        write_csv(file)
    }
  )
  ########################################################## ML-Regression-Logistic ######################
  #### error checking
  output$uploaded_logitreg_hide_tabpanel <- reactive({
    error_catghing_logitreg <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_logitreg)
      checknull <- NCOL(uploaded_logitreg_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_logitreg(input$uploaded_logitreg$name, input$datalogitregfileformat)
  })
  outputOptions(output, 'uploaded_logitreg_hide_tabpanel', suspendWhenHidden=FALSE)
  ################################### uploaded logitreg table ######################################################
  uploaded_logitreg_data <- eventReactive(input$uploaded_logitreg,{
    if(!is.null(input$uploaded_logitreg)){
      if(input$datalogitregfileformat == "," && input$logitregheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_logitreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$datalogitregfileformat == "," && input$logitregheader == "No") {
        rawdata <- try(read_csv(input$uploaded_logitreg$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datalogitregfileformat == "\t" && input$logitregheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_logitreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datalogitregfileformat == "\t" && input$logitregheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_logitreg$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ########################################### logitreg table
  output$logitregdatashow <- renderDT({
    req(input$uploaded_logitreg)
    if(input$logitregprepross) {
      uploaded_logitreg_table <- uploaded_logitreg_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE)
      uploaded_logitreg_table
    }
  })
  ########################### response and predictor choices #####################
  # response 
  observeEvent(uploaded_logitreg_data(), {
    if(!is.null(uploaded_logitreg_data())){
      response_var <- names(uploaded_logitreg_data())
      updateSelectInput(
        session,
        "logitregresponse",
        choices = response_var)
    }
  })
  # numeric
  observeEvent(uploaded_logitreg_data(), {
    if(!is.null(uploaded_logitreg_data())){
      dftype <- sapply(uploaded_logitreg_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_var <- names(uploaded_logitreg_data())[isnumeric]
      updatePickerInput(
        session,
        "logitregpredictors_numeric",
        choices = predictor_var)
    }
  })
  # categorical
  observeEvent(uploaded_logitreg_data(), {
    if(!is.null(uploaded_logitreg_data())){
      dftype <- sapply(uploaded_logitreg_data(), class)
      ischar <- grep("character|logical", dftype)
      predictor_var <- names(uploaded_logitreg_data())[ischar]
      updatePickerInput(
        session,
        "logitregpredictors_categorical",
        choices = predictor_var)
    }
  })
  # LOGISTIC regression result
  logitreg_analysis <- reactive({
    if(!is.null(uploaded_logitreg_data()) && input$logitregaction){
      if(is.null(input$logitregresponse) | is.null(input$logitregpredictors_numeric) && is.null(input$logitregpredictors_categorical)){
        return(NULL)
      }
      uploaded_logitreg_data <- isolate(uploaded_logitreg_data())
      response <- input$logitregresponse
      predictors_numeric <- input$logitregpredictors_numeric
      predictors_categori <- input$logitregpredictors_categorical
      res_name <- which(names(uploaded_logitreg_data) %in% response)
      pred_name_num <- which(names(uploaded_logitreg_data) %in% predictors_numeric)
      pred_name_cat <- which(names(uploaded_logitreg_data) %in% predictors_categori)
      if(length(res_name) > 0 | length(pred_name_num) > 0 | length(pred_name_cat) > 0){
        logitreg_df <- data.frame(uploaded_logitreg_data[, c(predictors_numeric, predictors_categori, response)])
        fac <- length(logitreg_df)
        logitreg_df[,fac] <- factor(logitreg_df[,fac])
      } else{
        return(NULL)
      }
      
      if(length(pred_name_cat) > 0){
        for(i in predictors_categori){
          logitreg_df[, i] <- factor(logitreg_df[, i])
        }
      }

      set.seed(123)
      split <- sample.split(logitreg_df[,fac], SplitRatio = input$logitregratiosplit)
      training_set <- subset(logitreg_df, split == TRUE)
      test_set <- subset(logitreg_df, split == FALSE)
      
      factors <- which(names(logitreg_df) %in% predictors_categori)
      if(length(factors) > 0){
        training_set[c(-fac, -factors)] <- scale(training_set[c(-fac, -factors)])
        test_set[c(-fac, -factors)] <- scale(test_set[c(-fac, -factors)])
      }
      else{
        training_set[-fac] <- scale(training_set[-fac])
        test_set[-fac] <- scale(test_set[-fac])
      }
 
      if(input$logitregkfoldcv > 0) {
        folds <- createFolds(logitreg_df[,fac] , k = input$logitregkfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          ### weights
          userclass <- input$logitregweights
          if(userclass == ""){
            weights <- NULL
          } else{
            nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
            nu_u_c <- na.omit(nu_u_c)
            classes <- levels(factor(training_fold[,fac]))
            if(length(nu_u_c) == length(classes)){
              w <- list()
              for(i in 1:length(training_fold[,fac])){
                w[[i]] <- ifelse(classes[1] == training_fold[i,fac],  nu_u_c[1], nu_u_c[2])
              }
              weights <- do.call(rbind, w)
            } else{
              weights <- NULL
            } 
          }
          formula <- as.formula(paste0(names(logitreg_df)[fac], " ~ ."))
          logitregmodel <- glm(formula = formula,
                               data = training_fold,
                               family = "binomial",
                               weights = weights[,1],
                               na.action = na.exclude)
          y_pred <- predict(logitregmodel, newdata = test_fold, type = "response")
          y_pred <- ifelse(y_pred > input$logitregprob, 1, 0)
          result <- confusion_matrix(test_fold[, fac], y_pred)
          return(result)
        })
        multiclass_con_av(cv)
      } else{
        ### weights
        userclass <- input$logitregweights
        if(userclass == ""){
          weights <- NULL
        } else{
          nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
          nu_u_c <- na.omit(nu_u_c)
          classes <- levels(factor(training_set[,fac]))
          if(length(nu_u_c) == length(classes)){
            w <- list()
            for(i in 1:length(training_set[,fac])){
              w[[i]] <- ifelse(classes[1] == training_set[i,fac],  nu_u_c[1], nu_u_c[2])
            }
            weights <- do.call(rbind, w)
          } else{
            weights <- NULL
          } 
        }
        formula <- as.formula(paste0(names(logitreg_df)[fac], " ~ ."))
        logitregmodel <- glm(formula = formula,
                             data = training_set,
                             family = "binomial",
                             weights = weights[,1],
                             na.action = na.exclude)
        y_pred <- predict(logitregmodel, newdata = test_set, type = "response")
        y_pred <- ifelse(y_pred > input$logitregprob, 1, 0)
        confusion_matrix(test_set[, fac], y_pred)
      }
    }
  })
  ##################### logitreg result table 
  output$logitregoutput <- renderDT({
    if(!is.null(logitreg_analysis()) && input$logitregaction) {
      logitreg_analysis <- isolate(logitreg_analysis())
      logitreg_output <- logitreg_analysis %>%
        datatable(
          options = list(scrollX = TRUE, dom = "t"),
          rownames = TRUE)
      logitreg_output
    }
  })
  ########################################### logistic plot ##########################
  logitreg_reactive <- reactive({
    if(!is.null(uploaded_logitreg_data()) && input$logitregaction){
      if(is.null(input$logitregresponse) | is.null(input$logitregpredictors_numeric) && is.null(input$logitregpredictors_categorical)){
        return(NULL)
      }
      uploaded_logitreg_data <- isolate(uploaded_logitreg_data())
      response <- input$logitregresponse
      predictors_numeric <- input$logitregpredictors_numeric
      predictors_categori <- input$logitregpredictors_categorical
      res_name <- which(names(uploaded_logitreg_data) %in% response)
      pred_name_num <- which(names(uploaded_logitreg_data) %in% predictors_numeric)
      pred_name_cat <- which(names(uploaded_logitreg_data) %in% predictors_categori)
      if(length(res_name) > 0 | length(pred_name_num) > 0 | length(pred_name_cat) > 0){
        logitreg_df <- data.frame(uploaded_logitreg_data[, c(predictors_numeric, predictors_categori, response)])
        logitreg_df
      } else{
        return(NULL)
      }
    }
    })  
  # choice x
  observeEvent(logitreg_reactive(),{
    if(!is.null(logitreg_reactive())){
      res_fac <- length(logitreg_reactive())
      dftype <- sapply(logitreg_reactive(), class)
      isnumeric <- grep("numeric", dftype)
      ischar <- grep("character|logical", dftype[res_fac])
      if(length(ischar) == 1){
        logisticregplotchoicex <- names(logitreg_reactive())[isnumeric]
      }
      else if(length(ischar) == 0){
        logisticregplotchoicex <- names(logitreg_reactive())[isnumeric]
        len <- length(logisticregplotchoicex)
        logisticregplotchoicex <- logisticregplotchoicex[-len]
      }
      updateSelectInput(
        session,
        "logitregplotx",
        choices = logisticregplotchoicex)
    }
  })
  # choice y
  observeEvent(logitreg_reactive(),{
    if(!is.null(logitreg_reactive())){
      res_fac <- length(logitreg_reactive())
      dftype <- sapply(logitreg_reactive(), class)
      isnumeric <- grep("numeric", dftype)
      ischar <- grep("character|logical", dftype[res_fac])
      if(length(ischar) == 1){
        logisticregplotchoicey <- names(logitreg_reactive())[isnumeric]
        len <- length(logisticregplotchoicey)
        logisticregplotchoicey <- logisticregplotchoicey[len:1]
      }
      else if(length(ischar) == 0){
        logisticregplotchoicey <- names(logitreg_reactive())[isnumeric]
        len <- length(logisticregplotchoicey)
        logisticregplotchoicey <- logisticregplotchoicey[len:1][-1]
      }
      updateSelectInput(
        session,
        "logitregploty",
        choices = logisticregplotchoicey)
    }
  })
  #################### hide choices for PCA
  output$hide_logitreg_choices_pca <- reactive({
    if(!is.null(logitreg_reactive())){
    hidelogitregpca <- function(){
      df_len <- length(logitreg_reactive())
      dftype <- sapply(logitreg_reactive(), class)
      isnumeric <- grep("numeric", dftype[-df_len])
      if(length(isnumeric) >= 3){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    hidelogitregpca()
    }
  })
  outputOptions(output, 'hide_logitreg_choices_pca', suspendWhenHidden=FALSE)
  ###################################### display waiting massage for logistic regression plots ############
  logitregplot_waiting <- reactiveValues(ok = FALSE)
  observeEvent(input$logitregplotview, {
    if(input$logitregplotview == 1 && !is.null(logitreg_analysis())){
      shinyjs::show("logitregplotwait")
      logitregplot_waiting$ok <- FALSE
      Sys.sleep(5)
      logitregplot_waiting$ok <- TRUE
      shinyjs::hide("logitregplotwait")
    }
  })
  ################# training logistic plot
  output$logitregplottraining <- renderPlot({
    if(!is.null(uploaded_logitreg_data()) && input$logitregplotview){
      if(is.null(input$logitregresponse) | is.null(input$logitregpredictors_numeric) && is.null(input$logitregpredictors_categorical)){
        return(NULL)
      }
      uploaded_logitreg_data <- isolate(uploaded_logitreg_data())
      response <- input$logitregresponse
      predictors_numeric <- input$logitregpredictors_numeric
      predictors_categori <- input$logitregpredictors_categorical
      res_name <- which(names(uploaded_logitreg_data) %in% response)
      pred_name_num <- which(names(uploaded_logitreg_data) %in% predictors_numeric)
      pred_name_cat <- which(names(uploaded_logitreg_data) %in% predictors_categori)
      if(length(res_name) > 0 | length(pred_name_num) > 0 | length(pred_name_cat) > 0){
        logitreg_df <- data.frame(uploaded_logitreg_data[, c(predictors_numeric, predictors_categori, response)])
        fac <- length(logitreg_df)
        logitreg_df[,fac] <- factor(logitreg_df[,fac])
      } else{
        return(NULL)
      }
      
      if(length(pred_name_cat) > 0){
        for(i in predictors_categori){
          logitreg_df[, i] <- factor(logitreg_df[, i])
        }
      }
      
      set.seed(123)
      split <- sample.split(logitreg_df[,fac], SplitRatio = input$logitregratiosplit)
      training_set <- subset(logitreg_df, split == TRUE)
      test_set <- subset(logitreg_df, split == FALSE)
      
      factors <- which(names(logitreg_df) %in% predictors_categori)
      if(length(factors) > 0){
        training_set[c(-fac, -factors)] <- scale(training_set[c(-fac, -factors)])
        test_set[c(-fac, -factors)] <- scale(test_set[c(-fac, -factors)])
      }
      else{
        training_set[-fac] <- scale(training_set[-fac])
        test_set[-fac] <- scale(test_set[-fac])
      }
      
      numeric_only <- length(logitreg_df) - length(factors)
        ### weights
        userclass <- input$logitregweights
        if(userclass == ""){
          weights <- NULL
        } else{
          nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
          nu_u_c <- na.omit(nu_u_c)
          classes <- levels(factor(training_set[,fac]))
          if(length(nu_u_c) == length(classes)){
            w <- list()
            for(i in 1:length(training_set[,fac])){
              w[[i]] <- ifelse(classes[1] == training_set[i,fac],  nu_u_c[1], nu_u_c[2])
            }
            weights <- do.call(rbind, w)
          } else{
            weights <- NULL
          }
        }
        numeric_only <- length(logitreg_df) - length(factors)
        if(numeric_only == 3 ){
          par(mar=c(4,4,5,5))
          set <- training_set
          X1 <- seq(min(set[, input$logitregplotx]) - 1, max(set[, input$logitregplotx]) + 1, by = 0.05)
          X2 <- seq(min(set[, input$logitregploty]) - 1, max(set[, input$logitregploty]) + 1, by = 0.05)
          grid_set <- expand.grid(X1, X2)
          colnames(grid_set) <- c(input$logitregplotx, input$logitregploty)
          formula <- as.formula(paste0(names(logitreg_df)[fac], " ~ ."))
          logitregmodel <- glm(formula = formula,
                               data = training_set,
                               family = "binomial",
                               weights = weights[,1],
                               na.action = na.exclude)
          y_pred <- predict(logitregmodel, newdata = grid_set, type = "response")
          y_pred <- ifelse(y_pred > input$logitregprob, 1, 0)
          qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
          col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
          col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
          len_1 <- length(levels(factor(y_pred)))
          len_2 <- length(levels(factor(set[,fac])))
          level_1 <- levels(factor(y_pred))
          level_2 <- levels(factor(set[,fac]))
          col_1 <- col_vector[1:len_1]
          col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
          color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
          color_2 <- as.character(factor(set[,fac], levels = level_2, labels = col_2))
          p <- plot(set[, -fac],
                    main = 'Logistic Regression (Training set)',
                    xlab = input$logitregplotx, ylab = input$logitregploty,
                    xlim = range(X1), ylim = range(X2))
          contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
          points(grid_set, pch = '.', col = color_1)
          points(set, pch = 21, bg = color_2)
          legend(x = "topright",
                 inset = c(-.062, .4),
                 title = "Classes",
                 legend = level_2,
                 col = col_2,
                 bg = rgb(1, 0, 0, alpha = 0.15),
                 box.lty = 0,
                 pch = 19,
                 xpd = TRUE)
          p

        } else if(numeric_only > 3){
          
          pred_train <- training_set[fac]
          pred_test <- test_set[fac]
          pca <- princomp(x = training_set[-fac],  cor = TRUE, scores = TRUE, covmat = NULL)
          varpca <- pca$sdev^2 / sum(pca$sdev^2)
          training_set <- predict(pca, training_set)
          training_set <- data.frame(training_set[, 1:2])
          training_set <- cbind(training_set , pred_train)
          colnames(training_set) <- c("PC1", "PC2", "response")
          training_set$response <- factor(training_set$response)
          test_set <- predict(pca, test_set)
          test_set <- data.frame(test_set[, 1:2])
          test_set <- cbind(test_set, pred_test)
          colnames(test_set) <- c("PC1", "PC2", "response")
          test_set$response <- factor(test_set$response)
          set <- training_set
          X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
          X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.05)
          grid_set <- expand.grid(X1, X2)
          colnames(grid_set) <- c("PC1", "PC2")
          formula <- as.formula(paste0(names(training_set)[3], " ~ ."))
          logitregmodel <- glm(formula = formula,
                               data = training_set,
                               family = "binomial",
                               weights = weights[,1],
                               na.action = na.exclude)
          y_pred <- predict(logitregmodel, newdata = grid_set, type = "response")
          y_pred <- ifelse(y_pred > input$logitregprob, 1, 0)
          qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
          col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
          col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
          len_1 <- length(levels(factor(y_pred)))
          len_2 <- length(levels(factor(set[,3])))
          level_1 <- levels(factor(y_pred))
          level_2 <- levels(factor(set[,3]))
          col_1 <- col_vector[1:len_1]
          col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
          color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
          color_2 <- as.character(factor(set[,3], levels = level_2, labels = col_2))
          par(mar=c(4,4,5,5))
          p <- plot(set[, -3],
                    main = 'Logistic Regression (Training set)',
                    xlab = sprintf("PC1 (%2.2f%%)", varpca[1]*100), ylab = sprintf("PC2 (%2.2f%%)", varpca[2]*100),
                    xlim = range(X1), ylim = range(X2))
          contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
          points(grid_set, pch = '.', col = color_1)
          points(set, pch = 21, bg = color_2)
          legend(x = "topright",
                 inset = c(-.062, .4),
                 title = "Classes",
                 legend = level_2,
                 col = col_2,
                 bg = rgb(1, 0, 0, alpha = 0.15),
                 box.lty = 0,
                 pch = 19,
                 xpd = TRUE)
          p
        }
    }
  })
  ########################################### logistic test plot ###########################################
  output$logitregplottest <- renderPlot({
    if(!is.null(uploaded_logitreg_data()) && input$logitregplotview){
      if(is.null(input$logitregresponse) | is.null(input$logitregpredictors_numeric) && is.null(input$logitregpredictors_categorical)){
        return(NULL)
      }
      uploaded_logitreg_data <- isolate(uploaded_logitreg_data())
      response <- input$logitregresponse
      predictors_numeric <- input$logitregpredictors_numeric
      predictors_categori <- input$logitregpredictors_categorical
      res_name <- which(names(uploaded_logitreg_data) %in% response)
      pred_name_num <- which(names(uploaded_logitreg_data) %in% predictors_numeric)
      pred_name_cat <- which(names(uploaded_logitreg_data) %in% predictors_categori)
      if(length(res_name) > 0 | length(pred_name_num) > 0 | length(pred_name_cat) > 0){
        logitreg_df <- data.frame(uploaded_logitreg_data[, c(predictors_numeric, predictors_categori, response)])
        fac <- length(logitreg_df)
        logitreg_df[,fac] <- factor(logitreg_df[,fac])
      } else{
        return(NULL)
      }
      
      if(length(pred_name_cat) > 0){
        for(i in predictors_categori){
          logitreg_df[, i] <- factor(logitreg_df[, i])
        }
      }
      
      set.seed(123)
      split <- sample.split(logitreg_df[,fac], SplitRatio = input$logitregratiosplit)
      training_set <- subset(logitreg_df, split == TRUE)
      test_set <- subset(logitreg_df, split == FALSE)
      
      factors <- which(names(logitreg_df) %in% predictors_categori)
      if(length(factors) > 0){
        training_set[c(-fac, -factors)] <- scale(training_set[c(-fac, -factors)])
        test_set[c(-fac, -factors)] <- scale(test_set[c(-fac, -factors)])
      }
      else{
        training_set[-fac] <- scale(training_set[-fac])
        test_set[-fac] <- scale(test_set[-fac])
      }
      
      numeric_only <- length(logitreg_df) - length(factors)
        ### weights
        userclass <- input$logitregweights
        if(userclass == ""){
          weights <- NULL
        } else{
          nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
          nu_u_c <- na.omit(nu_u_c)
          classes <- levels(factor(training_set[,fac]))
          if(length(nu_u_c) == length(classes)){
            w <- list()
            for(i in 1:length(training_set[,fac])){
              w[[i]] <- ifelse(classes[1] == training_set[i,fac],  nu_u_c[1], nu_u_c[2])
            }
            weights <- do.call(rbind, w)
          } else{
            weights <- NULL
          }
        }
        numeric_only <- length(logitreg_df) - length(factors)
        if(numeric_only == 3 ){
          set <- test_set
          X1 <- seq(min(set[, input$logitregplotx]) - 1, max(set[, input$logitregplotx]) + 1, by = 0.05)
          X2 <- seq(min(set[, input$logitregploty]) - 1, max(set[, input$logitregploty]) + 1, by = 0.05)
          grid_set <- expand.grid(X1, X2)
          colnames(grid_set) <- c(input$logitregplotx, input$logitregploty)
          formula <- as.formula(paste0(names(logitreg_df)[fac], " ~ ."))
          logitregmodel <- glm(formula = formula,
                               data = training_set,
                               family = "binomial",
                               weights = weights[,1],
                               na.action = na.exclude)
          y_pred <- predict(logitregmodel, newdata = grid_set, type = "response")
          y_pred <- ifelse(y_pred > input$logitregprob, 1, 0)
          qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
          col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
          col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
          len_1 <- length(levels(factor(y_pred)))
          len_2 <- length(levels(factor(set[,fac])))
          level_1 <- levels(factor(y_pred))
          level_2 <- levels(factor(set[,fac]))
          col_1 <- col_vector[1:len_1]
          col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
          color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
          color_2 <- as.character(factor(set[,fac], levels = level_2, labels = col_2))
          par(mar=c(4,4,5,5))
          p <- plot(set[, -fac],
                    main = 'Logistic Regression (Test set)',
                    xlab = input$logitregplotx, ylab = input$logitregploty,
                    xlim = range(X1), ylim = range(X2))
          contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
          points(grid_set, pch = '.', col = color_1)
          points(set, pch = 21, bg = color_2)
          legend(x = "topright",
                 inset = c(-.062, .4),
                 title = "Classes",
                 legend = level_2,
                 col = col_2,
                 bg = rgb(1, 0, 0, alpha = 0.15),
                 box.lty = 0,
                 pch = 19,
                 xpd = TRUE)
          p
          
        } else if(numeric_only > 3){
          par(mar=c(4,4,5,5))
          pred_train <- training_set[fac]
          pred_test <- test_set[fac]
          pca <- princomp(x = training_set[-fac],  cor = TRUE, scores = TRUE, covmat = NULL)
          varpca <- pca$sdev^2 / sum(pca$sdev^2)
          training_set <- predict(pca, training_set)
          training_set <- data.frame(training_set[, 1:2])
          training_set <- cbind(training_set , pred_train)
          colnames(training_set) <- c("PC1", "PC2", "response")
          training_set$response <- factor(training_set$response)
          test_set <- predict(pca, test_set)
          test_set <- data.frame(test_set[, 1:2])
          test_set <- cbind(test_set, pred_test)
          colnames(test_set) <- c("PC1", "PC2", "response")
          test_set$response <- factor(test_set$response)
          set <- test_set
          X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
          X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.05)
          grid_set <- expand.grid(X1, X2)
          colnames(grid_set) <- c("PC1", "PC2")
          formula <- as.formula(paste0(names(training_set)[3], " ~ ."))
          logitregmodel <- glm(formula = formula,
                               data = training_set,
                               family = "binomial",
                               weights = weights[,1],
                               na.action = na.exclude)
          y_pred <- predict(logitregmodel, newdata = grid_set, type = "response")
          y_pred <- ifelse(y_pred > input$logitregprob, 1, 0)
          qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
          col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
          col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
          len_1 <- length(levels(factor(y_pred)))
          len_2 <- length(levels(factor(set[,3])))
          level_1 <- levels(factor(y_pred))
          level_2 <- levels(factor(set[,3]))
          col_1 <- col_vector[1:len_1]
          col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
          color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
          color_2 <- as.character(factor(set[,3], levels = level_2, labels = col_2))
          p <- plot(set[, -3],
                    main = 'Logistic Regression (Test set)',
                    xlab = sprintf("PC1 (%2.2f%%)", varpca[1]*100), ylab = sprintf("PC2 (%2.2f%%)", varpca[2]*100),
                    xlim = range(X1), ylim = range(X2))
          contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
          points(grid_set, pch = '.', col = color_1)
          points(set, pch = 21, bg = color_2)
          legend(x = "topright",
                 inset = c(-.062, .4),
                 title = "Classes",
                 legend = level_2,
                 col = col_2,
                 bg = rgb(1, 0, 0, alpha = 0.15),
                 box.lty = 0,
                 pch = 19,
                 xpd = TRUE)
          p
        }
      }
  })
  ############################################## logitreg prediction on new data ##############################
  #### error checking
  output$uploaded_logitregdata_prediction_hide_tabpanel <- reactive({
    error_catghing_logitreg_prediction <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_logitreg_prediction)
      checknull <- NCOL(uploaded_logitreg_data_prediction()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_logitreg_prediction(input$uploaded_logitreg_prediction$name, input$datalogitregfileformat_prediction)
  })
  outputOptions(output, 'uploaded_logitregdata_prediction_hide_tabpanel', suspendWhenHidden=FALSE)
  ############### uploaded prediction table
  uploaded_logitreg_data_prediction <- eventReactive(input$uploaded_logitreg_prediction, {
    if(!is.null(input$uploaded_logitreg_prediction)){
      if(input$datalogitregfileformat == "," && input$logitregheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_logitreg_prediction$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if(input$datalogitregfileformat == "," && input$logitregheader == "No") {
        rawdata <- try(read_csv(input$uploaded_logitreg_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datalogitregfileformat == "\t" && input$logitregheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_logitreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datalogitregfileformat == "\t" && input$logitregheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_logitreg_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ############### new table for prediction choices
  # response and predictor choices
  observeEvent(uploaded_logitreg_data_prediction(), {
    if(!is.null(uploaded_logitreg_data_prediction())){
      response_vars <- names(uploaded_logitreg_data_prediction())
      updateSelectInput(
        session,
        "logitreg_prediction_response",
        choices = response_vars)
    }
  })
  # numeric
  observeEvent(uploaded_logitreg_data_prediction(), {
    if(!is.null(uploaded_logitreg_data_prediction())){
      dftype <- sapply(uploaded_logitreg_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_var <- names(uploaded_logitreg_data_prediction())[isnumeric]
      updatePickerInput(
        session,
        "logitreg_prediction_predictor_numeric",
        choices = predictor_var)
    }
  })
  ## categoric
  observeEvent(uploaded_logitreg_data_prediction(), {
    if(!is.null(uploaded_logitreg_data_prediction())){
      dftype <- sapply(uploaded_logitreg_data_prediction(), class)
      ischar <- grep("character|logical", dftype)
      predictor_var <- names(uploaded_logitreg_data_prediction())[ischar]
      updatePickerInput(
        session,
        "logitreg_prediction_predictor_categorical",
        choices = predictor_var)
    }
  })
  logitreg_analysis_prediction <- reactive({
    if(!is.null(uploaded_logitreg_data()) && input$logitregaction){
      if(is.null(input$logitregresponse) | is.null(input$logitregpredictors_numeric) && is.null(input$logitregpredictors_categorical)){
        return(NULL)
      }
      ############ for logitreg prediction
      uploaded_logitreg_data_prediction <- isolate(uploaded_logitreg_data_prediction())
      logitreg_prediction_predictor_numeric <- input$logitreg_prediction_predictor_numeric
      logitreg_prediction_predictor_categori <- input$logitreg_prediction_predictor_categorical
      num_predict_num <- which(names(uploaded_logitreg_data_prediction) %in% logitreg_prediction_predictor_numeric)
      num_predict_cat <- which(names(uploaded_logitreg_data_prediction) %in% logitreg_prediction_predictor_categori)
      if(length(num_predict_num) > 0 | length(num_predict_cat) > 0){
        logitreg_df_prediction <- data.frame(uploaded_logitreg_data_prediction[, c(logitreg_prediction_predictor_numeric, logitreg_prediction_predictor_categori)])
        logitreg_df_prediction
      } else{
        return(NULL)
      }
      if(length(num_predict_cat) > 0){
        for(i in logitreg_prediction_predictor_categori){
          logitreg_df_prediction[, i] <- factor(logitreg_df_prediction[, i])
        }
      }
      uploaded_logitreg_data <- isolate(uploaded_logitreg_data())
      response <- input$logitregresponse
      predictors_numeric <- input$logitregpredictors_numeric
      predictors_categori <- input$logitregpredictors_categorical
      res_name <- which(names(uploaded_logitreg_data) %in% response)
      pred_name_num <- which(names(uploaded_logitreg_data) %in% predictors_numeric)
      pred_name_cat <- which(names(uploaded_logitreg_data) %in% predictors_categori)
      if(length(res_name) > 0 | length(pred_name_num) > 0 | length(pred_name_cat) > 0){
        logitreg_df <- data.frame(uploaded_logitreg_data[, c(predictors_numeric, predictors_categori, response)])
        fac <- length(logitreg_df)
        logitreg_df[,fac] <- factor(logitreg_df[,fac])
      } else{
        return(NULL)
      }
      
      if(length(pred_name_cat) > 0){
        for(i in predictors_categori){
          logitreg_df[, i] <- factor(logitreg_df[, i])
        }
      }
      ############## check both file to be almost the same ##########################
      similar_name <- which(names(logitreg_df) %in% names(logitreg_df_prediction))
      if(length(similar_name) < 1){
        return(NULL)
      }
      #####################################################################################
      set.seed(123)
      split <- sample.split(logitreg_df[,fac], SplitRatio = input$logitregratiosplit)
      training_set <- subset(logitreg_df, split == TRUE)
      test_set <- subset(logitreg_df, split == FALSE)
      if(input$logitregkfoldcv > 0) {
        folds <- createFolds(logitreg_df[,fac] , k = input$logitregkfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          ### weights
          userclass <- input$logitregweights
          if(userclass == ""){
            weights <- NULL
          } else{
            nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
            nu_u_c <- na.omit(nu_u_c)
            classes <- levels(factor(training_fold[,fac]))
            if(length(nu_u_c) == length(classes)){
              w <- list()
              for(i in 1:length(training_fold[,fac])){
                w[[i]] <- ifelse(classes[1] == training_fold[i,fac],  nu_u_c[1], nu_u_c[2])
              }
              weights <- do.call(rbind, w)
            } else{
              weights <- NULL
            } 
          }
          formula <- as.formula(paste0(names(logitreg_df)[fac], " ~ ."))
          logitregmodel <- glm(formula = formula,
                               data = training_fold,
                               family = "binomial",
                               weights = weights[,1],
                               na.action = na.exclude)
          y_pred <- predict(logitregmodel, newdata = logitreg_df_prediction, type = "response")
          y_pred <- ifelse(y_pred > input$logitregprob, 1, 0)
          return(y_pred)
        })
        Mode <- function(x) {
          ux <- unique(x)
          return(ux[which.max(tabulate(match(x, ux)))])
        }
        folds_predicted <- data.frame(Mode(cv)[[1]])
        names(folds_predicted) <- response
        folds_predicted
      } else{
        ### weights
        userclass <- input$logitregweights
        if(userclass == ""){
          weights <- NULL
        } else{
          nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
          nu_u_c <- na.omit(nu_u_c)
          classes <- levels(factor(training_set[,fac]))
          if(length(nu_u_c) == length(classes)){
            w <- list()
            for(i in 1:length(training_set[,fac])){
              w[[i]] <- ifelse(classes[1] == training_set[i,fac],  nu_u_c[1], nu_u_c[2])
            }
            weights <- do.call(rbind, w)
          } else{
            weights <- NULL
          } 
        }
        formula <- as.formula(paste0(names(logitreg_df)[fac], " ~ ."))
        logitregmodel <- glm(formula = formula,
                             data = training_set,
                             family = "binomial",
                             weights = weights[,1],
                             na.action = na.exclude)
        y_pred <- predict(logitregmodel, newdata = logitreg_df_prediction, type = "response")
        y_pred <- ifelse(y_pred > input$logitregprob, 1, 0)
        y_pred <- round(data.frame(y_pred),3)
        colnames(y_pred) <- response
        y_pred
      }
    }
  })
  
  ############ table of actual value for new data
  output$logitreg_actual_response <- renderDT({
    if(!is.null(input$logitreg_prediction_response) && input$logitregprepross_prediction_result) {
      uploaded_logitreg_data_prediction <- isolate(uploaded_logitreg_data_prediction())
      response_prediction <- input$logitreg_prediction_response
      logitreg_df_prediction <- data.frame(uploaded_logitreg_data_prediction[, response_prediction])
      logitreg_df_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 10),
          rownames = T)
    }
  })
  ############ table of predicted value for new data
  output$logitreg_predicted_response <- renderDT({
    req(logitreg_analysis_prediction())
    if(input$logitregprepross_prediction_result) {
      logitreg_analysis_prediction <- isolate(logitreg_analysis_prediction())
      logitreg_analysis_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 10),
          rownames = T)
    }
  })
  
  ### Download logitreg prediction table
  output$download_prediction_logitreg_table <- renderUI({
    if(input$logitregprepross_prediction_result) {
      downloadButton('logitregprediction_table', 'Download prediction table')
    }
  })
  
  output$logitregprediction_table <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_prediction_table.csv")
    },
    content = function(file){
      logitreg_analysis_prediction() %>%
        write_csv(file)
    }
  )
  ##################################### Random forest (classification -regression) ##############################################
  # error checking
  output$uploaded_randomforestdata_hide_tabpanel <- reactive({
    error_catghing_randomforest <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_randomforest)
      checknull <- NCOL(uploaded_randomforest_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_randomforest(input$uploaded_randomforest$name, input$datarandomforestfileformat)
  })
  outputOptions(output, 'uploaded_randomforestdata_hide_tabpanel', suspendWhenHidden=FALSE)
  ###############
  uploaded_randomforest_data <- eventReactive(input$uploaded_randomforest, {
    if(!is.null(input$uploaded_randomforest)){
      
      if(input$datarandomforestfileformat == "," && input$randomforestheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_randomforest$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$datarandomforestfileformat == "," && input$randomforestheader == "No") {
        rawdata <- try(read_csv(input$uploaded_randomforest$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datarandomforestfileformat == "\t" && input$randomforestheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_randomforest$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datarandomforestfileformat == "\t" && input$randomforestheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_randomforest$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  output$randomforestdatashow <- renderDT({
    
    req(input$uploaded_randomforest)
    if(input$randomforestprepross) {
      randomforest_table <- uploaded_randomforest_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          editable= T,
          style = "auto",
          rownames = FALSE)
      randomforest_table
    }
  })
  # response and predictor choices
  observeEvent(uploaded_randomforest_data(), {
    if(!is.null(uploaded_randomforest_data())){
      response_var <- names(uploaded_randomforest_data())
      updateSelectInput(
        session,
        "randomforestresponse",
        choices = response_var)
    }
  })
  # numeric
  observeEvent(uploaded_randomforest_data(), {
    if(!is.null(uploaded_randomforest_data())){
      dftype <- sapply(uploaded_randomforest_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_vars <- names(uploaded_randomforest_data())[isnumeric]
      updatePickerInput(
        session,
        "randomforestpredictors_numeric",
        choices = predictor_vars)
    }
  })
  # categorical
  observeEvent(uploaded_randomforest_data(), {
    if(!is.null(uploaded_randomforest_data())){
      dftype <- sapply(uploaded_randomforest_data(), class)
      ischar <- grep("character|logical", dftype)
      predictor_vars <- names(uploaded_randomforest_data())[ischar]
      updatePickerInput(
        session,
        "randomforestpredictors_categorical",
        choices = predictor_vars)
    }
  })
  
  ################################################ random forest parameters hidin
  output$hide_randomforest_classification <- reactive({
    hiderandomforestification <- function(){
      if(input$randomforesttype == "Classification"){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    hiderandomforestification()
  })
  outputOptions(output, 'hide_randomforest_classification', suspendWhenHidden=FALSE)
  #################################### reactive of variables
  randomforest_reactive <- reactive({
    if(!is.null(uploaded_randomforest_data())){
      uploaded_randomforest_data <- isolate(uploaded_randomforest_data())
      response <- input$randomforestresponse
      predictors_numeric <- input$randomforestpredictors_numeric
      predictors_categorical <- input$randomforestpredictors_categorical
      res_name <- which(names(uploaded_randomforest_data) %in% response)
      predic_num_name <- which(names(uploaded_randomforest_data) %in% predictors_numeric)
      predic_cat_name <- which(names(uploaded_randomforest_data) %in% predictors_categorical)
      if(length(res_name) > 0 | length(predic_num_name) > 0 | length(predic_cat_name) > 0 ){
        randomforest_df <- data.frame(uploaded_randomforest_data[, c(predictors_numeric, predictors_categorical, response)])        
      } else{
        return(NULL)
      }    
    }
  })
  ######################################## mtry number ##################################
  # classification
  observeEvent(randomforest_reactive(),{
    if(!is.null(randomforest_reactive())){
    df_length <- length(randomforest_reactive()) -1
    valuemtry <- round(sqrt(df_length))
    minmtry <- valuemtry
    maxmtry <- df_length
    updateNumericInput(session = session,
                       inputId = "randomforestmtryclass",
                       value = valuemtry,
                       min = valuemtry,
                       max = maxmtry, step = 1
    )
    }
  })
  # regression
  observeEvent(randomforest_reactive(),{
    if(!is.null(randomforest_reactive())){
      df_length <- length(randomforest_reactive()) -1
        valuemtry <- round(df_length/3)
        if(valuemtry > 0){
        minmtry <- valuemtry
        maxmtry <- df_length
      } else{
        valuemtry <- 1
        maxmtry <- 1
      }
      updateNumericInput(session = session,
                         inputId = "randomforestmtryreg",
                         value = valuemtry,
                         min = valuemtry,
                         max = maxmtry, step = 1
      )
    }
  })
  ################################### choice for strata
  observeEvent(randomforest_reactive(), {
    if(!is.null(randomforest_reactive())){
      dftype <- sapply(randomforest_reactive(), class)
      ischar <- grep("character|logical", dftype)
      strat <- names(randomforest_reactive())[ischar]
      updatePickerInput(
        session,
        "randomforeststrata",
        choices = strat)
    }
  })
  ################################# sample size number
  observeEvent(randomforest_reactive(),{
    if(!is.null(randomforest_reactive())){
      df_NROW <- NROW(randomforest_reactive()) 
      updateNumericInput(session = session,
                         inputId = "randomforestsampsize",
                         value = round(.3*df_NROW),
                         min = 1,
                         max = df_NROW, step = 1
      )
    }
  })
  ################ randomforest analysis 
  randomforest_analysis <- reactive({
    if(!is.null(uploaded_randomforest_data()) && input$randomforestaction){
      if(is.null(input$randomforestresponse) | is.null(input$randomforestpredictors_categorical) && is.null(input$randomforestpredictors_numeric)){
        return(NULL)
      }
      uploaded_randomforest_data <- isolate(uploaded_randomforest_data())
      response <- input$randomforestresponse
      predictors_numeric <- input$randomforestpredictors_numeric
      predictors_categorical <- input$randomforestpredictors_categorical
      res_name <- which(names(uploaded_randomforest_data) %in% response)
      predic_num_name <- which(names(uploaded_randomforest_data) %in% predictors_numeric)
      predic_cat_name <- which(names(uploaded_randomforest_data) %in% predictors_categorical)
      if(length(res_name) > 0 | length(predic_num_name) > 0 | length(predic_cat_name) > 0 ){
        randomforest_df <- data.frame(uploaded_randomforest_data[, c(predictors_numeric, predictors_categorical, response)])
        fac <- length(randomforest_df)
      } else{
        return(NULL)
      }
      ############################################## Classification ##############################

 if(input$randomforesttype == "Classification"){
   randomforest_df[,fac] <- factor(randomforest_df[,fac])
   cat_variable <- which(names(randomforest_df) %in% predictors_categorical)
   if(length(cat_variable) > 0){
     for(i in predictors_categorical){
       randomforest_df[, i] <- factor(randomforest_df[, i])
     }
   }
   set.seed(123)
   split <- sample.split(randomforest_df[, fac], SplitRatio = input$randomforestratiosplit)
   training_set <- subset(randomforest_df, split == TRUE)
   test_set <- subset(randomforest_df, split == FALSE)
   factors <- which(names(randomforest_df) %in% predictors_categorical)
  if(length(factors) > 0){
    training_set[c(-fac, -factors)] <- scale(training_set[c(-fac, -factors)])
    test_set[c(-fac, -factors)] <- scale(test_set[c(-fac, -factors)])
  }
   else{
    training_set[-fac] <- scale(training_set[-fac])
    test_set[-fac] <- scale(test_set[-fac])
  }
   ########## class weights
   userclass <- input$randomforestclasswt
   nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
   nu_u_c <- na.omit(nu_u_c)
   classwt <- table(randomforest_df[,fac])
   if(length(nu_u_c) == length(classwt)){
     for(i in 1:length(nu_u_c)){
       classwt[i] <- nu_u_c[i]
     }

   } else{
     classwt <- NULL

   }
   ############## cutoff
   usercutoff <- input$randomforestcutoff
   pro_u_cu <- as.numeric(unlist(str_split(usercutoff, ",")))
   pro_u_cu <- na.omit(pro_u_cu)
   cutoff <- table(randomforest_df[,fac])
   if(length(pro_u_cu) == length(cutoff)){
     for(i in 1:length(pro_u_cu)){
       cutoff[i] <- pro_u_cu[i]
     }
   } else{
     class <- length(levels(factor(randomforest_df[,fac])))
     for(i in 1:class){
       cutoff[i] <- 1/class
     }
   }
   ###########################
if(input$randomforestkfoldcv > 0) {
  folds <- createFolds(randomforest_df[,fac] , k = input$randomforestkfoldcv)

  if(input$randomforestreplace == "FALSE"){
    replace <- FALSE
  }else{
    replace <- TRUE
  }
  if(input$randomforestproximity == "FALSE"){
    proximity <- FALSE
  } else {
    proximity <- TRUE
  }
  if(input$randomforestoob.prox == "FALSE"){
    oob.prox <- FALSE
  } else {
    oob.prox <- TRUE
  }
  if(input$randomforestcorr.bias == "FALSE"){
    corr.bias <- FALSE
  } else {
    corr.bias <- TRUE
  }
  cv <- lapply(folds, function(x){
    training_fold <- training_set[-x, ]
    test_fold <- test_set[-x, ]
    classifier <- randomForest(x = training_fold[-fac],
                               y = training_fold[, fac],
                               ntree = input$randomforestntree,
                               mtry = input$randomforestmtryclass,
                               replace = replace,
                               classwt = classwt,
                               cutoff = cutoff,
                               strata = input$randomforeststrata,
                               sampsize = input$randomforestsampsize,
                               nodesize = input$randomforestnodesizeclass,
                               maxnodes = input$randomforestmaxnodes,
                               importance = FALSE,
                               localImp = FALSE,
                               nPerm = input$randomforestnPerm,
                               proximity = proximity,
                               oob.prox = oob.prox,
                               norm.votes = FALSE,
                               do.trace = FALSE,
                               keep.forest = TRUE,
                               corr.bias = corr.bias,
                               keep.inbag = FALSE,
                               na.action = na.omit)
    y_pred <- predict(classifier, newdata = test_fold[-fac])
    result <- confusion_matrix(test_fold[, fac], y_pred)
    return(result)
  })
  multiclass_con_av(cv)
} else{

  if(input$randomforestreplace == "FALSE"){
    replace <- FALSE
  }else{
    replace <- TRUE
  }
  if(input$randomforestproximity == "FALSE"){
    proximity <- FALSE
  } else {
    proximity <- TRUE
  }
  if(input$randomforestoob.prox == "FALSE"){
    oob.prox <- FALSE
  } else {
    oob.prox <- TRUE
  }
  if(input$randomforestcorr.bias == "FALSE"){
    corr.bias <- FALSE
  } else {
    corr.bias <- TRUE
  }
  classifier <- randomForest(x = training_set[-fac],
                             y = training_set[, fac],
                             ntree = input$randomforestntree,
                             mtry = input$randomforestmtryclass,
                             replace = replace,
                             classwt = classwt,
                             cutoff = cutoff,
                             strata = input$randomforeststrata,
                             sampsize = input$randomforestsampsize,
                             nodesize = input$randomforestnodesizeclass,
                             maxnodes = input$randomforestmaxnodes,
                             importance = FALSE,
                             localImp = FALSE,
                             nPerm = input$randomforestnPerm,
                             proximity = proximity,
                             oob.prox = oob.prox,
                             norm.votes = FALSE,
                             do.trace = FALSE,
                             keep.forest = TRUE,
                             corr.bias = corr.bias,
                             keep.inbag = FALSE,
                             na.action = na.omit)
                       y_pred <- predict(classifier, newdata = test_set[-fac])
                     confusion_matrix(test_set[, fac], y_pred)
             }
             }
         else{
        ######################################################## Regression ############################
        if(length(predic_cat_name) > 0){
          for(i in predictors_categorical){
            randomforest_df[, i] <- factor(randomforest_df[, i])
          }
        }
        set.seed(123)
        split <- sample.split(randomforest_df[, fac], SplitRatio = input$randomforestratiosplit)
        training_set <- subset(randomforest_df, split == TRUE)
        test_set <- subset(randomforest_df, split == FALSE)
        if(input$randomforestkfoldcv > 0) {
          ############################# parameters
          if(input$randomforestreplace == "FALSE"){
            replace <- FALSE
          }else{
            replace <- TRUE
          }
          if(input$randomforestproximity == "FALSE"){
            proximity <- FALSE
          } else {
            proximity <- TRUE
          }
          if(input$randomforestoob.prox == "FALSE"){
            oob.prox <- FALSE
          } else {
            oob.prox <- TRUE
          }
          if(input$randomforestcorr.bias == "FALSE"){
            corr.bias <- FALSE
          } else {
            corr.bias <- TRUE
          }
          folds <- createFolds(randomforest_df[,fac] , k = input$randomforestkfoldcv)
          cv <- lapply(folds, function(x){
            training_fold <- training_set[-x, ]
            test_fold <- test_set[-x, ]
            classifier <- randomForest(x = training_fold[-fac],
                                       y = training_fold[, fac],
                                       ntree = input$randomforestntree,
                                       mtry = input$randomforestmtryreg,
                                       replace = replace,
                                       strata = input$randomforeststrata,
                                       sampsize = input$randomforestsampsize,
                                       nodesize = input$randomforestnodesizereg,
                                       maxnodes = input$randomforestmaxnodes,
                                       importance = FALSE,
                                       localImp = FALSE,
                                       nPerm = input$randomforestnPerm,
                                       proximity = proximity,
                                       oob.prox = oob.prox,
                                       norm.votes = FALSE,
                                       do.trace = FALSE,
                                       keep.forest = TRUE,
                                       corr.bias = corr.bias,
                                       keep.inbag = FALSE,
                                       na.action = na.omit)
            y_pred <- predict(classifier, newdata = test_fold[-fac])
            mse <- MLmetrics::MSE(y_pred, test_fold[,fac])
            mae <- MLmetrics::MAE(y_pred, test_fold[,fac])
            rmse <- MLmetrics::RMSE(y_pred, test_fold[,fac])
            r2 <- MLmetrics::R2_Score(y_pred, test_fold[,fac])
            accuracy <- data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2)
            return(accuracy)
          })
          Accuracy <- round(colMeans(do.call(rbind, cv)), 3)
          Accuracy <- data.frame(t(Accuracy))
        } else{
          ############################# parameters
          if(input$randomforestreplace == "FALSE"){
            replace <- FALSE
          }else{
            replace <- TRUE
          }
          if(input$randomforestproximity == "FALSE"){
            proximity <- FALSE
          } else {
            proximity <- TRUE
          }
          if(input$randomforestoob.prox == "FALSE"){
            oob.prox <- FALSE
          } else {
            oob.prox <- TRUE
          }
          if(input$randomforestcorr.bias == "FALSE"){
            corr.bias <- FALSE
          } else {
            corr.bias <- TRUE
          }
          classifier <- randomForest(x  = training_set[-fac],
                                     y = training_set[, fac],
                                     ntree = input$randomforestntree,
                                     mtry = input$randomforestmtryreg,
                                     replace = replace,
                                     strata = input$randomforeststrata,
                                     sampsize = input$randomforestsampsize,
                                     nodesize = input$randomforestnodesizereg,
                                     maxnodes = input$randomforestmaxnodes,
                                     importance = FALSE,
                                     localImp = FALSE,
                                     nPerm = input$randomforestnPerm,
                                     proximity = proximity,
                                     oob.prox = oob.prox,
                                     norm.votes = FALSE,
                                     do.trace = FALSE,
                                     keep.forest = TRUE,
                                     corr.bias = corr.bias,
                                     keep.inbag = FALSE,
                                     na.action = na.omit)
          y_pred <- predict(classifier, newdata = test_set[-fac])
          mse <- MLmetrics::MSE(y_pred, test_set[,fac])
          mae <- MLmetrics::MAE(y_pred, test_set[,fac])
          rmse <- MLmetrics::RMSE(y_pred, test_set[,fac])
          r2 <- MLmetrics::R2_Score(y_pred, test_set[,fac])
          accuracy <- round(data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2), 3)
          accuracy
      }
      }
    }
  })
  ######################## randomforest result
  output$randomforestoutput <- renderDT({
    if(!is.null(randomforest_analysis()) && input$randomforestaction) {
      randomforest_analysis <- isolate(randomforest_analysis())
      randomforest_output <- randomforest_analysis %>%
        datatable(
          options = list(scrollX = TRUE, dom = "t"),
          rownames = TRUE)
      randomforest_output
    }
  })
  ########################################## random forest plot (classification) ##################################
  # ############################ choice x (classification)
  observeEvent(randomforest_reactive(),{
    if(!is.null(randomforest_reactive())){
      res_fac <- length(randomforest_reactive())
      dftype <- sapply(randomforest_reactive(), class)
      isnumeric <- grep("numeric", dftype)
      ischar <- grep("character|logical", dftype[res_fac])
      if(length(ischar) == 1){
        randomforestplotchoicex <- names(randomforest_reactive())[isnumeric]
      }
      else if(length(ischar) == 0){
        randomforestplotchoicex <- names(randomforest_reactive())[isnumeric]
        len <- length(randomforestplotchoicex)
        randomforestplotchoicex <- randomforestplotchoicex[-len]
      }
      updateSelectInput(
        session,
        "randomforestplotx",
        choices = randomforestplotchoicex)
    }
  })
  ############################ choice y
  observeEvent(randomforest_reactive(),{
    if(!is.null(randomforest_reactive())){
      res_fac <- length(randomforest_reactive())
      dftype <- sapply(randomforest_reactive(), class)
      isnumeric <- grep("numeric", dftype)
      ischar <- grep("character|logical", dftype[res_fac])
      if(length(ischar) == 1){
        randomforestplotchoicey <- names(randomforest_reactive())[isnumeric]
        len <- length(randomforestplotchoicey)
        randomforestplotchoicey <- randomforestplotchoicey[len:1]
      }
      else if(length(ischar) == 0){
        randomforestplotchoicey <- names(randomforest_reactive())[isnumeric]
        len <- length(randomforestplotchoicey)
        randomforestplotchoicey <- randomforestplotchoicey[len:1][-1]
      }
      updateSelectInput(
        session,
        "randomforestploty",
        choices = randomforestplotchoicey)
    }
  })
  
  #################### hide choices for PCA
  output$hide_randomforest_choices_pca <- reactive({
    if(!is.null(randomforest_reactive())){
      hiderandomforestpca <- function(){
        df_len <- length(randomforest_reactive())
        dftype <- sapply(randomforest_reactive(), class)
        isnumeric <- grep("numeric", dftype[-df_len])
        if(length(isnumeric) >= 3){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
      hiderandomforestpca()
    }
  })
  outputOptions(output, 'hide_randomforest_choices_pca', suspendWhenHidden=FALSE)
  
  
  ########################## choice x regression
  observeEvent(randomforest_reactive(),{
    if(!is.null(randomforest_reactive())){
      dftype <- sapply(randomforest_reactive(), class)
      isnumeric <- grep("numeric", dftype)
      ischar <- grep("character|logical", dftype[1])
      if(length(ischar) == 1){
        randomforestplotchoicex <- names(randomforest_reactive())[isnumeric]
        randomforestplotchoicex <- randomforestplotchoicex
      }
      else if(length(ischar) == 0){
        randomforestplotchoicex <- names(randomforest_reactive())[isnumeric]
        len <- length(randomforestplotchoicex)
        randomforestplotchoicex <- randomforestplotchoicex[-len]
      }
      
      updateSelectInput(
        session,
        "randomforestregplotx",
        choices = randomforestplotchoicex)
    }
  })
  
  ################################################ random forest training plot- classification ##################
  output$randomforestplottraining <- renderPlot({
    if(!is.null(uploaded_randomforest_data()) && input$randomforestplotview){
      if(is.null(input$randomforestresponse) | is.null(input$randomforestpredictors_categorical) && is.null(input$randomforestpredictors_numeric)){
        return(NULL)
      }
      uploaded_randomforest_data <- isolate(uploaded_randomforest_data())
      response <- input$randomforestresponse
      predictors_numeric <- input$randomforestpredictors_numeric
      predictors_categorical <- input$randomforestpredictors_categorical
      res_name <- which(names(uploaded_randomforest_data) %in% response)
      predic_num_name <- which(names(uploaded_randomforest_data) %in% predictors_numeric)
      predic_cat_name <- which(names(uploaded_randomforest_data) %in% predictors_categorical)
      if(length(res_name) > 0 | length(predic_num_name) > 0 | length(predic_cat_name) > 0 ){
        randomforest_df <- data.frame(uploaded_randomforest_data[, c(predictors_numeric, predictors_categorical, response)])         
        fac <- length(randomforest_df)
      } else{
        return(NULL)
      }
      ############################################## Classification ##############################
      
      if(input$randomforesttype == "Classification"){
        randomforest_df[,fac] <- factor(randomforest_df[,fac])
        if(length(predic_cat_name) > 0){
          for(i in predictors_categorical){
            randomforest_df[, i] <- factor(randomforest_df[, i])
          }
        }
        set.seed(123)
        split <- sample.split(randomforest_df[, fac], SplitRatio = input$randomforestratiosplit)
        training_set <- subset(randomforest_df, split == TRUE)
        test_set <- subset(randomforest_df, split == FALSE)
        factors <- which(names(randomforest_df) %in% predictors_categorical)
        if(length(factors) > 0){
          training_set[c(-fac, -factors)] <- scale(training_set[c(-fac, -factors)])
          test_set[c(-fac, -factors)] <- scale(test_set[c(-fac, -factors)])
        }
        else{
          training_set[-fac] <- scale(training_set[-fac])
          test_set[-fac] <- scale(test_set[-fac])
        }
        ########## class weights
        userclass <- input$randomforestclasswt
        nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
        nu_u_c <- na.omit(nu_u_c)
        classwt <- table(randomforest_df[,fac])
        if(length(nu_u_c) == length(classwt)){
          for(i in 1:length(nu_u_c)){
            classwt[i] <- nu_u_c[i]
          }
          
        } else{
          classwt <- NULL
          
        }
        ############## cutoff
        usercutoff <- input$randomforestcutoff
        pro_u_cu <- as.numeric(unlist(str_split(usercutoff, ",")))
        pro_u_cu <- na.omit(pro_u_cu)
        cutoff <- table(randomforest_df[,fac])
        if(length(pro_u_cu) == length(cutoff)){
          for(i in 1:length(pro_u_cu)){
            cutoff[i] <- pro_u_cu[i]
          }
        } else{
          class <- length(levels(factor(randomforest_df[,fac])))
          for(i in 1:class){
            cutoff[i] <- 1/class
          }
        }
          if(input$randomforestreplace == "FALSE"){
            replace <- FALSE
          }else{
            replace <- TRUE
          }
          if(input$randomforestproximity == "FALSE"){
            proximity <- FALSE
          } else {
            proximity <- TRUE
          }
          if(input$randomforestoob.prox == "FALSE"){
            oob.prox <- FALSE
          } else {
            oob.prox <- TRUE
          }
          if(input$randomforestcorr.bias == "FALSE"){
            corr.bias <- FALSE
          } else {
            corr.bias <- TRUE
          }
          numeric_only <- length(randomforest_df) - length(factors)
          if(numeric_only == 3 && !is.null(input$randomforestplotx) && !is.null(input$randomforestploty)){
            par(mar=c(4,4,5,6))
            set <- training_set
            X1 <- seq(min(set[, input$randomforestplotx]) - 1, max(set[, input$randomforestplotx]) + 1, by = 0.05)
            X2 <- seq(min(set[, input$randomforestploty]) - 1, max(set[, input$randomforestploty]) + 1, by = 0.05)
            grid_set <- expand.grid(X1, X2)
            colnames(grid_set) = c(input$randomforestplotx, input$randomforestploty)
            classifier <- randomForest(x = training_set[, c(-fac, -factors)],
                                       y = training_set[, fac],
                                       ntree = input$randomforestntree,
                                       mtry = input$randomforestmtryclass,
                                       replace = replace,
                                       classwt = classwt,
                                       cutoff = cutoff,
                                       strata = input$randomforeststrata,
                                       sampsize = input$randomforestsampsize,
                                       nodesize = input$randomforestnodesizeclass,
                                       maxnodes = input$randomforestmaxnodes,
                                       importance = FALSE,
                                       localImp = FALSE,
                                       nPerm = input$randomforestnPerm,
                                       proximity = proximity,
                                       oob.prox = oob.prox,
                                       norm.votes = FALSE,
                                       do.trace = FALSE,
                                       keep.forest = TRUE,
                                       corr.bias = corr.bias,
                                       keep.inbag = FALSE,
                                       na.action = na.omit)
            y_pred <- predict(classifier, newdata = grid_set)
            qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
            col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
            col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
            len_1 <- length(levels(factor(y_pred)))
            len_2 <- length(levels(factor(set[,fac])))
            level_1 <- levels(factor(y_pred))
            level_2 <- levels(factor(set[,fac]))
            col_1 <- col_vector[1:len_1]
            col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
            color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
            color_2 <- as.character(factor(set[,fac], levels = level_2, labels = col_2))
            p <- plot(set[, c(input$randomforestplotx, input$randomforestploty)],
                      main = sprintf('Random forest (Training set) - trees = %d',input$randomforestntree),
                      xlab = input$randomforestplotx, ylab = input$randomforestploty,
                      xlim = range(X1), ylim = range(X2))
            contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
            points(grid_set, pch = '.', col = color_1)
            points(set, pch = 21, bg = color_2)
            legend(x = "topright",
                   inset = c(-.09, .4),
                   title = "Classes",
                   legend = level_2, 
                   col = col_2,
                   bg = rgb(1, 0, 0, alpha = 0.15),
                   box.lty = 0,
                   pch = 19,
                   xpd = TRUE)
            p
          }
          else if(numeric_only > 3){
            par(mar=c(4,4,5,6))
            pred_train <- training_set[fac]
            pred_test <- test_set[fac]
            pca <- princomp(x = training_set[-fac],  cor = TRUE, scores = TRUE, covmat = NULL)
            varpca <- pca$sdev^2 / sum(pca$sdev^2)
            training_set <- predict(pca, training_set)
            training_set <- data.frame(training_set[, 1:2])
            training_set <- cbind(training_set , pred_train)
            colnames(training_set) <- c("PC1", "PC2", "response")
            training_set$response <- factor(training_set$response)
            test_set <- predict(pca, test_set)
            test_set <- data.frame(test_set[, 1:2])
            test_set <- cbind(test_set, pred_test)
            colnames(test_set) <- c("PC1", "PC2", "response")
            test_set$response <- factor(test_set$response)
            set <- training_set
            X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
            X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.05)
            grid_set <- expand.grid(X1, X2)
            colnames(grid_set) <- c("PC1", "PC2")
            classifier <- randomForest(x = training_set[-3],
                                       y = training_set[, 3],
                                       ntree = input$randomforestntree,
                                       mtry = input$randomforestmtryclass,
                                       replace = replace,
                                       classwt = classwt,
                                       cutoff = cutoff,
                                       strata = input$randomforeststrata,
                                       sampsize = input$randomforestsampsize,
                                       nodesize = input$randomforestnodesizeclass,
                                       maxnodes = input$randomforestmaxnodes,
                                       importance = FALSE,
                                       localImp = FALSE,
                                       nPerm = input$randomforestnPerm,
                                       proximity = proximity,
                                       oob.prox = oob.prox,
                                       norm.votes = FALSE,
                                       do.trace = FALSE,
                                       keep.forest = TRUE,
                                       corr.bias = corr.bias,
                                       keep.inbag = FALSE,
                                       na.action = na.omit)
            y_pred <- predict(classifier, newdata = grid_set)
            qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
            col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
            col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
            len_1 <- length(levels(factor(y_pred)))
            len_2 <- length(levels(factor(set[,3])))
            level_1 <- levels(factor(y_pred))
            level_2 <- levels(factor(set[,3]))
            col_1 <- col_vector[1:len_1]
            col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
            color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
            color_2 <- as.character(factor(set[,3], levels = level_2, labels = col_2))
            p <- plot(set[, -3],
                      main = sprintf('Random forest (Training set) - trees = %d',input$randomforestntree),
                      xlab = sprintf("PC1 (%2.2f%%)", varpca[1]*100), ylab = sprintf("PC2 (%2.2f%%)", varpca[2]*100),
                      xlim = range(X1), ylim = range(X2))
            contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
            points(grid_set, pch = '.', col = color_1)
            points(set, pch = 21, bg = color_2)
            legend(x = "topright",
                   inset = c(-.09, .4),
                   title = "Classes",
                   legend = level_2, 
                   col = col_2,
                   bg = rgb(1, 0, 0, alpha = 0.15),
                   box.lty = 0,
                   pch = 19,
                   xpd = TRUE)
            p
          }
        }
    }
  })  
  ############################################ random forest test plot (classification) ###############
  output$randomforestplottest <- renderPlot({
    if(!is.null(uploaded_randomforest_data()) && input$randomforestplotview){
      if(is.null(input$randomforestresponse) | is.null(input$randomforestpredictors_categorical) && is.null(input$randomforestpredictors_numeric)){
        return(NULL)
      }
      uploaded_randomforest_data <- isolate(uploaded_randomforest_data())
      response <- input$randomforestresponse
      predictors_numeric <- input$randomforestpredictors_numeric
      predictors_categorical <- input$randomforestpredictors_categorical
      res_name <- which(names(uploaded_randomforest_data) %in% response)
      predic_num_name <- which(names(uploaded_randomforest_data) %in% predictors_numeric)
      predic_cat_name <- which(names(uploaded_randomforest_data) %in% predictors_categorical)
      if(length(res_name) > 0 | length(predic_num_name) > 0 | length(predic_cat_name) > 0 ){
        randomforest_df <- data.frame(uploaded_randomforest_data[, c(predictors_numeric, predictors_categorical, response)])         
        fac <- length(randomforest_df)
      } else{
        return(NULL)
      }
      ############################################## Classification ##############################
      
      if(input$randomforesttype == "Classification"){
        randomforest_df[,fac] <- factor(randomforest_df[,fac])
        if(length(predic_cat_name) > 0){
          for(i in predictors_categorical){
            randomforest_df[, i] <- factor(randomforest_df[, i])
          }
        }
        set.seed(123)
        split <- sample.split(randomforest_df[, fac], SplitRatio = input$randomforestratiosplit)
        training_set <- subset(randomforest_df, split == TRUE)
        test_set <- subset(randomforest_df, split == FALSE)
        factors <- which(names(randomforest_df) %in% predictors_categorical)
        if(length(factors) > 0){
          training_set[c(-fac, -factors)] <- scale(training_set[c(-fac, -factors)])
          test_set[c(-fac, -factors)] <- scale(test_set[c(-fac, -factors)])
        }
        else{
          training_set[-fac] <- scale(training_set[-fac])
          test_set[-fac] <- scale(test_set[-fac])
        }
        ########## class weights
        userclass <- input$randomforestclasswt
        nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
        nu_u_c <- na.omit(nu_u_c)
        classwt <- table(randomforest_df[,fac])
        if(length(nu_u_c) == length(classwt)){
          for(i in 1:length(nu_u_c)){
            classwt[i] <- nu_u_c[i]
          }
          
        } else{
          classwt <- NULL
          
        }
        ############## cutoff
        usercutoff <- input$randomforestcutoff
        pro_u_cu <- as.numeric(unlist(str_split(usercutoff, ",")))
        pro_u_cu <- na.omit(pro_u_cu)
        cutoff <- table(randomforest_df[,fac])
        if(length(pro_u_cu) == length(cutoff)){
          for(i in 1:length(pro_u_cu)){
            cutoff[i] <- pro_u_cu[i]
          }
        } else{
          class <- length(levels(factor(randomforest_df[,fac])))
          for(i in 1:class){
            cutoff[i] <- 1/class
          }
        }
          if(input$randomforestreplace == "FALSE"){
            replace <- FALSE
          }else{
            replace <- TRUE
          }
          if(input$randomforestproximity == "FALSE"){
            proximity <- FALSE
          } else {
            proximity <- TRUE
          }
          if(input$randomforestoob.prox == "FALSE"){
            oob.prox <- FALSE
          } else {
            oob.prox <- TRUE
          }
          if(input$randomforestcorr.bias == "FALSE"){
            corr.bias <- FALSE
          } else {
            corr.bias <- TRUE
          }
          numeric_only <- length(randomforest_df) - length(factors)
          if(numeric_only == 3 && !is.null(input$randomforestplotx) && !is.null(input$randomforestploty)){
            par(mar=c(4,4,5,6))
            set <- test_set
            X1 <- seq(min(set[, input$randomforestplotx]) - 1, max(set[, input$randomforestplotx]) + 1, by = 0.05)
            X2 <- seq(min(set[, input$randomforestploty]) - 1, max(set[, input$randomforestploty]) + 1, by = 0.05)
            grid_set <- expand.grid(X1, X2)
            colnames(grid_set) = c(input$randomforestplotx, input$randomforestploty)
            classifier <- randomForest(x = training_set[, c(-fac, -factors)],
                                       y = training_set[, fac],
                                       ntree = input$randomforestntree,
                                       mtry = input$randomforestmtryclass,
                                       replace = replace,
                                       classwt = classwt,
                                       cutoff = cutoff,
                                       strata = input$randomforeststrata,
                                       sampsize = input$randomforestsampsize,
                                       nodesize = input$randomforestnodesizeclass,
                                       maxnodes = input$randomforestmaxnodes,
                                       importance = FALSE,
                                       localImp = FALSE,
                                       nPerm = input$randomforestnPerm,
                                       proximity = proximity,
                                       oob.prox = oob.prox,
                                       norm.votes = FALSE,
                                       do.trace = FALSE,
                                       keep.forest = TRUE,
                                       corr.bias = corr.bias,
                                       keep.inbag = FALSE,
                                       na.action = na.omit)
            y_pred <- predict(classifier, newdata = grid_set)
            qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
            col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
            col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
            len_1 <- length(levels(factor(y_pred)))
            len_2 <- length(levels(factor(set[,fac])))
            level_1 <- levels(factor(y_pred))
            level_2 <- levels(factor(set[,fac]))
            col_1 <- col_vector[1:len_1]
            col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
            color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
            color_2 <- as.character(factor(set[,fac], levels = level_2, labels = col_2))
            p <- plot(set[, c(input$randomforestplotx, input$randomforestploty)],
                      main = sprintf('Random forest (Test set) - trees = %d',input$randomforestntree),
                      xlab = input$randomforestplotx, ylab = input$randomforestploty,
                      xlim = range(X1), ylim = range(X2))
            contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
            points(grid_set, pch = '.', col = color_1)
            points(set, pch = 21, bg = color_2)
            legend(x = "topright",
                   inset = c(-.09, .4),
                   title = "Classes",
                   legend = level_2, 
                   col = col_2,
                   bg = rgb(1, 0, 0, alpha = 0.15),
                   box.lty = 0,
                   pch = 19,
                   xpd = TRUE)
            p
          }
          else if(numeric_only > 3){
            par(mar=c(4,4,5,6))
            pred_train <- training_set[fac]
            pred_test <- test_set[fac]
            pca <- princomp(x = training_set[-fac],  cor = TRUE, scores = TRUE, covmat = NULL)
            varpca <- pca$sdev^2 / sum(pca$sdev^2)
            training_set <- predict(pca, training_set)
            training_set <- data.frame(training_set[, 1:2])
            training_set <- cbind(training_set , pred_train)
            colnames(training_set) <- c("PC1", "PC2", "response")
            training_set$response <- factor(training_set$response)
            test_set <- predict(pca, test_set)
            test_set <- data.frame(test_set[, 1:2])
            test_set <- cbind(test_set, pred_test)
            colnames(test_set) <- c("PC1", "PC2", "response")
            test_set$response <- factor(test_set$response)
            set <- test_set
            X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
            X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.05)
            grid_set <- expand.grid(X1, X2)
            colnames(grid_set) <- c("PC1", "PC2")
            classifier <- randomForest(x = training_set[-3],
                                       y = training_set[, 3],
                                       ntree = input$randomforestntree,
                                       mtry = input$randomforestmtryclass,
                                       replace = replace,
                                       classwt = classwt,
                                       cutoff = cutoff,
                                       strata = input$randomforeststrata,
                                       sampsize = input$randomforestsampsize,
                                       nodesize = input$randomforestnodesizeclass,
                                       maxnodes = input$randomforestmaxnodes,
                                       importance = FALSE,
                                       localImp = FALSE,
                                       nPerm = input$randomforestnPerm,
                                       proximity = proximity,
                                       oob.prox = oob.prox,
                                       norm.votes = FALSE,
                                       do.trace = FALSE,
                                       keep.forest = TRUE,
                                       corr.bias = corr.bias,
                                       keep.inbag = FALSE,
                                       na.action = na.omit)
            y_pred <- predict(classifier, newdata = grid_set)
            qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
            col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
            col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
            len_1 <- length(levels(factor(y_pred)))
            len_2 <- length(levels(factor(set[,3])))
            level_1 <- levels(factor(y_pred))
            level_2 <- levels(factor(set[,3]))
            col_1 <- col_vector[1:len_1]
            col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
            color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
            color_2 <- as.character(factor(set[,3], levels = level_2, labels = col_2))
            p <- plot(set[, -3],
                      main = sprintf('Random forest (Test set) - trees = %d',input$randomforestntree),
                      xlab = sprintf("PC1 (%2.2f%%)", varpca[1]*100), ylab = sprintf("PC2 (%2.2f%%)", varpca[2]*100),
                      xlim = range(X1), ylim = range(X2))
            contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
            points(grid_set, pch = '.', col = color_1)
            points(set, pch = 21, bg = color_2)
            legend(x = "topright",
                   inset = c(-.09, .4),
                   title = "Classes",
                   legend = level_2, 
                   col = col_2,
                   bg = rgb(1, 0, 0, alpha = 0.15),
                   box.lty = 0,
                   pch = 19,
                   xpd = TRUE)
            p
          }
        }
      }
  }) 
  ###################################### display waiting massage for random forest plot ############
  randomforestplot_waiting <- reactiveValues(ok = FALSE)
  observeEvent(input$randomforestplotview, {
    if(input$randomforestplotview == 1 && !is.null(randomforest_analysis())){
      shinyjs::show("randomforestplotwait")
      randomforestplot_waiting$ok <- FALSE
      Sys.sleep(5)
      randomforestplot_waiting$ok <- TRUE
      shinyjs::hide("randomforestplotwait")
    }
  })
  ####################################### random forest regression plot ##########################
  output$randomforestplotrgression <- renderPlot({
    if(!is.null(uploaded_randomforest_data()) && input$randomforestregplotview){
      if(is.null(input$randomforestresponse) | is.null(input$randomforestpredictors_categorical) && is.null(input$randomforestpredictors_numeric)){
        return(NULL)
      }
      if(input$randomforesttype != "Classification"){
        uploaded_randomforest_data <- isolate(uploaded_randomforest_data())
        response <- input$randomforestresponse
        predictors_numeric <- input$randomforestpredictors_numeric
        predictors_categorical <- input$randomforestpredictors_categorical
        res_name <- which(names(uploaded_randomforest_data) %in% response)
        predic_num_name <- which(names(uploaded_randomforest_data) %in% predictors_numeric)
        predic_cat_name <- which(names(uploaded_randomforest_data) %in% predictors_categorical)
        if(length(res_name) > 0 | length(predic_num_name) > 0 | length(predic_cat_name) > 0 ){
          randomforest_df <- data.frame(uploaded_randomforest_data[, c(predictors_numeric, predictors_categorical, response)])
          fac <- length(randomforest_df)
        } else{
          return(NULL)
        }
        if(length(predic_cat_name) > 0){
          for(i in predictors_categorical){
            randomforest_df[, i] <- factor(randomforest_df[, i])
          }
        }
        set.seed(123)
        split <- sample.split(randomforest_df[, fac], SplitRatio = input$randomforestratiosplit)
        training_set <- subset(randomforest_df, split == TRUE)
        test_set <- subset(randomforest_df, split == FALSE)
          ############################# parameters
          if(input$randomforestreplace == "FALSE"){
            replace <- FALSE
          }else{
            replace <- TRUE
          }
          if(input$randomforestproximity == "FALSE"){
            proximity <- FALSE
          } else {
            proximity <- TRUE
          }
          if(input$randomforestoob.prox == "FALSE"){
            oob.prox <- FALSE
          } else {
            oob.prox <- TRUE
          }
          if(input$randomforestcorr.bias == "FALSE"){
            corr.bias <- FALSE
          } else {
            corr.bias <- TRUE
          }
          regressor <- randomForest(x  = training_set[-fac],
                                     y = training_set[, fac],
                                     ntree = input$randomforestntree,
                                     mtry = input$randomforestmtryreg,
                                     replace = replace,
                                     strata = input$randomforeststrata,
                                     sampsize = input$randomforestsampsize,
                                     nodesize = input$randomforestnodesizereg,
                                     maxnodes = input$randomforestmaxnodes,
                                     importance = FALSE,
                                     localImp = FALSE,
                                     nPerm = input$randomforestnPerm,
                                     proximity = proximity,
                                     oob.prox = oob.prox,
                                     norm.votes = FALSE,
                                     do.trace = FALSE,
                                     keep.forest = TRUE,
                                     corr.bias = corr.bias,
                                     keep.inbag = FALSE,
                                     na.action = na.omit)
          y_pred <- predict(regressor, newdata = test_set[-fac])
          x <- data.frame(test_set[input$randomforestregplotx])
          p <- ggplot(aes(x = x[,1], y= test_set[,fac]), data = test_set) + geom_point(colour = '#145A32') +
            geom_line(aes(x = x[,1], y = y_pred),colour = '#B7950B') +
            xlab(input$randomforestregplotx) + ylab(response) + 
            theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                    axis.text = element_text(colour = "black"),
                                    axis.text.y = element_text(colour = "black")) +
            theme(axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                  axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                  axis.text.y = element_text(family = "Times",colour = "black"),
                  plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                  axis.title.y = element_text(family = "Times", size = rel(1.4)),
                  axis.title.x = element_text(family = "Times", size = rel(1.4)))+
            labs(subtitle = sprintf('Random forest - trees = %d',input$randomforestntree))
          p
        }
    }
  })
  ############################################## random forest prediction on new data ##############################
  #### error checking
  output$uploaded_randomforestdata_prediction_hide_tabpanel <- reactive({
    error_catghing_randomforest_prediction <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_randomforest_prediction)
      checknull <- NCOL(uploaded_randomforest_data_prediction()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_randomforest_prediction(input$uploaded_randomforest_prediction$name, input$datarandomforestfileformat_prediction)
  })
  outputOptions(output, 'uploaded_randomforestdata_prediction_hide_tabpanel', suspendWhenHidden=FALSE)
  ############### uploaded prediction table
  uploaded_randomforest_data_prediction <- eventReactive(input$uploaded_randomforest_prediction,{
    if(!is.null(input$uploaded_randomforest_prediction)){
      if(input$datarandomforestfileformat == "," && input$randomforestheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_randomforest_prediction$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if(input$datarandomforestfileformat == "," && input$randomforestheader == "No") {
        rawdata <- try(read_csv(input$uploaded_randomforest_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datarandomforestfileformat == "\t" && input$randomforestheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_randomforest$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datarandomforestfileformat == "\t" && input$randomforestheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_randomforest_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ############### new table for prediction choices
  # response and predictor choices
  observeEvent(uploaded_randomforest_data_prediction(), {
    if(!is.null(uploaded_randomforest_data_prediction())){
      response_var <- names(uploaded_randomforest_data_prediction())
      updateSelectInput(
        session,
        "randomforest_prediction_response",
        choices = response_var)
    }
  })
  # numeric
  observeEvent(uploaded_randomforest_data_prediction(), {
    if(!is.null(uploaded_randomforest_data_prediction())){
      dftype <- sapply(uploaded_randomforest_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_vars <- names(uploaded_randomforest_data_prediction())[isnumeric]
      updatePickerInput(
        session,
        "randomforestpredictors_numeric_prediction",
        choices = predictor_vars)
    }
  })
  # categorical
  observeEvent(uploaded_randomforest_data_prediction(), {
    if(!is.null(uploaded_randomforest_data_prediction())){
      dftype <- sapply(uploaded_randomforest_data_prediction(), class)
      ischar <- grep("character|logical", dftype)
      predictor_vars <- names(uploaded_randomforest_data_prediction())[ischar]
      updatePickerInput(
        session,
        "randomforestpredictors_categorical_prediction",
        choices = predictor_vars)
    }
  })
  ######################################## random forest prediction result ###################################
  randomforest_analysis_prediction <- reactive({
    if(!is.null(uploaded_randomforest_data()) && input$randomforestprepross_prediction_result){
      if(is.null(input$randomforestresponse) | is.null(input$randomforestpredictors_categorical) && is.null(input$randomforestpredictors_numeric)){
        return(NULL)
      }
      ############ for randomforest prediction
      uploaded_randomforest_data_prediction <- isolate(uploaded_randomforest_data_prediction())
      predictors_cat_prediction <- input$randomforestpredictors_categorical_prediction
      predictors_num_prediction <- input$randomforestpredictors_numeric_prediction
      num_predict <- which(names(uploaded_randomforest_data_prediction) %in% predictors_num_prediction)
      cat_predict <- which(names(uploaded_randomforest_data_prediction) %in% predictors_cat_prediction)
      if(length(num_predict) > 0 | length(cat_predict) > 0){
        randomforest_df_prediction <- data.frame(uploaded_randomforest_data_prediction[, c(predictors_num_prediction, predictors_cat_prediction)])
        randomforest_df_prediction
      } else{
        return(NULL)
      }
      ########################################## random forest analysis 
      uploaded_randomforest_data <- isolate(uploaded_randomforest_data())
      response <- input$randomforestresponse
      predictors_numeric <- input$randomforestpredictors_numeric
      predictors_categorical <- input$randomforestpredictors_categorical
      res_name <- which(names(uploaded_randomforest_data) %in% response)
      predic_num_name <- which(names(uploaded_randomforest_data) %in% predictors_numeric)
      predic_cat_name <- which(names(uploaded_randomforest_data) %in% predictors_categorical)
      if(length(res_name) > 0 | length(predic_num_name) > 0 | length(predic_cat_name) > 0 ){
        randomforest_df <- data.frame(uploaded_randomforest_data[, c(predictors_numeric, predictors_categorical, response)])
        fac <- length(randomforest_df)
      } else{
        return(NULL)
      }
      ############## check both file to be almost the same ##########################
      similar_name <- which(names(randomforest_df) %in% names(randomforest_df_prediction))
      if(length(similar_name) < 1){
        return(NULL)
      }
      ############################################## Classification ##############################
      
      if(input$randomforesttype == "Classification"){
        randomforest_df[,fac] <- factor(randomforest_df[,fac])
        # categorical to factor - analysis
        if(length(predic_cat_name) > 0){
          for(i in predictors_categorical){
            randomforest_df[, i] <- factor(randomforest_df[, i])
          }
        }
        
        # categorical to factor - prediction
        if(length(cat_predict) > 0){
          for(i in predictors_cat_prediction){
            randomforest_df_prediction[, i] <- factor(randomforest_df_prediction[, i])
          }
        }
        ############ for random forest analysis
        set.seed(123)
        split <- sample.split(randomforest_df[, fac], SplitRatio = input$randomforestratiosplit)
        training_set <- subset(randomforest_df, split == TRUE)
        test_set <- subset(randomforest_df, split == FALSE)
        factors <- which(names(randomforest_df) %in% predictors_categorical)
        if(length(factors) > 0){
          training_set[c(-fac, -factors)] <- scale(training_set[c(-fac, -factors)])
          test_set[c(-fac, -factors)] <- scale(test_set[c(-fac, -factors)])
        }
        else{
          training_set[-fac] <- scale(training_set[-fac])
          test_set[-fac] <- scale(test_set[-fac])
        }
        ############ for random forest prediction  
        factors_prediction <- which(names(randomforest_df_prediction) %in% predictors_cat_prediction)
        if(length(factors_prediction) > 0){
          randomforest_df_prediction[-factors_prediction] <- scale(randomforest_df_prediction[-factors_prediction])
          randomforest_df_prediction <- data.frame(randomforest_df_prediction)
        }
        else{
          randomforest_df_prediction <- scale(randomforest_df_prediction)
          randomforest_df_prediction <- data.frame(randomforest_df_prediction)
        }
        ########## class weights
        userclass <- input$randomforestclasswt
        nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
        nu_u_c <- na.omit(nu_u_c)
        classwt <- table(randomforest_df[,fac])
        if(length(nu_u_c) == length(classwt)){
          for(i in 1:length(nu_u_c)){
            classwt[i] <- nu_u_c[i]
          }
          
        } else{
          classwt <- NULL
          
        }
        ############## cutoff
        usercutoff <- input$randomforestcutoff
        pro_u_cu <- as.numeric(unlist(str_split(usercutoff, ",")))
        pro_u_cu <- na.omit(pro_u_cu)
        cutoff <- table(randomforest_df[,fac])
        if(length(pro_u_cu) == length(cutoff)){
          for(i in 1:length(pro_u_cu)){
            cutoff[i] <- pro_u_cu[i]
          }
        } else{
          class <- length(levels(factor(randomforest_df[,fac])))
          for(i in 1:class){
            cutoff[i] <- 1/class
          }
        }
        ###########################
        if(input$randomforestkfoldcv > 0) {
          folds <- createFolds(randomforest_df[,fac] , k = input$randomforestkfoldcv)
          
          if(input$randomforestreplace == "FALSE"){
            replace <- FALSE
          }else{
            replace <- TRUE
          }
          if(input$randomforestproximity == "FALSE"){
            proximity <- FALSE
          } else {
            proximity <- TRUE
          }
          if(input$randomforestoob.prox == "FALSE"){
            oob.prox <- FALSE
          } else {
            oob.prox <- TRUE
          }
          if(input$randomforestcorr.bias == "FALSE"){
            corr.bias <- FALSE
          } else {
            corr.bias <- TRUE
          }
          cv <- lapply(folds, function(x){
            training_fold <- training_set[-x, ]
            test_fold <- test_set[-x, ]
            classifier <- randomForest(x = training_fold[, -fac],
                                       y = training_fold[, fac],
                                       ntree = input$randomforestntree,
                                       mtry = input$randomforestmtryclass,
                                       replace = replace,
                                       classwt = classwt,
                                       cutoff = cutoff,
                                       strata = input$randomforeststrata,
                                       sampsize = input$randomforestsampsize,
                                       nodesize = input$randomforestnodesizeclass,
                                       maxnodes = input$randomforestmaxnodes,
                                       importance = FALSE,
                                       localImp = FALSE,
                                       nPerm = input$randomforestnPerm,
                                       proximity = proximity,
                                       oob.prox = oob.prox,
                                       norm.votes = FALSE,
                                       do.trace = FALSE,
                                       keep.forest = TRUE,
                                       corr.bias = corr.bias,
                                       keep.inbag = FALSE,
                                       na.action = na.omit)
            y_pred <- predict(classifier, newdata = randomforest_df_prediction)
            return(y_pred)
          })
          cv
          Mode <- function(x) {
            ux <- unique(x)
            return(ux[which.max(tabulate(match(x, ux)))])
          }
          folds_predicted <- data.frame(Mode(cv)[[1]])
          names(folds_predicted) <- response
          folds_predicted
        } else{
          
          if(input$randomforestreplace == "FALSE"){
            replace <- FALSE
          }else{
            replace <- TRUE
          }
          if(input$randomforestproximity == "FALSE"){
            proximity <- FALSE
          } else {
            proximity <- TRUE
          }
          if(input$randomforestoob.prox == "FALSE"){
            oob.prox <- FALSE
          } else {
            oob.prox <- TRUE
          }
          if(input$randomforestcorr.bias == "FALSE"){
            corr.bias <- FALSE
          } else {
            corr.bias <- TRUE
          }
          classifier <- randomForest(x = training_set[, -fac],
                                     y = training_set[, fac],
                                     ntree = input$randomforestntree,
                                     mtry = input$randomforestmtryclass,
                                     replace = replace,
                                     classwt = classwt,
                                     cutoff = cutoff,
                                     strata = input$randomforeststrata,
                                     sampsize = input$randomforestsampsize,
                                     nodesize = input$randomforestnodesizeclass,
                                     maxnodes = input$randomforestmaxnodes,
                                     importance = FALSE,
                                     localImp = FALSE,
                                     nPerm = input$randomforestnPerm,
                                     proximity = proximity,
                                     oob.prox = oob.prox,
                                     norm.votes = FALSE,
                                     do.trace = FALSE,
                                     keep.forest = TRUE,
                                     corr.bias = corr.bias,
                                     keep.inbag = FALSE,
                                     na.action = na.omit)
          y_pred <- predict(classifier, newdata = randomforest_df_prediction)
          y_pred <- data.frame(y_pred)
          colnames(y_pred) <- response
          y_pred
        }
      }
      else{
        ######################################################## Regression ############################
        if(length(predic_cat_name) > 0){
          for(i in predictors_categorical){
            randomforest_df[, i] <- factor(randomforest_df[, i])
          }
        }
        # categorical to factor - prediction
        if(length(cat_predict) > 0){
          for(i in predictors_cat_prediction){
            randomforest_df_prediction[, i] <- factor(randomforest_df_prediction[, i])
          }
        }
        
        set.seed(123)
        split <- sample.split(randomforest_df[, fac], SplitRatio = input$randomforestratiosplit)
        training_set <- subset(randomforest_df, split == TRUE)
        test_set <- subset(randomforest_df, split == FALSE)
        if(input$randomforestkfoldcv > 0) {
          ############################# parameters
          if(input$randomforestreplace == "FALSE"){
            replace <- FALSE
          }else{
            replace <- TRUE
          }
          if(input$randomforestproximity == "FALSE"){
            proximity <- FALSE
          } else {
            proximity <- TRUE
          }
          if(input$randomforestoob.prox == "FALSE"){
            oob.prox <- FALSE
          } else {
            oob.prox <- TRUE
          }
          if(input$randomforestcorr.bias == "FALSE"){
            corr.bias <- FALSE
          } else {
            corr.bias <- TRUE
          }
          folds <- createFolds(randomforest_df[,fac] , k = input$randomforestkfoldcv)
          cv <- lapply(folds, function(x){
            training_fold <- training_set[-x, ]
            test_fold <- test_set[-x, ]
            regressor <- randomForest(x = training_fold[-fac],
                                      y = training_fold[, fac],
                                      ntree = input$randomforestntree,
                                      mtry = input$randomforestmtryreg,
                                      replace = replace,
                                      strata = input$randomforeststrata,
                                      sampsize = input$randomforestsampsize,
                                      nodesize = input$randomforestnodesizereg,
                                      maxnodes = input$randomforestmaxnodes,
                                      importance = FALSE,
                                      localImp = FALSE,
                                      nPerm = input$randomforestnPerm,
                                      proximity = proximity,
                                      oob.prox = oob.prox,
                                      norm.votes = FALSE,
                                      do.trace = FALSE,
                                      keep.forest = TRUE,
                                      corr.bias = corr.bias,
                                      keep.inbag = FALSE,
                                      na.action = na.omit)
            y_pred <- predict(regressor, newdata = randomforest_df_prediction)
            return(y_pred)
          })
          cv
          Prediction <- round(colMeans(do.call(rbind, cv)), 3)
          Prediction <- data.frame(Prediction)
          colnames(Prediction) <- response
          Prediction
        } else{
          ############################# parameters
          if(input$randomforestreplace == "FALSE"){
            replace <- FALSE
          }else{
            replace <- TRUE
          }
          if(input$randomforestproximity == "FALSE"){
            proximity <- FALSE
          } else {
            proximity <- TRUE
          }
          if(input$randomforestoob.prox == "FALSE"){
            oob.prox <- FALSE
          } else {
            oob.prox <- TRUE
          }
          if(input$randomforestcorr.bias == "FALSE"){
            corr.bias <- FALSE
          } else {
            corr.bias <- TRUE
          }
          regressor <- randomForest(x  = training_set[-fac],
                                    y = training_set[, fac],
                                    ntree = input$randomforestntree,
                                    mtry = input$randomforestmtryreg,
                                    replace = replace,
                                    strata = input$randomforeststrata,
                                    sampsize = input$randomforestsampsize,
                                    nodesize = input$randomforestnodesizereg,
                                    maxnodes = input$randomforestmaxnodes,
                                    importance = FALSE,
                                    localImp = FALSE,
                                    nPerm = input$randomforestnPerm,
                                    proximity = proximity,
                                    oob.prox = oob.prox,
                                    norm.votes = FALSE,
                                    do.trace = FALSE,
                                    keep.forest = TRUE,
                                    corr.bias = corr.bias,
                                    keep.inbag = FALSE,
                                    na.action = na.omit)
          y_pred <- predict(regressor, newdata = randomforest_df_prediction)
          y_pred <- round(data.frame(y_pred),3)
          colnames(y_pred) <- response
          y_pred
        }
      }
    }
  })
  ############ table of actual value for new data
  output$randomforest_actual_response <- renderDT({
    if(!is.null(input$randomforest_prediction_response) && input$randomforestprepross_prediction_result) {
      uploaded_randomforest_data_prediction <- isolate(uploaded_randomforest_data_prediction())
      response_prediction <- input$randomforest_prediction_response
      randomforest_df_prediction <- data.frame(uploaded_randomforest_data_prediction[, response_prediction])
      randomforest_df_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 9),
          rownames = T)
    }
  })
  ############ table of predicted value for new data
  output$randomforest_predicted_response <- renderDT({
    req(randomforest_analysis_prediction())
    if(input$randomforestprepross_prediction_result) {
      randomforest_analysis_prediction <- isolate(randomforest_analysis_prediction())
      randomforest_analysis_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 9),
          rownames = T)
    }
  })
  
  ### Download randomforest prediction table
  output$download_prediction_randomforest_table <- renderUI({
    if(input$randomforestprepross_prediction_result) {
      downloadButton('randomforestprediction_table', 'Download prediction table')
    }
  })
  
  output$randomforestprediction_table <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_prediction_table.csv")
    },
    content = function(file){
      randomforest_analysis_prediction() %>%
        write_csv(file)
    }
  )
  ########################################## KNN class ##############################################
  # error checking
  output$uploaded_knnclassdata_hide_tabpanel <- reactive({
    error_catghing_knnclass <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_knnclass)
      checknull <- NCOL(uploaded_knnclass_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_knnclass(input$uploaded_knnclass$name, input$dataknnclassfileformat)
  })
  outputOptions(output, 'uploaded_knnclassdata_hide_tabpanel', suspendWhenHidden=FALSE)
  ###############
  uploaded_knnclass_data <- eventReactive(input$uploaded_knnclass, {
    if(!is.null(input$uploaded_knnclass)){
      
      if(input$dataknnclassfileformat == "," && input$knnclassheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_knnclass$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$dataknnclassfileformat == "," && input$knnclassheader == "No") {
        rawdata <- try(read_csv(input$uploaded_knnclass$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$dataknnclassfileformat == "\t" && input$knnclassheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_knnclass$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$dataknnclassfileformat == "\t" && input$knnclassheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_knnclass$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  output$knnclassdatashow <- renderDT({
    
    req(input$uploaded_knnclass)
    if(input$knnclassprepross) {
      knnclass_table <- uploaded_knnclass_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          editable= T,
          style = "auto",
          rownames = FALSE)
      knnclass_table
    }
  })
  # response and predictor choices
  observeEvent(uploaded_knnclass_data(), {
    if(!is.null(uploaded_knnclass_data())){
      response_var <- names(uploaded_knnclass_data())
      updateSelectInput(
        session,
        "knnclassresponse",
        choices = response_var)
    }
  })
  # numeric
  observeEvent(uploaded_knnclass_data(), {
    if(!is.null(uploaded_knnclass_data())){
      dftype <- sapply(uploaded_knnclass_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_vars <- names(uploaded_knnclass_data())[isnumeric]
      updatePickerInput(
        session,
        "knnclasspredictors",
        choices = predictor_vars)
    }
  })
  
  ################ knnclass analysis 
  knnclass_analysis <- reactive({
    if(!is.null(uploaded_knnclass_data()) && input$knnclassaction){
      if(is.null(input$knnclassresponse) | is.null(input$knnclasspredictors)){
        return(NULL)
      }
      uploaded_knnclass_data <- isolate(uploaded_knnclass_data())
      response <- input$knnclassresponse
      predictors <- input$knnclasspredictors
      res_name <- which(names(uploaded_knnclass_data) %in% response)
      predi_name <- which(names(uploaded_knnclass_data) %in% predictors)
      if(length(res_name) > 0 | length(predi_name) > 0 ){
        knnclass_df <- data.frame(uploaded_knnclass_data[, c(predictors, response)])
        fac <- length(knnclass_df)
        knnclass_df[,fac] <- factor(knnclass_df[,fac])
        l <- length(levels(knnclass_df[,fac]))
        if(l > 10){
          return(NULL)
        }
      } else{
        return(NULL)
      }
      set.seed(123)
      split <- sample.split(knnclass_df[, fac], SplitRatio = input$knnclassratiosplit)
      training_set <- subset(knnclass_df, split == TRUE)
      test_set <- subset(knnclass_df, split == FALSE)
      training_set[-fac] <- scale(training_set[-fac])
      test_set[-fac] <- scale(test_set[-fac])
      if(input$knnclasskfoldcv > 0) {
         folds <- createFolds(knnclass_df[,fac] , k = input$knnclasskfoldcv)
        if(input$knnclassprob == "FALSE"){
          prob <- FALSE
        }else{
          prob <- TRUE
        }
        if(input$knnclassuseall == "FALSE"){
          use.all <- FALSE
        } else {
          use.all <- TRUE
        }
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          knnclass_pred <- class::knn(train = training_fold[, -fac],
                                      test = test_fold[, -fac],
                                      cl = training_fold[, fac],
                                      k = input$knnclassk,
                                      l = input$knnclassl,
                                      prob = prob,
                                      use.all = use.all)
          result <- confusion_matrix(test_fold[, fac], knnclass_pred)
          return(result)
        })
        multiclass_con_av(cv)
      } else{
        if(input$knnclassprob == "FALSE"){
          prob <- FALSE
        }else{
          prob <- TRUE
        }
        if(input$knnclassuseall == "FALSE"){
          use.all <- FALSE
        } else {
          use.all <- TRUE
        }
        knnclass_pred <- class::knn(train = training_set[, -fac],
                                    test = test_set[, -fac],
                                    cl = training_set[, fac],
                                    k = input$knnclassk,
                                    l = input$knnclassl,
                                    prob = prob,
                                    use.all = use.all)
        confusion_matrix(test_set[, fac], knnclass_pred)
      }
    }
    
  })
  ######################## knnclass 
  output$knnclassoutput <- renderDT({
    if(!is.null(knnclass_analysis()) && input$knnclassaction) {
      knnclass_analysis <- isolate(knnclass_analysis())
      knnclass_output <- knnclass_analysis %>%
        datatable(
          options = list(scrollX = TRUE, dom = "t"),
          rownames = TRUE)
      knnclass_output
    }
  })
  ########################################## knn plot ##################################
  ###################### knn plot choices
  knnclassplotchoices <- reactive({
    if(!is.null(uploaded_knnclass_data()) && input$knnclassaction){
      if(is.null(input$knnclassresponse) | is.null(knnclass_analysis()) && is.null(input$knnclasspredictors)){
        return(NULL)
      }
      uploaded_knnclass_data <- isolate(uploaded_knnclass_data())
      predictors <- input$knnclasspredictors
      pre_name <- which(names(uploaded_knnclass_data) %in% predictors)
      if(length(pre_name) > 0){
        knnclass_df <- data.frame(uploaded_knnclass_data[, predictors])
        knnclass_df
      } else{
        return(NULL)
      }
    }
  })

  # ############################ choice x
  observeEvent(knnclassplotchoices(),{
    if(!is.null(knnclassplotchoices())){
      knnclassplotchoicex <- names(knnclassplotchoices())
      updateSelectInput(
        session,
        "knnclassplotx",
        choices = knnclassplotchoicex)
    }
  })
  ############################ choice y
  observeEvent(knnclassplotchoices(),{
    if(!is.null(knnclassplotchoices())){
      len <- length(knnclassplotchoices())
      knnclassplotchoicex <- names(knnclassplotchoices())[len:1]
      updateSelectInput(
        session,
        "knnclassploty",
        choices = knnclassplotchoicex)
    }
  })
  
  #################### hide choices for PCA
  output$hide_knnclass_choices_pca <- reactive({
    if(!is.null(knnclassplotchoices())){
      hideknncalss <- function(){
        df_len <- length(knnclassplotchoices())
        dftype <- sapply(knnclassplotchoices(), class)
        isnumeric <- grep("numeric", dftype[-df_len])
        if(length(isnumeric) >= 2){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
      hideknncalss()
    }
  })
  outputOptions(output, 'hide_knnclass_choices_pca', suspendWhenHidden=FALSE)
###################################### knn class training plot 
  output$knnclassplottraining <- renderPlot({
    if(input$knnclassplotview){
      if(is.null(input$knnclassresponse) | is.null(input$knnclasspredictors)){
        return(NULL)
      }
      uploaded_knnclass_data <- isolate(uploaded_knnclass_data())
      response <- input$knnclassresponse
      predictors <- input$knnclasspredictors
      res_name <- which(names(uploaded_knnclass_data) %in% response)
      predinum_name <- which(names(uploaded_knnclass_data) %in% predictors)
      if(length(res_name) > 0 | length(predinum_name) > 0 ){
        knnclass_df <- data.frame(uploaded_knnclass_data[, c(predictors, response)])
        fac <- length(knnclass_df)
        knnclass_df[,fac] <- factor(knnclass_df[,fac])
        l <- length(levels(knnclass_df[,fac]))
        if(l > 10){
          return(NULL)
        }
      } else{
        return(NULL)
      }
      set.seed(123)
      split <- sample.split(knnclass_df[,fac], SplitRatio = input$knnclassratiosplit)
      training_set <- subset(knnclass_df, split == TRUE)
      test_set <- subset(knnclass_df, split == FALSE)
      training_set[-fac] <- scale(training_set[-fac])
      test_set[-fac] <- scale(test_set[-fac])
        if(input$knnclassprob == "FALSE"){
          prob <- FALSE
        }else{
          prob <- TRUE
        }
        if(input$knnclassuseall == "FALSE"){
          use.all <- FALSE
        } else {
          use.all <- TRUE
        }
        if(length(knnclass_df) == 3){
          par(mar=c(4,4,5,6))
          set <- training_set
          X1 <- seq(min(set[, input$knnclassplotx]) - 1, max(set[, input$knnclassplotx]) + 1, by = 0.05)
          X2 <- seq(min(set[, input$knnclassploty]) - 1, max(set[, input$knnclassploty]) + 1, by = 0.05)
          grid_set <- expand.grid(X1, X2)
          colnames(grid_set) <- c(input$knnclassplotx, input$knnclassploty)
          knnclass_pred <- class::knn(train = training_set[, c(input$knnclassplotx, input$knnclassploty)],
                                      test = grid_set,
                                      cl = training_set[, fac],
                                      k = input$knnclassk,
                                      l = input$knnclassl,
                                      prob = prob,
                                      use.all = use.all)
          qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
          col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
          col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
          len_1 <- length(levels(factor(knnclass_pred)))
          len_2 <- length(levels(factor(set[,fac])))
          level_1 <- levels(factor(knnclass_pred))
          level_2 <- levels(factor(set[,fac]))
          col_1 <- col_vector[1:len_1]
          col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
          color_1 <- as.character(factor(knnclass_pred, levels = level_1, labels = col_1))
          color_2 <- as.character(factor(set[,fac], levels = level_2, labels = col_2))
          p <- plot(set[, c(input$knnclassplotx, input$knnclassploty)],
                    main = sprintf('K-NN (Training set) - k = %d',input$knnclassk),
                    xlab = input$knnclassplotx, ylab = input$knnclassploty,
                    xlim = range(X1), ylim = range(X2))
          contour(X1, X2, matrix(as.numeric(knnclass_pred), length(X1), length(X2)), add = TRUE)
          points(grid_set, pch = '.', col = color_1)
          points(set, pch = 21, bg = color_2)
          legend(x = "topright",
                 inset = c(-.09, .4),
                 title = "Classes",
                 legend = level_2, 
                 col = col_2,
                 bg = rgb(1, 0, 0, alpha = 0.15),
                 box.lty = 0,
                 pch = 19,
                 xpd = TRUE)
        }
        else if(length(knnclass_df) > 3 ){
          par(mar=c(4,4,5,6))
          pred_train <- training_set[fac]
          pred_test <- test_set[fac]
          pca <- princomp(x = training_set[-fac],  cor = TRUE, scores = TRUE, covmat = NULL)
          varpca <- pca$sdev^2 / sum(pca$sdev^2)
          training_set <- predict(pca, training_set)
          training_set <- data.frame(training_set[, 1:2])
          training_set <- cbind(training_set , pred_train)
          colnames(training_set) <- c("PC1", "PC2", "response")
          training_set$response <- factor(training_set$response)
          test_set <- predict(pca, test_set)
          test_set <- data.frame(test_set[, 1:2])
          test_set <- cbind(test_set, pred_test)
          colnames(test_set) <- c("PC1", "PC2", "response")
          test_set$response <- factor(test_set$response)
          set <- training_set
          X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
          X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.05)
          grid_set <- expand.grid(X1, X2)
          colnames(grid_set) <- c("PC1", "PC2")
          #print(grid_set)
          knnclass_pred <- class::knn(train = training_set[-3],
                                      test = grid_set,
                                      cl = training_set[, 3],
                                      k = input$knnclassk,
                                      l = input$knnclassl,
                                      prob = prob,
                                      use.all = use.all)
          qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
          col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
          col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
          len_1 <- length(levels(factor(knnclass_pred)))
          len_2 <- length(levels(factor(set[,3])))
          level_1 <- levels(factor(knnclass_pred))
          level_2 <- levels(factor(set[,3]))
          col_1 <- col_vector[1:len_1]
          col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
          color_1 <- as.character(factor(knnclass_pred, levels = level_1, labels = col_1))
          color_2 <- as.character(factor(set[,3], levels = level_2, labels = col_2))
          p <- plot(set[, -3],
                    main = sprintf('K-NN (Training set) - k = %d',input$knnclassk),
                    xlab = sprintf("PC1 (%2.2f%%)", varpca[1]*100), ylab = sprintf("PC2 (%2.2f%%)", varpca[2]*100),
                    xlim = range(X1), ylim = range(X2))
          contour(X1, X2, matrix(as.numeric(knnclass_pred), length(X1), length(X2)), add = TRUE)
          points(grid_set, pch = '.', col = color_1)
          points(set, pch = 21, bg = color_2)
          legend(x = "topright",
                 inset = c(-.09, .4),
                 title = "Classes",
                 legend = level_2,
                 col = col_2,
                 bg = rgb(1, 0, 0, alpha = 0.15),
                 box.lty = 0,
                 pch = 19,
                 xpd = TRUE)
        }
      }
  })
  
  ###################################### knn class test plot 
  output$knnclassplottest <- renderPlot({
    if(!is.null(uploaded_knnclass_data()) && !is.null(knnclass_analysis()) && input$knnclassplotview){
      if(is.null(input$knnclassresponse) | is.null(input$knnclasspredictors)){
        return(NULL)
      }
      uploaded_knnclass_data <- isolate(uploaded_knnclass_data())
      response <- input$knnclassresponse
      predictors <- input$knnclasspredictors
      res_name <- which(names(uploaded_knnclass_data) %in% response)
      predinum_name <- which(names(uploaded_knnclass_data) %in% predictors)
      if(length(res_name) > 0 | length(predinum_name) > 0 ){
        knnclass_df <- data.frame(uploaded_knnclass_data[, c(predictors, response)])
        fac <- length(knnclass_df)
        knnclass_df[,fac] <- factor(knnclass_df[,fac])
        l <- length(levels(knnclass_df[,fac]))
        if(l > 10){
          return(NULL)
        }
      } else{
        return(NULL)
      }
      set.seed(123)
      split <- sample.split(knnclass_df[,fac], SplitRatio = input$knnclassratiosplit)
      training_set <- subset(knnclass_df, split == TRUE)
      test_set <- subset(knnclass_df, split == FALSE)
      training_set[-fac] <- scale(training_set[-fac])
      test_set[-fac] <- scale(test_set[-fac])
        if(input$knnclassprob == "FALSE"){
          prob <- FALSE
        }else{
          prob <- TRUE
        }
        if(input$knnclassuseall == "FALSE"){
          use.all <- FALSE
        } else {
          use.all <- TRUE
        }
        if(length(knnclass_df) == 3){
          par(mar=c(4,4,5,6))
          set <- test_set
          X1 <- seq(min(set[, input$knnclassplotx]) - 1, max(set[, input$knnclassplotx]) + 1, by = 0.05)
          X2 <- seq(min(set[, input$knnclassploty]) - 1, max(set[, input$knnclassploty]) + 1, by = 0.05)
          grid_set <- expand.grid(X1, X2)
          colnames(grid_set) <- c(input$knnclassplotx, input$knnclassploty)
          knnclass_pred <- class::knn(train = training_set[, c(input$knnclassplotx, input$knnclassploty)],
                                      test = grid_set,
                                      cl = training_set[, fac],
                                      k = input$knnclassk,
                                      l = input$knnclassl,
                                      prob = prob,
                                      use.all = use.all)
          qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
          col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
          col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
          len_1 <- length(levels(factor(knnclass_pred)))
          len_2 <- length(levels(factor(set[,fac])))
          level_1 <- levels(factor(knnclass_pred))
          level_2 <- levels(factor(set[,fac]))
          col_1 <- col_vector[1:len_1]
          col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
          color_1 <- as.character(factor(knnclass_pred, levels = level_1, labels = col_1))
          color_2 <- as.character(factor(set[,fac], levels = level_2, labels = col_2))
          p <- plot(set[, c(input$knnclassplotx, input$knnclassploty)],
                    main = sprintf('K-NN (Test set) - k = %d',input$knnclassk),
                    xlab = input$knnclassplotx, ylab = input$knnclassploty,
                    xlim = range(X1), ylim = range(X2))
          contour(X1, X2, matrix(as.numeric(knnclass_pred), length(X1), length(X2)), add = TRUE)
          points(grid_set, pch = '.', col = color_1)
          points(set, pch = 21, bg = color_2)
          legend(x = "topright",
                 inset = c(-.09, .4),
                 title = "Classes",
                 legend = level_2, 
                 col = col_2,
                 bg = rgb(1, 0, 0, alpha = 0.15),
                 box.lty = 0,
                 pch = 19,
                 xpd = TRUE)
        }
        else if(length(knnclass_df) > 3 ){
          par(mar=c(4,4,5,6))
          pred_train <- training_set[fac]
          pred_test <- test_set[fac]
          pca <- princomp(x = training_set[-fac],  cor = TRUE, scores = TRUE, covmat = NULL)
          varpca <- pca$sdev^2 / sum(pca$sdev^2)
          training_set <- predict(pca, training_set)
          training_set <- data.frame(training_set[, 1:2])
          training_set <- cbind(training_set , pred_train)
          colnames(training_set) <- c("PC1", "PC2", "response")
          training_set$response <- factor(training_set$response)
          test_set <- predict(pca, test_set)
          test_set <- data.frame(test_set[, 1:2])
          test_set <- cbind(test_set, pred_test)
          colnames(test_set) <- c("PC1", "PC2", "response")
          test_set$response <- factor(test_set$response)
          set <- test_set
          X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
          X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.05)
          grid_set <- expand.grid(X1, X2)
          colnames(grid_set) <- c("PC1", "PC2")
          #print(grid_set)
          knnclass_pred <- class::knn(train = training_set[-3],
                                      test = grid_set,
                                      cl = training_set[, 3],
                                      k = input$knnclassk,
                                      l = input$knnclassl,
                                      prob = prob,
                                      use.all = use.all)
          qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
          col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
          col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
          len_1 <- length(levels(factor(knnclass_pred)))
          len_2 <- length(levels(factor(set[,3])))
          level_1 <- levels(factor(knnclass_pred))
          level_2 <- levels(factor(set[,3]))
          col_1 <- col_vector[1:len_1]
          col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
          color_1 <- as.character(factor(knnclass_pred, levels = level_1, labels = col_1))
          color_2 <- as.character(factor(set[,3], levels = level_2, labels = col_2))
          p <- plot(set[, -3],
                    main = sprintf('K-NN (Test set) - k = %d',input$knnclassk),
                    xlab = sprintf("PC1 (%2.2f%%)", varpca[1]*100), ylab = sprintf("PC2 (%2.2f%%)", varpca[2]*100),
                    xlim = range(X1), ylim = range(X2))
          contour(X1, X2, matrix(as.numeric(knnclass_pred), length(X1), length(X2)), add = TRUE)
          points(grid_set, pch = '.', col = color_1)
          points(set, pch = 21, bg = color_2)
          legend(x = "topright",
                 inset = c(-.09, .4),
                 title = "Classes",
                 legend = level_2,
                 col = col_2,
                 bg = rgb(1, 0, 0, alpha = 0.15),
                 box.lty = 0,
                 pch = 19,
                 xpd = TRUE)
        }
      }
  })
  
  ###################################### display waiting massage for knn's plot ############
  knnclassplot_waiting <- reactiveValues(ok = FALSE)

  observeEvent(input$knnclassplotview, {
    if(input$knnclassplotview == 1 && !is.null(knnclass_analysis())){
    shinyjs::show("knnclassplotwait")
    knnclassplot_waiting$ok <- FALSE
    Sys.sleep(5)
    knnclassplot_waiting$ok <- TRUE
    shinyjs::hide("knnclassplotwait")
    }
  })

  # ############################################## knn class prediction on new data
  # #### erorr checking
  # # error checking
  output$uploaded_knnclassdata_prediction_hide_tabpanel <- reactive({
    error_catghing_knnclass_prediction <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_knnclass_prediction)
      checknull <- NCOL(uploaded_knnclass_data_prediction()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_knnclass_prediction(input$uploaded_knnclass_prediction$name, input$dataknnclassfileformat_prediction)
  })
  outputOptions(output, 'uploaded_knnclassdata_prediction_hide_tabpanel', suspendWhenHidden=FALSE)
  ############### uploaded prediction table
  uploaded_knnclass_data_prediction <- eventReactive(input$uploaded_knnclass_prediction,{
    if(!is.null(input$uploaded_knnclass_prediction)){
      if(input$dataknnclassfileformat == "," && input$knnclassheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_knnclass_prediction$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if(input$dataknnclassfileformat == "," && input$knnclassheader == "No") {
        rawdata <- try(read_csv(input$uploaded_knnclass_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$dataknnclassfileformat == "\t" && input$knnclassheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_knnclass$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$dataknnclassfileformat == "\t" && input$knnclassheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_knnclass_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ############### new table for prediction choices
  observeEvent(uploaded_knnclass_data_prediction(), {
    if(!is.null(uploaded_knnclass_data_prediction())){
      response_var <- names(uploaded_knnclass_data_prediction())
      updateSelectInput(
        session,
        "knnclass_prediction_response",
        choices = response_var)
    }
  })
  observeEvent(uploaded_knnclass_data_prediction(), {
    if(!is.null(uploaded_knnclass_data_prediction())){
      dftype <- sapply(uploaded_knnclass_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_vars <- names(uploaded_knnclass_data_prediction())[isnumeric]
      updatePickerInput(
        session,
        "knnclass_prediction_predictors",
        choices = predictor_vars)
    }
  })
  ######################################## prediction result
  knnclass_analysis_prediction <- reactive({
    if(!is.null(uploaded_knnclass_data()) && input$knnclassprepross_prediction_result){
      if(is.null(input$knnclassresponse) | is.null(input$knnclasspredictors)){
        return(NULL)
      }
      ############ for knnclass prediction
      uploaded_knnclass_data_prediction <- isolate(uploaded_knnclass_data_prediction())
      predictors_prediction <- input$knnclass_prediction_predictors
      num <- which(names(uploaded_knnclass_data_prediction) %in% predictors_prediction)
      if(length(num) > 0 ){
      knnclass_df_prediction <- data.frame(uploaded_knnclass_data_prediction[, predictors_prediction])
      knnclass_df_prediction <- scale(knnclass_df_prediction)
      knnclass_df_prediction <- data.frame(knnclass_df_prediction)
      knnclass_df_prediction
      } else{
        return(NULL)
      }
      ##########################################
      uploaded_knnclass_data <- isolate(uploaded_knnclass_data())
      response <- input$knnclassresponse
      predictors <- input$knnclasspredictors
      res_name <- which(names(uploaded_knnclass_data) %in% response)
      predi_name <- which(names(uploaded_knnclass_data) %in% predictors)
      if(length(res_name) > 0 | length(predi_name) > 0 ){
        knnclass_df <- data.frame(uploaded_knnclass_data[, c(predictors, response)])
        fac <- length(knnclass_df)
        knnclass_df[,fac] <- factor(knnclass_df[,fac])
        l <- length(levels(knnclass_df[,fac]))
        if(l > 10){
          return(NULL)
        }
      } else{
        return(NULL)
      }

      ############## check both file to be the almost the same
      similar_name <- which(names(knnclass_df) %in% names(knnclass_df_prediction))
      if(length(similar_name) < 1){
        return(NULL)
      }
      ###########################################################
      set.seed(123)
      split <- sample.split(knnclass_df[, fac], SplitRatio = input$knnclassratiosplit)
      training_set <- subset(knnclass_df, split == TRUE)
      test_set <- subset(knnclass_df, split == FALSE)
      training_set[-fac] <- scale(training_set[-fac])
      test_set[-fac] <- scale(test_set[-fac])
      if(input$knnclasskfoldcv > 0) {
        folds <- createFolds(knnclass_df[,fac] , k = input$knnclasskfoldcv)
        if(input$knnclassprob == "FALSE"){
          prob <- FALSE
        }else{
          prob <- TRUE
        }
        if(input$knnclassuseall == "FALSE"){
          use.all <- FALSE
        } else {
          use.all <- TRUE
        }
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          knnclass_pred <- class::knn(train = training_fold[, -fac],
                                      test = knnclass_df_prediction,
                                      cl = training_fold[, fac],
                                      k = input$knnclassk,
                                      l = input$knnclassl,
                                      prob = prob,
                                      use.all = use.all)
          
          return(knnclass_pred)
        })
        cv
        Mode <- function(x) {
          ux <- unique(x)
          return(ux[which.max(tabulate(match(x, ux)))])
        }
        folds_predicted <- data.frame(Mode(cv)[[1]])
        names(folds_predicted) <- response
        folds_predicted
      } else{
        if(input$knnclassprob == "FALSE"){
          prob <- FALSE
        }else{
          prob <- TRUE
        }
        if(input$knnclassuseall == "FALSE"){
          use.all <- FALSE
        } else {
          use.all <- TRUE
        }
        knnclass_pred <- class::knn(train = training_set[, -fac],
                                    test = knnclass_df_prediction,
                                    cl = training_set[, fac],
                                    k = input$knnclassk,
                                    l = input$knnclassl,
                                    prob = prob,
                                    use.all = use.all)
        knnclass_pred <- data.frame(knnclass_pred)
        knnclass_pred
      }
    }

  })


  ############ table of actual value for new data
  output$knnclass_actual_response <- renderDT({
    if(!is.null(input$knnclass_prediction_response) && input$knnclassprepross_prediction_result) {
      uploaded_knnclass_data_prediction <- isolate(uploaded_knnclass_data_prediction())
      response_prediction <- input$knnclass_prediction_response
      knnclass_df_prediction <- data.frame(uploaded_knnclass_data_prediction[, response_prediction])
      knnclass_df_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 8),
          rownames = T)
    }
  })
  ############ table of predicted value for new data
  output$knnclass_predicted_response <- renderDT({
    req(knnclass_analysis_prediction())
    if(input$knnclassprepross_prediction_result) {
      knnclass_analysis_prediction <- isolate(knnclass_analysis_prediction())
      knnclass_analysis_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 8),
          rownames = T)
    }
  })

  ### Download knnclass prediction table
  output$download_prediction_knnclass_table <- renderUI({
    if(input$knnclassprepross_prediction_result) {
      downloadButton('knnclassprediction_table', 'Download prediction table')
    }
  })

  output$knnclassprediction_table <- downloadHandler(

    filename = function(){
      paste0(Sys.Date(),"_prediction_table.csv")
    },
    content = function(file){
      knnclass_analysis_prediction() %>%
        write_csv(file)
    }
  )
  
  ########################################## KNN Regression ##############################################
  # error checking
  output$uploaded_knnregdata_hide_tabpanel <- reactive({
    error_catghing_knnreg <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_knnreg)
      checknull <- NCOL(uploaded_knnreg_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_knnreg(input$uploaded_knnreg$name, input$dataknnregfileformat)
  })
  outputOptions(output, 'uploaded_knnregdata_hide_tabpanel', suspendWhenHidden=FALSE)
  ###############
  uploaded_knnreg_data <- eventReactive(input$uploaded_knnreg, {
    if(!is.null(input$uploaded_knnreg)){
      
      if(input$dataknnregfileformat == "," && input$knnregheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_knnreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$dataknnregfileformat == "," && input$knnregheader == "No") {
        rawdata <- try(read_csv(input$uploaded_knnreg$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$dataknnregfileformat == "\t" && input$knnregheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_knnreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$dataknnregfileformat == "\t" && input$knnregheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_knnreg$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  output$knnregdatashow <- renderDT({
    
    req(input$uploaded_knnreg)
    if(input$knnregprepross) {
      knnreg_table <- uploaded_knnreg_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          editable= T,
          style = "auto",
          rownames = FALSE)
      knnreg_table
    }
  })
  # response and predictor choices
  observeEvent(uploaded_knnreg_data(), {
    if(!is.null(uploaded_knnreg_data())){
      response_var <- names(uploaded_knnreg_data())
      updateSelectInput(
        session,
        "knnregresponse",
        choices = response_var)
    }
  })
  # numeric
  observeEvent(uploaded_knnreg_data(), {
    if(!is.null(uploaded_knnreg_data())){
      dftype <- sapply(uploaded_knnreg_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_vars <- names(uploaded_knnreg_data())[isnumeric]
      updatePickerInput(
        session,
        "knnregpredictors",
        choices = predictor_vars)
    }
  })
  
  ################ knnreg analysis 
  knnreg_analysis <- reactive({
    if(!is.null(uploaded_knnreg_data()) && input$knnregaction){
      if(is.null(input$knnregresponse) | is.null(input$knnregpredictors)){
        return(NULL)
      }
      uploaded_knnreg_data <- isolate(uploaded_knnreg_data())
      response <- input$knnregresponse
      predictors <- input$knnregpredictors
      res_name <- which(names(uploaded_knnreg_data) %in% response)
      predi_name <- which(names(uploaded_knnreg_data) %in% predictors)
      if(length(res_name) > 0 | length(predi_name) > 0 ){
        knnreg_df <- data.frame(uploaded_knnreg_data[, c(predictors, response)])
        fac <- length(knnreg_df)
        knnreg_df
      } else{
        return(NULL)
      }
      set.seed(123)
      split <- sample.split(knnreg_df[, fac], SplitRatio = input$knnregratiosplit)
      training_set <- subset(knnreg_df, split == TRUE)
      test_set <- subset(knnreg_df, split == FALSE)
      if(input$knnregkfoldcv > 0) {
        folds <- createFolds(knnreg_df[,fac] , k = input$knnregkfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          knnreg_pred <- knn.reg(train = training_fold[-fac],
                               test = test_fold[-fac],
                               y = training_fold[fac],
                               k = input$knnregk,
                               algorithm = input$knnregalgorithm)$pred
          mse <- MLmetrics::MSE(knnreg_pred, test_fold[,fac])
          mae <- MLmetrics::MAE(knnreg_pred, test_fold[,fac])
          rmse <- MLmetrics::RMSE(knnreg_pred, test_fold[,fac])
          r2 <- MLmetrics::R2_Score(knnreg_pred, test_fold[,fac])
          accuracy <- data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2)
          return(accuracy)
        })
        Accuracy <- round(colMeans(do.call(rbind, cv)), 3)
        Accuracy <- data.frame(t(Accuracy))
      } else{
        knnreg_pred <- knn.reg(train = training_set[-fac],
                               test = test_set[-fac],
                               y = training_set[fac],
                               k = input$knnregk,
                               algorithm = input$knnregalgorithm)$pred
        mse <- MLmetrics::MSE(knnreg_pred, test_fold[,fac])
        mae <- MLmetrics::MAE(knnreg_pred, test_fold[,fac])
        rmse <- MLmetrics::RMSE(knnreg_pred, test_fold[,fac])
        r2 <- MLmetrics::R2_Score(knnreg_pred, test_fold[,fac])
        accuracy <- round(data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2), 3)
        accuracy  
      }
    }
    
  })
  ######################## knnreg 
  output$knnregoutput <- renderDT({
    if(!is.null(knnreg_analysis()) && input$knnregaction) {
      knnreg_analysis <- isolate(knnreg_analysis())
      knnreg_output <- knnreg_analysis %>%
        datatable(
          options = list(scrollX = TRUE, dom = "t"),
          rownames = FALSE)
      knnreg_output
    }
  })
  ########################################## knn plot regression ##################################
  ###################### knn plot choices
  knnregplotchoices <- reactive({
    if(!is.null(uploaded_knnreg_data()) && input$knnregaction){
      if(is.null(input$knnregresponse) | is.null(input$knnregpredictors)){
        return(NULL)
      }
      uploaded_knnreg_data <- isolate(uploaded_knnreg_data())
      predictors <- input$knnregpredictors
      pre_name <- which(names(uploaded_knnreg_data) %in% predictors)
      if(length(pre_name) > 0){
        knnreg_df <- data.frame(uploaded_knnreg_data[, predictors])
        knnreg_df
      } else{
        return(NULL)
      }
    }
  })
  
  # ############################ choice x
  observeEvent(knnregplotchoices(),{
    if(!is.null(knnregplotchoices())){
      knnregplotchoicex <- names(knnregplotchoices())
      updateSelectInput(
        session,
        "knnregplotx",
        choices = knnregplotchoicex)
    }
  })
 
  ###################################### knn regression plot 
  output$knnregplottest <- renderPlot({
    if(!is.null(uploaded_knnreg_data()) && input$knnregplotview){
      if(is.null(input$knnregresponse) | is.null(input$knnregpredictors)){
        return(NULL)
      }
      uploaded_knnreg_data <- isolate(uploaded_knnreg_data())
      response <- input$knnregresponse
      predictors <- input$knnregpredictors
      res_name <- which(names(uploaded_knnreg_data) %in% response)
      predinum_name <- which(names(uploaded_knnreg_data) %in% predictors)
      if(length(res_name) > 0 | length(predinum_name) > 0 ){
        knnreg_df <- data.frame(uploaded_knnreg_data[, c(predictors, response)])
        fac <- length(knnreg_df)
      } else{
        return(NULL)
      }
      set.seed(123)
      split <- sample.split(knnreg_df[,fac], SplitRatio = input$knnregratiosplit)
      training_set <- subset(knnreg_df, split == TRUE)
      test_set <- subset(knnreg_df, split == FALSE)
        if(length(knnreg_df) >= 2 && !is.null(input$knnregplotx)){
          knnreg_pred <- knn.reg(train = training_set[-fac],
                                 test = test_set[-fac],
                                 y = training_set[fac],
                                 k = input$knnregk,
                                 algorithm = input$knnregalgorithm)$pred
          x <- data.frame(test_set[input$knnregplotx])
          p <- ggplot(aes(x = x[,1], y= test_set[,fac]), data = test_set) + geom_point(colour = '#145A32') +
            geom_line(aes(x = x[,1], y = knnreg_pred),colour = '#B7950B', size = 1) +
            xlab(input$knnregplotx) + ylab(response) + 
            theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                    axis.text = element_text(colour = "black"),
                                    axis.text.y = element_text(colour = "black")) +
            theme(axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                  axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                  axis.text.y = element_text(family = "Times",colour = "black"),
                  plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                  axis.title.y = element_text(family = "Times", size = rel(1.4)),
                  axis.title.x = element_text(family = "Times", size = rel(1.4)))+
            labs(subtitle = sprintf("K-NN regression plot - k = %d", input$knnregk))
          p
        }
      }
  })

  # ############################################## knn regression prediction on new data
  # #### erorr checking

  output$uploaded_knnregdata_prediction_hide_tabpanel <- reactive({
    error_catghing_knnreg_prediction <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_knnreg_prediction)
      checknull <- NCOL(uploaded_knnreg_data_prediction()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_knnreg_prediction(input$uploaded_knnreg_prediction$name, input$dataknnregfileformat_prediction)
  })
  outputOptions(output, 'uploaded_knnregdata_prediction_hide_tabpanel', suspendWhenHidden=FALSE)
  ############### uploaded prediction table
  uploaded_knnreg_data_prediction <- eventReactive(input$uploaded_knnreg_prediction,{
    if(!is.null(input$uploaded_knnreg_prediction)){
      if(input$dataknnregfileformat == "," && input$knnregheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_knnreg_prediction$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if(input$dataknnregfileformat == "," && input$knnregheader == "No") {
        rawdata <- try(read_csv(input$uploaded_knnreg_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$dataknnregfileformat == "\t" && input$knnregheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_knnreg$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$dataknnregfileformat == "\t" && input$knnregheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_knnreg_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ############### new table for prediction choices
  observeEvent(uploaded_knnreg_data_prediction(), {
    if(!is.null(uploaded_knnreg_data_prediction())){
      response_var <- names(uploaded_knnreg_data_prediction())
      updateSelectInput(
        session,
        "knnreg_prediction_response",
        choices = response_var)
    }
  })
  observeEvent(uploaded_knnreg_data_prediction(), {
    if(!is.null(uploaded_knnreg_data_prediction())){
      dftype <- sapply(uploaded_knnreg_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_vars <- names(uploaded_knnreg_data_prediction())[isnumeric]
      updatePickerInput(
        session,
        "knnreg_prediction_predictors",
        choices = predictor_vars)
    }
  })
  ######################################## prediction result
  knnreg_analysis_prediction <- reactive({
    if(!is.null(uploaded_knnreg_data()) && input$knnregprepross_prediction_result){
      if(is.null(input$knnregresponse) | is.null(input$knnregpredictors)){
        return(NULL)
      }
      ############ for knnreg prediction
      uploaded_knnreg_data_prediction <- isolate(uploaded_knnreg_data_prediction())
      predictors_prediction <- input$knnreg_prediction_predictors
      num <- which(names(uploaded_knnreg_data_prediction) %in% predictors_prediction)
      if(length(num) > 0 ){
        knnreg_df_prediction <- data.frame(uploaded_knnreg_data_prediction[, predictors_prediction])
        knnreg_df_prediction
      } else{
        return(NULL)
      }
      ##########################################
      uploaded_knnreg_data <- isolate(uploaded_knnreg_data())
      response <- input$knnregresponse
      predictors <- input$knnregpredictors
      res_name <- which(names(uploaded_knnreg_data) %in% response)
      predi_name <- which(names(uploaded_knnreg_data) %in% predictors)
      if(length(res_name) > 0 | length(predi_name) > 0 ){
        knnreg_df <- data.frame(uploaded_knnreg_data[, c(predictors, response)])
        fac <- length(knnreg_df)
      } else{
        return(NULL)
      }
      
      ############## check both file to be the almost the same
      similar_name <- which(names(knnreg_df) %in% names(knnreg_df_prediction))
      if(length(similar_name) < 1){
        return(NULL)
      }
      ###########################################################
      set.seed(123)
      split <- sample.split(knnreg_df[, fac], SplitRatio = input$knnregratiosplit)
      training_set <- subset(knnreg_df, split == TRUE)
      test_set <- subset(knnreg_df, split == FALSE)
      if(input$knnregkfoldcv > 0) {
        folds <- createFolds(knnreg_df[,fac] , k = input$knnregkfoldcv)
        cv <- lapply(folds, function(x){      
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          knnreg_pred <- knn.reg(train = training_fold[-fac],
                               test = knnreg_df_prediction,
                               y = training_fold[fac],
                               k = input$knnregk,
                               algorithm = input$knnregalgorithm)$pred
          return(knnreg_pred)
        })
        cv
        Prediction <- round(colMeans(do.call(rbind, cv)), 3)
        Prediction <- data.frame(Prediction)
        colnames(Prediction) <- response
        Prediction
      } else{
        knnreg_pred <- knn(train = training_set[-fac],
                             test = knnreg_df_prediction,
                             y = training_set[fac],
                             k = input$knnregk,
                             algorithm = input$knnregalgorithm)$pred
        knnreg_pred <- data.frame(knnreg_pred)
        colnames(knnreg_pred) <- response
        knnreg_pred
      }
    }
    
  })
  
  
  ############ table of actual value for new data
  output$knnreg_actual_response <- renderDT({
    if(!is.null(input$knnreg_prediction_response) && input$knnregprepross_prediction_result) {
      uploaded_knnreg_data_prediction <- isolate(uploaded_knnreg_data_prediction())
      response_prediction <- input$knnreg_prediction_response
      knnreg_df_prediction <- data.frame(uploaded_knnreg_data_prediction[, response_prediction])
      knnreg_df_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 8),
          rownames = T)
    }
  })
  ############ table of predicted value for new data
  output$knnreg_predicted_response <- renderDT({
    req(knnreg_analysis_prediction())
    if(input$knnregprepross_prediction_result) {
      knnreg_analysis_prediction <- isolate(knnreg_analysis_prediction())
      knnreg_analysis_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 8),
          rownames = T)
    }
  })
  
  ### Download knnreg prediction table
  output$download_prediction_knnreg_table <- renderUI({
    if(input$knnregprepross_prediction_result) {
      downloadButton('knnregprediction_table', 'Download prediction table')
    }
  })
  
  output$knnregprediction_table <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_prediction_table.csv")
    },
    content = function(file){
      knnreg_analysis_prediction() %>%
        write_csv(file)
    }
  )
  ####################################################### SVM #################################
  # error checking
  output$uploaded_svmdata_hide_tabpanel <- reactive({
    error_catghing_svm <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_svm)
      checknull <- NCOL(uploaded_svm_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_svm(input$uploaded_svm$name, input$datasvmfileformat)
  })
  outputOptions(output, 'uploaded_svmdata_hide_tabpanel', suspendWhenHidden=FALSE)
  ###############
  uploaded_svm_data <- eventReactive(input$uploaded_svm, {
    if(!is.null(input$uploaded_svm)){
      
      if(input$datasvmfileformat == "," && input$svmheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_svm$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$datasvmfileformat == "," && input$svmheader == "No") {
        rawdata <- try(read_csv(input$uploaded_svm$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datasvmfileformat == "\t" && input$svmheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_svm$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datasvmfileformat == "\t" && input$svmheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_svm$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  output$svmdatashow <- renderDT({
    
    req(input$uploaded_svm)
    if(input$svmprepross) {
      svm_table <- uploaded_svm_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          editable= T,
          style = "auto",
          rownames = FALSE)
      svm_table
    }
  })
  # response and predictor choices
  observeEvent(uploaded_svm_data(), {
    if(!is.null(uploaded_svm_data())){
    response_var <- names(uploaded_svm_data())
    updateSelectInput(
      session,
      "svmresponse",
      choices = response_var)
    }
  })
  observeEvent(uploaded_svm_data(), {
    if(!is.null(uploaded_svm_data())){
      dftype <- sapply(uploaded_svm_data(), class)
      isnumeric <- grep("numeric", dftype)
    predictor_vars <- names(uploaded_svm_data())[isnumeric]
    updatePickerInput(
      session,
      "svmpredictors",
      choices = predictor_vars)
    }
  })
  
  ######################################## svm parameters hiding 
  
  # degree for polynomial 
  output$hide_svm_polynomial_degree <- reactive({
    hidepoldegree <- function(){
      if(input$svmkernel == "polynomial"){
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }
    hidepoldegree()
  })
  outputOptions(output, 'hide_svm_polynomial_degree', suspendWhenHidden=FALSE)
  
  ############# COEF0
  output$hide_svm_coefo <- reactive({
    hidecoefo <- function(){
      if(input$svmkernel == "polynomial" | input$svmkernel == "sigmoid"){
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }
    hidecoefo()
  })
  outputOptions(output, 'hide_svm_coefo', suspendWhenHidden=FALSE)
  
  ############### nu
  output$hide_svm_nu <- reactive({
    hidenu <- function(){
      if(input$svmtype == "nu-classification" | input$svmtype == "one-classification" | input$svmtype == "nu-regression"){
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }
    hidenu()
  })
  outputOptions(output, 'hide_svm_nu', suspendWhenHidden=FALSE)
  ################ svm analysis 
  svm_analysis <- reactive({
    if(!is.null(uploaded_svm_data()) && input$svmaction){
      if(is.null(input$svmresponse) | is.null(input$svmpredictors)){
        return(NULL)
      }
      uploaded_svm_data <- isolate(uploaded_svm_data())
      response <- input$svmresponse
      predictors <- input$svmpredictors
      svm_df <- data.frame(uploaded_svm_data[, c(response, predictors)])
      if(input$svmtype == "C-classification"| input$svmtype ==  "nu-classification"| input$svmtype ==  "one-classification"){
      svm_df[,1] <- factor(svm_df[,1])
      }
      ########## class.weights
      userclass <- input$svmclassweights
      nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
      nu_u_c <- na.omit(nu_u_c)
      class <- table(svm_df[,1])
      if(length(nu_u_c) == length(class)){
          for(i in 1:length(nu_u_c)){
            class[i] <- nu_u_c[i]
          }
       
      } else{
        class <- NULL
       
      }
      ##############
      set.seed(123)
      split <- sample.split(svm_df[,1], SplitRatio = input$svmratiosplit)
      training_set <- subset(svm_df, split == TRUE)
      test_set <- subset(svm_df, split == FALSE)
      ################################################### SVR #######################################
      if(input$svmtype == "eps-regression"| input$svmtype ==  "nu-regression"){
        if(input$svmkfoldcv > 0) {
          folds <- createFolds(svm_df[,1] , k = input$svmkfoldcv)
          if(input$svmprobability == "FALSE"){
            probability <- FALSE
          } else{
            probability <- TRUE
          }
          cv <- lapply(folds, function(x){
            training_fold <- training_set[-x, ]
            test_fold <- test_set[-x, ]
            formula <- as.formula(paste0(names(svm_df)[1], " ~ ."))
            regressor <- svm(formula = formula,
                              data = training_fold,
                              type = input$svmtype,
                              kernel = input$svmkernel,
                              cost = input$svmcost,
                              class.weights = class,
                              gamma = input$svmgamma,
                              epsilon = input$svmepsilon,
                              coef0 = input$svmcoef0polynomial_sigmoid,
                              nu = input$svmnu,
                              degree = input$svmpolynomialdegree,
                              # cachesize = input$svmcachesize,
                              tolerance = input$svmtolerance,
                              shrinking = factor(input$svmshrinking),
                              # fitted = factor(input$svmfitted),
                              probability = probability,
                              na.action = na.omit,
                              scale = TRUE)
            y_pred <- predict(regressor, newdata = test_fold[-1])
            mse <- MLmetrics::MSE(y_pred, test_fold[,1])
            mae <- MLmetrics::MAE(y_pred, test_fold[,1])
            rmse <- MLmetrics::RMSE(y_pred, test_fold[,1])
            r2 <- MLmetrics::R2_Score(y_pred, test_fold[,1])
            accuracy <- data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2)
            return(accuracy)
          }) 
          Accuracy <- round(colMeans(do.call(rbind, cv)), 3)
          Accuracy <- data.frame(t(Accuracy))
        } else{
          if(input$svmprobability == "FALSE"){
            probability <- FALSE
          } else{
            probability <- TRUE
          }
          formula <- as.formula(paste0(names(svm_df)[1], " ~ ."))
          classifier <- svm(formula = formula,
                            data = training_set,
                            type = input$svmtype,
                            kernel = input$svmkernel,
                            cost = input$svmcost,
                            class.weights = class,
                            gamma = input$svmgamma,
                            epsilon = input$svmepsilon,
                            coef0 = input$svmcoef0polynomial_sigmoid,
                            nu = input$svmnu,
                            degree = input$svmpolynomialdegree,
                            # cachesize = input$svmcachesize,
                            tolerance = input$svmtolerance,
                            shrinking = factor(input$svmshrinking),
                            # fitted = factor(input$svmfitted),
                            probability = probability,
                            na.action = na.omit,
                            scale = TRUE)
          y_pred <- predict(classifier, newdata = test_set[-1])
          mse <- MLmetrics::MSE(y_pred, test_set[,1])
          mae <- MLmetrics::MAE(y_pred, test_set[,1])
          rmse <- MLmetrics::RMSE(y_pred, test_set[,1])
          r2 <- MLmetrics::R2_Score(y_pred, test_set[,1])
          accuracy <- round(data.frame(MSE = mse, MAE = mae, RMSE= rmse, R2= r2), 3)
          accuracy
        }
      } else{
      
      training_set[-1] <- scale(training_set[-1])
      test_set[-1] <- scale(test_set[-1])
      if(input$svmkfoldcv > 0) {
      folds <- createFolds(svm_df[,1] , k = input$svmkfoldcv)
      if(input$svmprobability == "FALSE"){
        probability <- FALSE
      } else{
        probability <- TRUE
      }
      cv <- lapply(folds, function(x){
        training_fold <- training_set[-x, ]
        test_fold <- test_set[-x, ]
        formula <- as.formula(paste0(names(svm_df)[1], " ~ ."))
        classifier <- svm(formula = formula,
                          data = training_fold,
                          type = input$svmtype,
                          kernel = input$svmkernel,
                          cost = input$svmcost,
                          class.weights = class,
                          gamma = input$svmgamma,
                          epsilon = input$svmepsilon,
                          coef0 = input$svmcoef0polynomial_sigmoid,
                          nu = input$svmnu,
                          degree = input$svmpolynomialdegree,
                          # cachesize = input$svmcachesize,
                          tolerance = input$svmtolerance,
                          shrinking = factor(input$svmshrinking),
                          # fitted = factor(input$svmfitted),
                          probability = probability,
                          na.action = na.omit,
                          scale = FALSE)
        y_pred <- predict(classifier, newdata = test_fold[-1])
        result <- confusion_matrix(test_fold[, 1], y_pred)
        return(result)
      })
      multiclass_con_av(cv)
      } else{
        if(input$svmprobability == "FALSE"){
          probability <- FALSE
        } else{
          probability <- TRUE
        }
        formula <- as.formula(paste0(names(svm_df)[1], " ~ ."))
        classifier <- svm(formula = formula,
                          data = training_set,
                          type = input$svmtype,
                          kernel = input$svmkernel,
                          cost = input$svmcost,
                          class.weights = class,
                          gamma = input$svmgamma,
                          epsilon = input$svmepsilon,
                          coef0 = input$svmcoef0polynomial_sigmoid,
                          nu = input$svmnu,
                          degree = input$svmpolynomialdegree,
                          # cachesize = input$svmcachesize,
                          tolerance = input$svmtolerance,
                          shrinking = factor(input$svmshrinking),
                          # fitted = factor(input$svmfitted),
                          probability = probability,
                          na.action = na.omit,
                          scale = FALSE)
        y_pred <- predict(classifier, newdata = test_set[-1])
        confusion_matrix(y_true = test_set[, 1], y_pred = y_pred)
      }
      }
    }

  })
  output$svmoutput <- renderDT({
    if(!is.null(svm_analysis()) && input$svmaction) {
      svm_analysis <- isolate(svm_analysis())
      svm_output <- svm_analysis %>%
        datatable(
          options = list(dom = "t"),
          rownames = TRUE)
      svm_output
    }
  })
  
  
  ###################### svm plot choices
  svmplotchoices <- reactive({
    if(!is.null(uploaded_svm_data()) && input$svmaction){
      if(is.null(input$svmresponse) | is.null(input$svmpredictors)){
        return(NULL)
      }
      uploaded_svm_data <- isolate(uploaded_svm_data())
      predictors <- input$svmpredictors
     pre_name <- which(names(uploaded_svm_data) %in% predictors)
      if(length(pre_name) > 0){
        svm_df <- data.frame(uploaded_svm_data[, predictors])
        svm_df
        } else{
        return(NULL)
      }
    }
  })

  # ############################ choice x
  observeEvent(svmplotchoices(),{
    if(!is.null(svmplotchoices())){
      svmplotchoicex <- names(svmplotchoices())
      updateSelectInput(
        session,
        "svmplotx",
        choices = svmplotchoicex)
    }
  })
  ############################ choice y
  observeEvent(svmplotchoices(),{
    if(!is.null(svmplotchoices())){
      len <- length(svmplotchoices())
      svmplotchoicex <- names(svmplotchoices())[len:1]
      updateSelectInput(
        session,
        "svmploty",
        choices = svmplotchoicex)
    }
  })
  # ###### plot svm
  output$svmplot <- renderPlot({
    if(!is.null(uploaded_svm_data()) && input$svmplotview){
      if(is.null(input$svmresponse) | is.null(input$svmpredictors)){
        return(NULL)
      }
      uploaded_svm_data <- isolate(uploaded_svm_data())
      response <- input$svmresponse
      predictors <- input$svmpredictors
      res_name <- which(names(uploaded_svm_data) %in% response)
      pre_name <- which(names(uploaded_svm_data) %in% predictors)
      if(length(res_name) > 0 && length(pre_name) > 0){
        svm_df <- data.frame(uploaded_svm_data[, c(response, predictors)])
        if(input$svmtype == "C-classification"| input$svmtype ==  "nu-classification"| input$svmtype ==  "one-classification"){
          svm_df[,1] <- factor(svm_df[,1])
          svm_df
        }
      } else{
        return(NULL)
      }
      ########## class.weights
      userclass <- input$svmclassweights
      nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
      nu_u_c <- na.omit(nu_u_c)
      class <- table(svm_df[,1])
      if(length(nu_u_c) == length(class)){
        for(i in 1:length(nu_u_c)){
          class[i] <- nu_u_c[i]
        }

      } else{
        class <- NULL

      }
      ##############
      set.seed(123)
      split <- sample.split(svm_df[,1], SplitRatio = input$svmratiosplit)
      training_set <- subset(svm_df, split == TRUE)
      test_set <- subset(svm_df, split == FALSE)
      training_set[-1] <- scale(training_set[-1])
      test_set[-1] <- scale(test_set[-1])
        if(input$svmprobability == "FALSE"){
          probability <- FALSE
        } else{
          probability <- TRUE
        }
        formula <- as.formula(paste0(names(svm_df)[1], " ~ ."))
        classifier <- svm(formula = formula,
                          data = training_set,
                          type = input$svmtype,
                          kernel = input$svmkernel,
                          cost = input$svmcost,
                          class.weights = class,
                          gamma = input$svmgamma,
                          epsilon = input$svmepsilon,
                          coef0 = input$svmcoef0polynomial_sigmoid,
                          nu = input$svmnu,
                          degree = input$svmpolynomialdegree,
                          # cachesize = input$svmcachesize,
                          tolerance = input$svmtolerance,
                          shrinking = factor(input$svmshrinking),
                          # fitted = factor(input$svmfitted),
                          probability = probability,
                          na.action = na.omit,
                          scale = FALSE)
        if(length(svm_df) >= 3 && !is.null(input$svmplotx) && !is.null(input$svmploty)){
          par(mar=c(4,4,2,3.5))
          x <- input$svmplotx
          y <- input$svmploty
          ncx <- which(names(svm_df) %in% x)
          ncy <- which(names(svm_df) %in% y)
          formula <- as.formula(paste0(y, " ~ ", x))
          slice_names <- names(svm_df)[c(-1, -ncx, -ncy)]
          slice_names_list <- list()
          for(i in 1:length(slice_names)){
            slice_names_list[slice_names[i]] <- i
          }
          p <- plot(classifier, svm_df, formula, fill = TRUE, grid = 50,
                    slice = slice_names_list, svSymbol = 1, dataSymbol = 4, symbolPalette = rainbow(7),
                    color.palette = terrain.colors)
          p
        }
      }
  })
  ######################### Hide x and y choices for svm plot 
  output$hide_svm_plot_xy <- reactive({
    hidesvmplotxy <- function(){
      if(input$svmtype == "eps-regression"| input$svmtype ==  "nu-regression"){
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }
    hidesvmplotxy()
  })
  outputOptions(output, 'hide_svm_plot_xy', suspendWhenHidden=FALSE)
  
  ######## hide svr 
  output$hide_svr_plot <- reactive({
    hidesvmplotxy <- function(){
      if(input$svmtype != "eps-regression"| input$svmtype !=  "nu-regression"){
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }
    hidesvmplotxy()
  })
  outputOptions(output, 'hide_svr_plot', suspendWhenHidden=FALSE)
  
  
  
  ###################################### SVR plot ##################################
  output$svrplot <- renderPlot({
    if(!is.null(uploaded_svm_data()) && input$svrplotview){
      if(is.null(input$svmresponse) | is.null(input$svmpredictors)){
        return(NULL)
      }
      if(input$svmtype == "eps-regression"| input$svmtype ==  "nu-regression"){
      uploaded_svm_data <- isolate(uploaded_svm_data())
      response <- input$svmresponse
      predictors <- input$svmpredictors
      res_name <- which(names(uploaded_svm_data) %in% response)
      pre_name <- which(names(uploaded_svm_data) %in% predictors)
      if(length(res_name) > 0 && length(pre_name) > 0){
        svm_df <- data.frame(uploaded_svm_data[, c(response, predictors)])
      } else{
        return(NULL)
      }
      ########## class.weights
      userclass <- input$svmclassweights
      nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
      nu_u_c <- na.omit(nu_u_c)
      class <- table(svm_df[,1])
      if(length(nu_u_c) == length(class)){
        for(i in 1:length(nu_u_c)){
          class[i] <- nu_u_c[i]
        }
        
      } else{
        class <- NULL
        
      }
      ##############
      set.seed(123)
      split <- sample.split(svm_df[,1], SplitRatio = input$svmratiosplit)
      training_set <- subset(svm_df, split == TRUE)
      test_set <- subset(svm_df, split == FALSE)
        if(input$svmprobability == "FALSE"){
          probability <- FALSE
        } else{
          probability <- TRUE
        }
        formula <- as.formula(paste0(names(svm_df)[1], " ~ ."))
        classifier <- svm(formula = formula,
                          data = training_set,
                          type = input$svmtype,
                          kernel = input$svmkernel,
                          cost = input$svmcost,
                          class.weights = class,
                          gamma = input$svmgamma,
                          epsilon = input$svmepsilon,
                          coef0 = input$svmcoef0polynomial_sigmoid,
                          nu = input$svmnu,
                          degree = input$svmpolynomialdegree,
                          # cachesize = input$svmcachesize,
                          tolerance = input$svmtolerance,
                          shrinking = factor(input$svmshrinking),
                          # fitted = factor(input$svmfitted),
                          probability = probability,
                          na.action = na.omit,
                          scale = TRUE)
        y_pred <- predict(classifier, newdata = test_set[-1])
        x=1:length(test_set[,1])
        g <- ggplot() + geom_point(aes(x = x, y = test_set[,1]), colour = '#145A32') +
          geom_line(aes(x = x, y = predict(classifier, newdata = test_set[-1])),colour = '#B7950B') +
          xlab('Level') + ylab(input$svmresponse) +
          theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                  axis.text = element_text(colour = "black"),
                                  axis.text.y = element_text(colour = "black")) +
          theme(axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                axis.text.y = element_text(family = "Times",colour = "black"),
                plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                axis.title.y = element_text(family = "Times", size = rel(1.4)),
                axis.title.x = element_text(family = "Times", size = rel(1.4)))+
          labs(subtitle = "SVM regression plot    ")
        g
      }
    }
  })

  ############################################## SVM prediction on new data
  # error checking
  output$uploaded_svmdata_prediction_hide_tabpanel <- reactive({
    error_catghing_svm_prediction <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_svm_prediction)
      checknull <- NCOL(uploaded_svm_data_prediction()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_svm_prediction(input$uploaded_svm_prediction$name, input$datasvmfileformat_prediction)
  })
  outputOptions(output, 'uploaded_svmdata_prediction_hide_tabpanel', suspendWhenHidden=FALSE)
  ############### uploaded prediction table
  uploaded_svm_data_prediction <- eventReactive(input$uploaded_svm_prediction, {
    if(!is.null(input$uploaded_svm_prediction)){
      if(input$datasvmfileformat == "," && input$svmheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_svm_prediction$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if(input$datasvmfileformat == "," && input$svmheader == "No") {
        rawdata <- try(read_csv(input$uploaded_svm_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datasvmfileformat == "\t" && input$svmheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_svm$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datasvmfileformat == "\t" && input$svmheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_svm_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ############### new table for prediction choices
  observeEvent(uploaded_svm_data_prediction(), {
    if(!is.null(uploaded_svm_data_prediction())){
      response_var <- names(uploaded_svm_data_prediction())
      updateSelectInput(
        session,
        "svm_prediction_response",
        choices = response_var)
    }
  })
  observeEvent(uploaded_svm_data_prediction(), {
    if(!is.null(uploaded_svm_data_prediction())){
      dftype <- sapply(uploaded_svm_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_vars <- names(uploaded_svm_data_prediction())[isnumeric]
      updatePickerInput(
        session,
        "svm_prediction_num",
        choices = predictor_vars)
    }
  })
  ######################################## svm prediction result
  svm_analysis_prediction <- reactive({
    if(!is.null(uploaded_svm_data()) && input$svmprepross_prediction_result){
      if(is.null(input$svmresponse) | is.null(input$svmpredictors)){
        return(NULL)
      }
      
      if(input$svmtype == "eps-regression"| input$svmtype ==  "nu-regression"){
        ############ for svm prediction
        uploaded_svm_data_prediction <- isolate(uploaded_svm_data_prediction())
        uploaded_svm_data_prediction <- data.frame(uploaded_svm_data_prediction)
        predictorsnum_prediction <- input$svm_prediction_num
        res_name_pred <- which(names(uploaded_svm_data_prediction) %in% predictorsnum_prediction)
        if(length(res_name_pred) == 0 ){
          return(NULL)
        }
        else if(length(res_name_pred) == 1 ){
          svm_df_prediction <- data.frame(uploaded_svm_data_prediction[, predictorsnum_prediction])
          colnames(svm_df_prediction) <- predictorsnum_prediction
          svm_df_prediction
        }
        else if(length(res_name_pred) > 1 ){
          svm_df_prediction <- data.frame(uploaded_svm_data_prediction[, predictorsnum_prediction])
          svm_df_prediction
        }
        # ###### for svm analysis
        uploaded_svm_data <- isolate(uploaded_svm_data())
        response <- input$svmresponse
        predictors <- input$svmpredictors
        res_name <- which(names(uploaded_svm_data) %in% response)
        pre_name <- which(names(uploaded_svm_data) %in% predictors)
        if(length(res_name) > 0 && length(pre_name) > 0){
          svm_df <- data.frame(uploaded_svm_data[, c(response, predictors)])
        } else{
          return(NULL)
        }
        ############### check both file to be the almost the same
        similar_name <- which(names(svm_df) %in% names(svm_df_prediction))
        if(length(similar_name) < 1){
          return(NULL)
        }
        # ############################################################
        # ########## class.weights
        userclass <- input$svmclassweights
        nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
        nu_u_c <- na.omit(nu_u_c)
        class <- table(svm_df[,1])
        if(length(nu_u_c) == length(class)){
          for(i in 1:length(nu_u_c)){
            class[i] <- nu_u_c[i]
          }
          
        } else{
          class <- NULL
          
        }
        set.seed(123)
        split <- sample.split(svm_df[,1], SplitRatio = input$svmratiosplit)
        training_set <- subset(svm_df, split == TRUE)
        test_set <- subset(svm_df, split == FALSE)
        if(input$svmkfoldcv > 0){
          folds <- createFolds(svm_df[,1] , k = input$svmkfoldcv)
          if(input$svmprobability == "FALSE"){
            probability <- FALSE
          } else{
            probability <- TRUE
          }
          folds <- createFolds(svm_df[,1] , k = input$svmkfoldcv)
          cv <- lapply(folds, function(x){
            training_fold <- training_set[-x, ]
            test_fold <- test_set[-x, ]
            formula <- as.formula(paste0(names(svm_df)[1], " ~ ."))
            regressor <- svm(formula = formula,
                              data = training_fold,
                              type = input$svmtype,
                              kernel = input$svmkernel,
                              cost = input$svmcost,
                              class.weights = class,
                              gamma = input$svmgamma,
                              epsilon = input$svmepsilon,
                              coef0 = input$svmcoef0polynomial_sigmoid,
                              nu = input$svmnu,
                              degree = input$svmpolynomialdegree,
                              # cachesize = input$svmcachesize,
                              tolerance = input$svmtolerance,
                              shrinking = factor(input$svmshrinking),
                              # fitted = factor(input$svmfitted),
                              probability = probability,
                              na.action = na.omit,
                              scale = TRUE)
            y_pred <- predict(regressor,
                              newdata = svm_df_prediction)
            return(y_pred)
          })
          prediction_result <- round(colMeans(do.call(rbind, cv)), 3)
          prediction_result <- data.frame(prediction_result)
          colnames(prediction_result) <- response
          prediction_result
        } else {
          if(input$svmprobability == "FALSE"){
            probability <- FALSE
          } else{
            probability <- TRUE
          }
          formula <- as.formula(paste0(names(svm_df)[1], " ~ ."))
          regressor <- svm(formula = formula,
                            data = training_set,
                            type = input$svmtype,
                            kernel = input$svmkernel,
                            cost = input$svmcost,
                            class.weights = class,
                            gamma = input$svmgamma,
                            epsilon = input$svmepsilon,
                            coef0 = input$svmcoef0polynomial_sigmoid,
                            nu = input$svmnu,
                            degree = input$svmpolynomialdegree,
                            # cachesize = input$svmcachesize,
                            tolerance = input$svmtolerance,
                            shrinking = factor(input$svmshrinking),
                            # fitted = factor(input$svmfitted),
                            probability = probability,
                            na.action = na.omit,
                            scale = TRUE)
          y_pred <- predict(regressor,
                            newdata = svm_df_prediction)
          prediction_result <- round(data.frame(y_pred), 3)
          colnames(prediction_result) <- response
          prediction_result
        }
      } else{
      ############ for svm prediction
      uploaded_svm_data_prediction <- isolate(uploaded_svm_data_prediction())
      uploaded_svm_data_prediction <- data.frame(uploaded_svm_data_prediction)
      predictorsnum_prediction <- input$svm_prediction_num
      res_name_pred <- which(names(uploaded_svm_data_prediction) %in% predictorsnum_prediction)
      if(length(res_name_pred) == 0 ){
        return(NULL)
      }
      else if(length(res_name_pred) == 1 ){
        svm_df_prediction <- data.frame(uploaded_svm_data_prediction[, predictorsnum_prediction])
        svm_df_prediction <- scale(svm_df_prediction)
        svm_df_prediction <- data.frame(svm_df_prediction)
        colnames(svm_df_prediction) <- predictorsnum_prediction
        svm_df_prediction
      }
      else if(length(res_name_pred) > 1 ){
        svm_df_prediction <- data.frame(uploaded_svm_data_prediction[, predictorsnum_prediction])
        svm_df_prediction <- scale(svm_df_prediction)
        svm_df_prediction <- data.frame(svm_df_prediction)
        svm_df_prediction
      }
      ##########################################
      # ###### for svm analysis
      uploaded_svm_data <- isolate(uploaded_svm_data())
      response <- input$svmresponse
      predictors <- input$svmpredictors
      res_name <- which(names(uploaded_svm_data) %in% response)
      pre_name <- which(names(uploaded_svm_data) %in% predictors)
      if(length(res_name) > 0 && length(pre_name) > 0){
        svm_df <- data.frame(uploaded_svm_data[, c(response, predictors)])
          svm_df[,1] <- factor(svm_df[,1])
          svm_df
      } else{
        return(NULL)
      }
      ############### check both file to be the almost the same
      similar_name <- which(names(svm_df) %in% names(svm_df_prediction))
      if(length(similar_name) < 1){
        return(NULL)
      }
      # ############################################################
      # ########## class.weights
      userclass <- input$svmclassweights
      nu_u_c <- as.numeric(unlist(str_split(userclass, ",")))
      nu_u_c <- na.omit(nu_u_c)
      class <- table(svm_df[,1])
      if(length(nu_u_c) == length(class)){
        for(i in 1:length(nu_u_c)){
          class[i] <- nu_u_c[i]
        }

      } else{
        class <- NULL

      }
      set.seed(123)
      split <- sample.split(svm_df[,1], SplitRatio = input$svmratiosplit)
      training_set <- subset(svm_df, split == TRUE)
      test_set <- subset(svm_df, split == FALSE)
      training_set[-1] <- scale(training_set[-1])
      test_set[-1] <- scale(test_set[-1])
      if(input$svmkfoldcv > 0) {
        folds <- createFolds(svm_df[,1] , k = input$svmkfoldcv)
        if(input$svmprobability == "FALSE"){
          probability <- FALSE
        } else{
          probability <- TRUE
        }
        folds <- createFolds(svm_df[,1] , k = input$svmkfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          formula <- as.formula(paste0(names(svm_df)[1], " ~ ."))
          classifier <- svm(formula = formula,
                            data = training_fold,
                            type = input$svmtype,
                            kernel = input$svmkernel,
                            cost = input$svmcost,
                            class.weights = class,
                            gamma = input$svmgamma,
                            epsilon = input$svmepsilon,
                            coef0 = input$svmcoef0polynomial_sigmoid,
                            nu = input$svmnu,
                            degree = input$svmpolynomialdegree,
                            # cachesize = input$svmcachesize,
                            tolerance = input$svmtolerance,
                            shrinking = factor(input$svmshrinking),
                            # fitted = factor(input$svmfitted),
                            probability = probability,
                            na.action = na.omit,
                            scale = FALSE)
          y_pred <- predict(classifier,
                            newdata = svm_df_prediction)
          return(y_pred)
        })
        Mode <- function(x) {
          ux <- unique(x)
          return(ux[which.max(tabulate(match(x, ux)))])
        }
        folds_predicted <- data.frame(Mode(cv)[[1]])
        colnames(folds_predicted) <- response
        folds_predicted
      } else {
        if(input$svmprobability == "FALSE"){
          probability <- FALSE
        } else{
          probability <- TRUE
        }
        formula <- as.formula(paste0(names(svm_df)[1], " ~ ."))
        classifier <- svm(formula = formula,
                          data = training_set,
                          type = input$svmtype,
                          kernel = input$svmkernel,
                          cost = input$svmcost,
                          class.weights = class,
                          gamma = input$svmgamma,
                          epsilon = input$svmepsilon,
                          coef0 = input$svmcoef0polynomial_sigmoid,
                          nu = input$svmnu,
                          degree = input$svmpolynomialdegree,
                          # cachesize = input$svmcachesize,
                          tolerance = input$svmtolerance,
                          shrinking = factor(input$svmshrinking),
                          # fitted = factor(input$svmfitted),
                          probability = probability,
                          na.action = na.omit,
                          scale = FALSE)
        y_pred <- predict(classifier,
                          newdata = svm_df_prediction)
        y_pred <- data.frame(y_pred)
        colnames(y_pred) <- response
        y_pred
      }
    }
  }
  })
  ############ table of actual value for new data
  output$svm_actual_response <- renderDT({
    if(!is.null(input$svm_prediction_response) && input$svmprepross_prediction_result) {
      uploaded_svm_data_prediction <- isolate(uploaded_svm_data_prediction())
      response_prediction <- input$svm_prediction_response
      svm_df_prediction <- data.frame(uploaded_svm_data_prediction[, response_prediction])
      svm_df_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 8),
          rownames = T)
    }
  })

  ############ table of predicted value for new data
  output$svm_predicted_response <- renderDT({
    req(svm_analysis_prediction())
    if(input$svmprepross_prediction_result) {
      svm_analysis_prediction <- isolate(svm_analysis_prediction())
      svm_analysis_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 8),
          rownames = T)
    }
  })

  ### Download svm prediction table
  output$download_prediction_svm_table <- renderUI({
    if(input$svmprepross_prediction_result) {
      downloadButton('svmprediction_table', 'Download prediction table')
    }
  })

  output$svmprediction_table <- downloadHandler(

    filename = function(){
      paste0(Sys.Date(),"_prediction_table.csv")
    },
    content = function(file){
      svm_analysis_prediction() %>%
        write_csv(file)
    }
  )


  
  ####################################################### naive bayes #################################
  # error checking
  output$uploaded_naivebayesdata_hide_tabpanel <- reactive({
    error_catghing_naivebayes <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_naivebayes)
      checknull <- NCOL(uploaded_naivebayes_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_naivebayes(input$uploaded_naivebayes$name, input$datanaivebayesfileformat)
  })
  outputOptions(output, 'uploaded_naivebayesdata_hide_tabpanel', suspendWhenHidden=FALSE)
  ###############
  uploaded_naivebayes_data <- eventReactive(input$uploaded_naivebayes, {
    if(!is.null(input$uploaded_naivebayes)){
      
      if(input$datanaivebayesfileformat == "," && input$naivebayesheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_naivebayes$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$datanaivebayesfileformat == "," && input$naivebayesheader == "No") {
        rawdata <- try(read_csv(input$uploaded_naivebayes$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datanaivebayesfileformat == "\t" && input$naivebayesheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_naivebayes$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datanaivebayesfileformat == "\t" && input$naivebayesheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_naivebayes$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  output$naivebayesdatashow <- renderDT({
    
    req(input$uploaded_naivebayes)
    if(input$naivebayesprepross) {
      naivebayes_table <- uploaded_naivebayes_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          editable= T,
          style = "auto",
          rownames = FALSE)
      naivebayes_table
    }
  })
  # response and predictor choices
  observeEvent(uploaded_naivebayes_data(), {
    if(!is.null(uploaded_naivebayes_data())){
      response_var <- names(uploaded_naivebayes_data())
      updateSelectInput(
        session,
        "naivebayesresponse",
        choices = response_var)
    }
  })
  # numeric
  observeEvent(uploaded_naivebayes_data(), {
    if(!is.null(uploaded_naivebayes_data())){
      dftype <- sapply(uploaded_naivebayes_data(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_vars <- names(uploaded_naivebayes_data())[isnumeric]
      updatePickerInput(
        session,
        "naivebayespredictorsnum",
        choices = predictor_vars)
    }
  })
  # categorical 
  observeEvent(uploaded_naivebayes_data(), {
    if(!is.null(uploaded_naivebayes_data())){
      dftype <- sapply(uploaded_naivebayes_data(), class)
      ischar <- grep("character|logical", dftype)
      predictor_vars <- names(uploaded_naivebayes_data())[ischar]
      updatePickerInput(
        session,
        "naivebayespredictorscat",
        choices = predictor_vars)
    }
  })
  
  ################ naiveBayes analysis 
  naivebayes_analysis <- reactive({
    if(!is.null(uploaded_naivebayes_data()) && input$naivebayesaction){
      if(is.null(input$naivebayesresponse) | is.null(input$naivebayespredictorsnum) && is.null(input$naivebayespredictorscat)){
        return(NULL)
      }
      uploaded_naivebayes_data <- isolate(uploaded_naivebayes_data())
      response <- input$naivebayesresponse
      predictorsnum <- input$naivebayespredictorsnum
      predictorscat <- input$naivebayespredictorscat
      res_name <- which(names(uploaded_naivebayes_data) %in% response)
      predinum_name <- which(names(uploaded_naivebayes_data) %in% predictorsnum)
      predicat_name <- which(names(uploaded_naivebayes_data) %in% predictorscat)
      if(length(res_name) > 0 | length(predinum_name) > 0 | length(predicat_name) > 0 ){
      naivebayes_df <- data.frame(uploaded_naivebayes_data[, c(predictorsnum, predictorscat, response)])
      fac <- length(naivebayes_df)
      naivebayes_df[,fac] <- factor(naivebayes_df[,fac])
      ### create factor for categorical predictor 
      if(length(predicat_name) > 0){
        for(i in predictorscat){
          naivebayes_df[, i] <- factor(naivebayes_df[, i])
        }
       }
      } else{
        return(NULL)
      }
      set.seed(123)
      split <- sample.split(naivebayes_df[,fac], SplitRatio = input$naivebayesratiosplit)
      training_set <- subset(naivebayes_df, split == TRUE)
      test_set <- subset(naivebayes_df, split == FALSE)
      # excluding categorical predictors from scaling
      if(length(predicat_name) > 0){
        num <- which(names(naivebayes_df) %in% predictorscat)
        training_set[c(-fac, -num)] <- scale(training_set[c(-fac, -num)])
        test_set[c(-fac, -num)] <- scale(test_set[c(-fac, -num)])
      } else{
        training_set[-fac] <- scale(training_set[-fac])
        test_set[-fac] <- scale(test_set[-fac])
      }
      
      if(input$naivebayeskfoldcv > 0) {
        folds <- createFolds(naivebayes_df[,fac] , k = input$naivebayeskfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          formula <- as.formula(paste0(names(naivebayes_df)[fac], " ~ ."))
          classifier <- naiveBayes(formula,
                                   data = training_fold,
                                   laplace = input$naivebayeslaplace,
                                   na.action = na.omit)
          y_pred <- predict(classifier,
                            newdata = test_fold[-fac],
                            threshold = input$naivebayesthreshold,
                            eps = input$naivebayeseps)
          cm <- table(test_fold[, fac], y_pred)
          result <- confusion_matrix(test_fold[, fac], y_pred)
          return(result)
        })
        multiclass_con_av(cv)
      
      } else{
      formula <- as.formula(paste0(names(naivebayes_df)[fac], " ~ ."))
      classifier <- naiveBayes(formula,
                               data = training_set,
                               laplace = input$naivebayeslaplace,
                               na.action = na.omit)

      y_pred <- predict(classifier,
                        newdata = test_set[-fac],
                        threshold = input$naivebayesthreshold,
                        eps = input$naivebayeseps)
      confusion_matrix(test_set[, fac], y_pred)
      }
    }
    
  })
  ######################## naive bayes 
  output$naivebayesoutput <- renderDT({
    if(!is.null(naivebayes_analysis()) && input$naivebayesaction) {
      naivebayes_analysis <- isolate(naivebayes_analysis())
      naivebayes_output <- naivebayes_analysis %>%
        datatable(
          options = list(scrollX = TRUE, dom = "t"),
          rownames = TRUE)
      naivebayes_output
    }
  })
  ############################################################ Naive bayes plot ############################
  # plot choices 
  # making choices available
  naivebayes_plot_choices <- reactive({
    if(!is.null(uploaded_naivebayes_data()) && input$naivebayesaction){
      if(is.null(input$naivebayesresponse) | is.null(input$naivebayespredictorsnum) && is.null(input$naivebayespredictorscat)){
        return(NULL)
      }
      uploaded_naivebayes_data <- isolate(uploaded_naivebayes_data())
      response <- input$naivebayesresponse
      predictorsnum <- input$naivebayespredictorsnum
      predictorscat <- input$naivebayespredictorscat
      res_name <- which(names(uploaded_naivebayes_data) %in% response)
      predinum_name <- which(names(uploaded_naivebayes_data) %in% predictorsnum)
      predicat_name <- which(names(uploaded_naivebayes_data) %in% predictorscat)
      if(length(res_name) > 0 | length(predinum_name) > 0 | length(predicat_name) > 0 ){
        naivebayes_df <- data.frame(uploaded_naivebayes_data[, c( predictorsnum, predictorscat, response)])
        naivebayes_df
          }
        }
      })
  # choice y
  observeEvent(naivebayes_plot_choices(),{
    if(!is.null(naivebayes_plot_choices())){
      res_fac <- length(naivebayes_plot_choices())
      dftype <- sapply(naivebayes_plot_choices(), class)
      isnumeric <- grep("numeric", dftype)
      ischar <- grep("character|logical", dftype[res_fac])
      if(length(ischar) == 1){
        naivebayesplotchoicey <- names(naivebayes_plot_choices())[isnumeric]
        len <- length(naivebayesplotchoicey)
        naivebayesplotchoicey <- naivebayesplotchoicey[len:1]
      }
      else if(length(ischar) == 0){
        naivebayesplotchoicey <- names(naivebayes_plot_choices())[isnumeric]
        len <- length(naivebayesplotchoicey)
        naivebayesplotchoicey <- naivebayesplotchoicey[len:1][-1]
      }
      updateSelectInput(
        session,
        "naivebayesploty",
        choices = naivebayesplotchoicey)
    }
  })
  # choice x
  observeEvent(naivebayes_plot_choices(),{
    if(!is.null(naivebayes_plot_choices())){
      res_fac <- length(naivebayes_plot_choices())
      dftype <- sapply(naivebayes_plot_choices(), class)
      isnumeric <- grep("numeric", dftype)
      ischar <- grep("character|logical", dftype[res_fac])
      if(length(ischar) == 1){
        naivebayesplotchoicex <- names(naivebayes_plot_choices())[isnumeric]
      }
      else if(length(ischar) == 0){
        naivebayesplotchoicex <- names(naivebayes_plot_choices())[isnumeric]
        len <- length(naivebayesplotchoicex)
        naivebayesplotchoicex <- naivebayesplotchoicex[-len]
      }
      updateSelectInput(
        session,
        "naivebayesplotx",
        choices = naivebayesplotchoicex)
    }
  })
  # hide choices for PCA
  output$hide_naivebayes_choices_pca <- reactive({
    if(!is.null(naivebayes_plot_choices())){
      hidenaivebayespca <- function(){
        df_len <- length(naivebayes_plot_choices())
        dftype <- sapply(naivebayes_plot_choices(), class)
        isnumeric <- grep("numeric", dftype[-df_len])
        if(length(isnumeric) >= 3){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
      hidenaivebayespca()
    }
  })
  outputOptions(output, 'hide_naivebayes_choices_pca', suspendWhenHidden=FALSE)
  
  
  ###################################### display waiting massage for naive bayes plots ############
  naivebayesplot_waiting <- reactiveValues(ok = FALSE)
  observeEvent(input$naivebayesplotview, {
    if(input$naivebayesplotview == 1 && !is.null(naivebayes_analysis())){
      shinyjs::show("naivebayesplotwait")
      naivebayesplot_waiting$ok <- FALSE
      Sys.sleep(5)
      naivebayesplot_waiting$ok <- TRUE
      shinyjs::hide("naivebayesplotwait")
    }
  })
  ####################################### Training plot ###################################
  output$naivebayesplottraining <- renderPlot({
    if(!is.null(uploaded_naivebayes_data()) && input$naivebayesplotview){
      if(is.null(input$naivebayesresponse) | is.null(input$naivebayespredictorsnum) && is.null(input$naivebayespredictorscat)){
        return(NULL)
      }
      uploaded_naivebayes_data <- isolate(uploaded_naivebayes_data())
      response <- input$naivebayesresponse
      predictorsnum <- input$naivebayespredictorsnum
      predictorscat <- input$naivebayespredictorscat
      res_name <- which(names(uploaded_naivebayes_data) %in% response)
      predinum_name <- which(names(uploaded_naivebayes_data) %in% predictorsnum)
      predicat_name <- which(names(uploaded_naivebayes_data) %in% predictorscat)
      if(length(res_name) > 0 | length(predinum_name) > 0 | length(predicat_name) > 0 ){
        naivebayes_df <- data.frame(uploaded_naivebayes_data[, c(predictorsnum, predictorscat, response)])
        fac <- length(naivebayes_df)
        naivebayes_df[,fac] <- factor(naivebayes_df[,fac])
        ### create factor for categorical predictor 
        if(length(predicat_name) > 0){
          for(i in predictorscat){
            naivebayes_df[, i] <- factor(naivebayes_df[, i])
          }
        }
      } else{
        return(NULL)
      }
      set.seed(123)
      split <- sample.split(naivebayes_df[,fac], SplitRatio = input$naivebayesratiosplit)
      training_set <- subset(naivebayes_df, split == TRUE)
      test_set <- subset(naivebayes_df, split == FALSE)
      # excluding categorical predictors from scaling
     factors <- which(names(naivebayes_df) %in% predictorscat)
      if(length(factors) > 0){
        training_set[c(-fac, -factors)] <- scale(training_set[c(-fac, -factors)])
        test_set[c(-fac, -factors)] <- scale(test_set[c(-fac, -factors)])
      } else{
        training_set[-fac] <- scale(training_set[-fac])
        test_set[-fac] <- scale(test_set[-fac])
      }
     numeric_only <- length(naivebayes_df) - length(factors)

        if(numeric_only == 3){
          par(mar=c(4,4,5,6))
          set <- training_set
          X1 <- seq(min(set[, input$naivebayesplotx]) - 1, max(set[, input$naivebayesplotx]) + 1, by = 0.05)
          X2 <- seq(min(set[, input$naivebayesploty]) - 1, max(set[, input$naivebayesploty]) + 1, by = 0.05)
          grid_set <- expand.grid(X1, X2)
          colnames(grid_set) <- c(input$naivebayesplotx, input$naivebayesploty)
          formula <- as.formula(paste0(names(naivebayes_df)[fac], " ~ ."))
          classifier <- naiveBayes(formula,
                                   data = training_set[, c(input$naivebayesplotx, input$naivebayesploty, names(naivebayes_df)[fac])],
                                   laplace = input$naivebayeslaplace,
                                   na.action = na.omit)

          y_pred <- predict(classifier,
                            newdata = grid_set,
                            threshold = input$naivebayesthreshold,
                            eps = input$naivebayeseps)
          qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
          col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
          col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
          len_1 <- length(levels(factor(y_pred)))
          len_2 <- length(levels(factor(set[,fac])))
          level_1 <- levels(factor(y_pred))
          level_2 <- levels(factor(set[,fac]))
          col_1 <- col_vector[1:len_1]
          col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
          color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
          color_2 <- as.character(factor(set[,fac], levels = level_2, labels = col_2))
          p <- plot(set[, c(input$naivebayesplotx, input$naivebayesploty)],
                    main = 'Naive bayes (Training set)',
                    xlab = input$naivebayesplotx, ylab = input$naivebayesploty,
                    xlim = range(X1), ylim = range(X2))
          contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
          points(grid_set, pch = '.', col = color_1)
          points(set, pch = 21, bg = color_2)
          legend(x = "topright",
                 inset = c(-.09, .4),
                 title = "Classes",
                 legend = level_2,
                 col = col_2,
                 bg = rgb(1, 0, 0, alpha = 0.15),
                 box.lty = 0,
                 pch = 19,
                 xpd = TRUE)
          p
        }
        else if(numeric_only > 3){
          par(mar=c(4,4,5,6))
          pred_train <- training_set[fac]
          pred_test <- test_set[fac]
          pca <- princomp(x = training_set[-fac],  cor = TRUE, scores = TRUE, covmat = NULL)
          varpca <- pca$sdev^2 / sum(pca$sdev^2)
          training_set <- predict(pca, training_set)
          training_set <- data.frame(training_set[, 1:2])
          training_set <- cbind(training_set , pred_train)
          colnames(training_set) <- c("PC1", "PC2", "response")
          training_set$response <- factor(training_set$response)
          test_set <- predict(pca, test_set)
          test_set <- data.frame(test_set[, 1:2])
          test_set <- cbind(test_set, pred_test)
          colnames(test_set) <- c("PC1", "PC2", "response")
          test_set$response <- factor(test_set$response)
          set <- training_set
          X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
          X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.05)
          grid_set <- expand.grid(X1, X2)
          colnames(grid_set) <- c("PC1", "PC2")
          classifier <- naiveBayes(response ~ .,
                                   data = training_set,
                                   laplace = input$naivebayeslaplace,
                                   na.action = na.omit)
          y_pred <- predict(classifier,
                            newdata = grid_set,
                            threshold = input$naivebayesthreshold,
                            eps = input$naivebayeseps)
          qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
          col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
          col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
          len_1 <- length(levels(factor(y_pred)))
          len_2 <- length(levels(factor(set[,3])))
          level_1 <- levels(factor(y_pred))
          level_2 <- levels(factor(set[,3]))
          col_1 <- col_vector[1:len_1]
          col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
          color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
          color_2 <- as.character(factor(set[,3], levels = level_2, labels = col_2))
          p <- plot(set[, -3],
                    main = 'Naive bayes (Training set)',
                    xlab = sprintf("PC1 (%2.2f%%)", varpca[1]*100), ylab = sprintf("PC2 (%2.2f%%)", varpca[2]*100),
                    xlim = range(X1), ylim = range(X2))
          contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
          points(grid_set, pch = '.', col = color_1)
          points(set, pch = 21, bg = color_2)
          legend(x = "topright",
                 inset = c(-0.09, .4),
                 title = "Classes",
                 legend = level_2,
                 col = col_2,
                 bg = rgb(1, 0, 0, alpha = 0.15),
                 box.lty = 0,
                 pch = 19,
                 xpd = TRUE)
          p
        }
    }
  })
  #################################################### NAIVE bayes test plot ################
  output$naivebayesplottest <- renderPlot({
    if(!is.null(uploaded_naivebayes_data()) && input$naivebayesplotview){
      if(is.null(input$naivebayesresponse) | is.null(input$naivebayespredictorsnum) && is.null(input$naivebayespredictorscat)){
        return(NULL)
      }
      uploaded_naivebayes_data <- isolate(uploaded_naivebayes_data())
      response <- input$naivebayesresponse
      predictorsnum <- input$naivebayespredictorsnum
      predictorscat <- input$naivebayespredictorscat
      res_name <- which(names(uploaded_naivebayes_data) %in% response)
      predinum_name <- which(names(uploaded_naivebayes_data) %in% predictorsnum)
      predicat_name <- which(names(uploaded_naivebayes_data) %in% predictorscat)
      if(length(res_name) > 0 | length(predinum_name) > 0 | length(predicat_name) > 0 ){
        naivebayes_df <- data.frame(uploaded_naivebayes_data[, c(predictorsnum, predictorscat, response)])
        fac <- length(naivebayes_df)
        naivebayes_df[,fac] <- factor(naivebayes_df[,fac])
        ### create factor for categorical predictor 
        if(length(predicat_name) > 0){
          for(i in predictorscat){
            naivebayes_df[, i] <- factor(naivebayes_df[, i])
          }
        }
      } else{
        return(NULL)
      }
      set.seed(123)
      split <- sample.split(naivebayes_df[,fac], SplitRatio = input$naivebayesratiosplit)
      training_set <- subset(naivebayes_df, split == TRUE)
      test_set <- subset(naivebayes_df, split == FALSE)
      # excluding categorical predictors from scaling
      factors <- which(names(naivebayes_df) %in% predictorscat)
      if(length(factors) > 0){
        training_set[c(-fac, -factors)] <- scale(training_set[c(-fac, -factors)])
        test_set[c(-fac, -factors)] <- scale(test_set[c(-fac, -factors)])
      } else{
        training_set[-fac] <- scale(training_set[-fac])
        test_set[-fac] <- scale(test_set[-fac])
      }
      numeric_only <- length(naivebayes_df) - length(factors)
      
      if(numeric_only == 3){
        par(mar=c(4,4,5,6))
        set <- test_set
        X1 <- seq(min(set[, input$naivebayesplotx]) - 1, max(set[, input$naivebayesplotx]) + 1, by = 0.05)
        X2 <- seq(min(set[, input$naivebayesploty]) - 1, max(set[, input$naivebayesploty]) + 1, by = 0.05)
        grid_set <- expand.grid(X1, X2)
        colnames(grid_set) <- c(input$naivebayesplotx, input$naivebayesploty)
        formula <- as.formula(paste0(names(naivebayes_df)[fac], " ~ ."))
        classifier <- naiveBayes(formula,
                                 data = training_set[, c(input$naivebayesplotx, input$naivebayesploty, names(naivebayes_df)[fac])],
                                 laplace = input$naivebayeslaplace,
                                 na.action = na.omit)
        
        y_pred <- predict(classifier,
                          newdata = grid_set,
                          threshold = input$naivebayesthreshold,
                          eps = input$naivebayeseps)
        qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
        col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
        col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
        len_1 <- length(levels(factor(y_pred)))
        len_2 <- length(levels(factor(set[,fac])))
        level_1 <- levels(factor(y_pred))
        level_2 <- levels(factor(set[,fac]))
        col_1 <- col_vector[1:len_1]
        col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
        color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
        color_2 <- as.character(factor(set[,fac], levels = level_2, labels = col_2))
        p <- plot(set[, c(input$naivebayesplotx, input$naivebayesploty)],
                  main = 'Naive bayes (Test set)',
                  xlab = input$naivebayesplotx, ylab = input$naivebayesploty,
                  xlim = range(X1), ylim = range(X2))
        contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
        points(grid_set, pch = '.', col = color_1)
        points(set, pch = 21, bg = color_2)
        legend(x = "topright",
               inset = c(-.09, .4),
               title = "Classes",
               legend = level_2,
               col = col_2,
               bg = rgb(1, 0, 0, alpha = 0.15),
               box.lty = 0,
               pch = 19,
               xpd = TRUE)
        p
      }
      else if(numeric_only > 3){
        par(mar=c(4,4,5,6))
        pred_train <- training_set[fac]
        pred_test <- test_set[fac]
        pca <- princomp(x = training_set[-fac],  cor = TRUE, scores = TRUE, covmat = NULL)
        varpca <- pca$sdev^2 / sum(pca$sdev^2)
        training_set <- predict(pca, training_set)
        training_set <- data.frame(training_set[, 1:2])
        training_set <- cbind(training_set , pred_train)
        colnames(training_set) <- c("PC1", "PC2", "response")
        training_set$response <- factor(training_set$response)
        test_set <- predict(pca, test_set)
        test_set <- data.frame(test_set[, 1:2])
        test_set <- cbind(test_set, pred_test)
        colnames(test_set) <- c("PC1", "PC2", "response")
        test_set$response <- factor(test_set$response)
        set <- test_set
        X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
        X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.05)
        grid_set <- expand.grid(X1, X2)
        colnames(grid_set) <- c("PC1", "PC2")
        classifier <- naiveBayes(response ~ .,
                                 data = training_set,
                                 laplace = input$naivebayeslaplace,
                                 na.action = na.omit)
        y_pred <- predict(classifier,
                          newdata = grid_set,
                          threshold = input$naivebayesthreshold,
                          eps = input$naivebayeseps)
        qual_colals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
        col_vector <- unlist(mapply(brewer.pal, qual_colals$maxcolors, rownames(qual_colals)))
        col_vector <- col_vector[c(-3, -7, -8, -12, -14,-17,-24,-40,-41,-44,-45)]
        len_1 <- length(levels(factor(y_pred)))
        len_2 <- length(levels(factor(set[,3])))
        level_1 <- levels(factor(y_pred))
        level_2 <- levels(factor(set[,3]))
        col_1 <- col_vector[1:len_1]
        col_2 <- col_vector[(len_1 + 1):(len_1 + len_2)]
        color_1 <- as.character(factor(y_pred, levels = level_1, labels = col_1))
        color_2 <- as.character(factor(set[,3], levels = level_2, labels = col_2))
        p <- plot(set[, -3],
                  main = 'Naive bayes (Test set)',
                  xlab = sprintf("PC1 (%2.2f%%)", varpca[1]*100), ylab = sprintf("PC2 (%2.2f%%)", varpca[2]*100),
                  xlim = range(X1), ylim = range(X2))
        contour(X1, X2, matrix(as.numeric(y_pred), length(X1), length(X2)), add = TRUE)
        points(grid_set, pch = '.', col = color_1)
        points(set, pch = 21, bg = color_2)
        legend(x = "topright",
               inset = c(-.09, .4),
               title = "Classes",
               legend = level_2,
               col = col_2,
               bg = rgb(1, 0, 0, alpha = 0.15),
               box.lty = 0,
               pch = 19,
               xpd = TRUE)
        p
      }
    }
  })
  ############################################## Naive bayes prediction on new data
  #### erorr checking
  # error checking
  output$uploaded_naivebayesdata_prediction_hide_tabpanel <- reactive({
    error_catghing_naivebayes_prediction <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_naivebayes_prediction)
      checknull <- NCOL(uploaded_naivebayes_data_prediction()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_naivebayes_prediction(input$uploaded_naivebayes_prediction$name, input$datanaivebayesfileformat_prediction)
  })
  outputOptions(output, 'uploaded_naivebayesdata_prediction_hide_tabpanel', suspendWhenHidden=FALSE)
  ############### uploaded prediction table
  uploaded_naivebayes_data_prediction <- eventReactive(input$uploaded_naivebayes_prediction,{
    if(!is.null(input$uploaded_naivebayes_prediction)){
      if(input$datanaivebayesfileformat == "," && input$naivebayesheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_naivebayes_prediction$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$datanaivebayesfileformat == "," && input$naivebayesheader == "No") {
        rawdata <- try(read_csv(input$uploaded_naivebayes_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$datanaivebayesfileformat == "\t" && input$naivebayesheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_naivebayes$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$datanaivebayesfileformat == "\t" && input$naivebayesheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_naivebayes_prediction$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ############### new table for prediction choices
  observeEvent(uploaded_naivebayes_data_prediction(), {
    if(!is.null(uploaded_naivebayes_data_prediction())){
      response_var <- names(uploaded_naivebayes_data_prediction())
      updateSelectInput(
        session,
        "naivebayes_prediction_response",
        choices = response_var)
    }
  })
  observeEvent(uploaded_naivebayes_data_prediction(), {
    if(!is.null(uploaded_naivebayes_data_prediction())){
      dftype <- sapply(uploaded_naivebayes_data_prediction(), class)
      isnumeric <- grep("numeric", dftype)
      predictor_vars <- names(uploaded_naivebayes_data_prediction())[isnumeric]
      updatePickerInput(
        session,
        "naivebaye_prediction_num",
        choices = predictor_vars)
    }
  })
  # categorical 
  observeEvent(uploaded_naivebayes_data_prediction(), {
    if(!is.null(uploaded_naivebayes_data_prediction())){
      dftype <- sapply(uploaded_naivebayes_data_prediction(), class)
      ischar <- grep("character|logical", dftype)
      predictor_vars <- names(uploaded_naivebayes_data_prediction())[ischar]
      updatePickerInput(
        session,
        "naivebayes_prediction_cat",
        choices = predictor_vars)
    }
  })
  ######################################## prediction result
  naivebayes_analysis_prediction <- reactive({
    if(!is.null(uploaded_naivebayes_data()) && input$naivebayesprepross_prediction_result){
      if(is.null(input$naivebayesresponse) | is.null(input$naivebayespredictorsnum) && is.null(input$naivebayespredictorscat)){
        return(NULL)
      }
      ############ for naive prediction
      uploaded_naivebayes_data_prediction <- isolate(uploaded_naivebayes_data_prediction())
      predictorsnum_prediction <- input$naivebaye_prediction_num
      predictorscat_prediction <- input$naivebayes_prediction_cat
      predictorscat_prediction_length <- length(predictorscat_prediction)
      naivebayes_df_prediction <- data.frame(uploaded_naivebayes_data_prediction[, c(predictorsnum_prediction, predictorscat_prediction)])
      if(predictorscat_prediction_length >= 1){
        for(i in predictorscat_prediction){
          naivebayes_df_prediction[, i] <- factor(naivebayes_df_prediction[, i])
        }
        num <- which(names(naivebayes_df_prediction) %in% predictorscat_prediction)
        naivebayes_df_prediction[-num] <- scale(naivebayes_df_prediction[-num])
      }
      ##########################################
      ###### for naive analysis
      uploaded_naivebayes_data <- isolate(uploaded_naivebayes_data())
      response <- input$naivebayesresponse
      predictorsnum <- input$naivebayespredictorsnum
      predictorscat <- input$naivebayespredictorscat
      predictorscat_length <- length(predictorscat)
      naivebayes_df <- data.frame(uploaded_naivebayes_data[, c(predictorsnum, predictorscat, response)])
      fac <- length(naivebayes_df)
      naivebayes_df[,fac] <- factor(naivebayes_df[,fac])
      ### create factor for categorical predictor 
      if(predictorscat_length >= 1){
        for(i in predictorscat){
          naivebayes_df[, i] <- factor(naivebayes_df[, i])
        }
      }
      
      ############### check both file to be the almost the same
      similar_name <- which(names(naivebayes_df) %in% names(naivebayes_df_prediction))
      if(length(similar_name) < 1){
        return(NULL)
      }
      ############################################################
      set.seed(123)
      split <- sample.split(naivebayes_df[,fac], SplitRatio = input$naivebayesratiosplit)
      training_set <- subset(naivebayes_df, split == TRUE)
      test_set <- subset(naivebayes_df, split == FALSE)
      # excluding categorical predictors from scaling
      if(predictorscat_length >= 1){
        num <- which(names(naivebayes_df) %in% predictorscat)
        training_set[c(-fac, -num)] <- scale(training_set[c(-fac, -num)])
        test_set[c(-fac, -num)] <- scale(test_set[c(-fac, -num)])
      } else{
        training_set[-fac] <- scale(training_set[-fac])
        test_set[-fac] <- scale(test_set[-fac])
      }
      if(input$naivebayeskfoldcv > 0) {
        folds <- createFolds(naivebayes_df[,fac] , k = input$naivebayeskfoldcv)
        cv <- lapply(folds, function(x){
          training_fold <- training_set[-x, ]
          test_fold <- test_set[-x, ]
          formula <- as.formula(paste0(names(naivebayes_df)[fac], " ~ ."))
          classifier <- naiveBayes(formula,
                                   data = training_fold,
                                   laplace = input$naivebayeslaplace,
                                   na.action = na.omit)
          y_pred <- predict(classifier,
                            newdata = naivebayes_df_prediction,
                            threshold = input$naivebayesthreshold,
                            eps = input$naivebayeseps)
          return(y_pred)
        })
        cv
        Mode <- function(x) {
          ux <- unique(x)
          return(ux[which.max(tabulate(match(x, ux)))])
        }
        folds_predicted <- data.frame(Mode(cv)[[1]])
        names(folds_predicted) <- response
        folds_predicted
      } else{
        formula <- as.formula(paste0(names(naivebayes_df)[fac], " ~ ."))
        classifier <- naiveBayes(formula,
                                 data = training_set,
                                 laplace = input$naivebayeslaplace,
                                 na.action = na.omit)
        y_pred <- predict(classifier,
                          newdata = naivebayes_df_prediction,
                          threshold = input$naivebayesthreshold,
                          eps = input$naivebayeseps)
        y_pred
       }
    }
    
  })
  
  
  ############ table of actual value for new data
  output$naivebayes_actual_response <- renderDT({
    if(!is.null(input$naivebayes_prediction_response) && input$naivebayesprepross_prediction_result) {
      uploaded_naivebayes_data_prediction <- isolate(uploaded_naivebayes_data_prediction())
      response_prediction <- input$naivebayes_prediction_response
      naivebayes_df_prediction <- data.frame(uploaded_naivebayes_data_prediction[, response_prediction])
      naivebayes_df_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 10),
          rownames = T)
    }
  })
  
  ############ table of predicted value for new data
  output$naivebayes_predicted_response <- renderDT({
    req(naivebayes_analysis_prediction())
    if(input$naivebayesprepross_prediction_result) {
      naivebayes_analysis_prediction <- isolate(naivebayes_analysis_prediction())
      naivebayes_analysis_prediction %>%
        datatable(
          options = list(dom = "tp", pageLength = 10),
          rownames = T)
    }
  })
  
  ### Download naive bayes prediction table 
  output$download_prediction_naivebayes_table <- renderUI({
    if(input$naivebayesprepross_prediction_result) {
      downloadButton('naivebayesprediction_table', 'Download prediction table')
    }
  })
  
  output$naivebayesprediction_table <- downloadHandler(
    
    filename = function(){
      paste0(Sys.Date(),"_prediction_table.csv")
    },
    content = function(file){
      naivebayes_analysis_prediction() %>%
        write_csv(file)
    }
  )
  ################################################################# clustering #######################################
  #### erorr checking
  output$uploaded_clustering_hide_tabpanel <- reactive({
    error_catghing_clustering <- function(path, sep){
      ext <- str_extract(path, '(?=.)[a-z]{3}$')
      if(sep == ","){
        sepa <- "csv"
      } else {
        sepa <- "tsv"
      }
      table <- !is.null(input$uploaded_clustering)
      checknull <- NCOL(uploaded_clustering_data()) > 1
      if(table && checknull && sepa == ext){
        TRUE
      } else {
        FALSE
      }
    }
    error_catghing_clustering(input$uploaded_clustering$name, input$dataclusteringfileformat)
  })
  outputOptions(output, 'uploaded_clustering_hide_tabpanel', suspendWhenHidden=FALSE)
  ################################### uploaded clustering table ######################################################
  uploaded_clustering_data <- eventReactive(input$uploaded_clustering, {
    if(!is.null(input$uploaded_clustering)){
      if(input$dataclusteringfileformat == "," && input$clusteringheader == "Yes"){
        rawdata <- try(read_csv(input$uploaded_clustering$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      } 
      else if(input$dataclusteringfileformat == "," && input$clusteringheader == "No") {
        rawdata <- try(read_csv(input$uploaded_clustering$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
      else if((input$dataclusteringfileformat == "\t" && input$clusteringheader == "Yes")){
        rawdata <- try(read_tsv(input$uploaded_clustering$datapath,
                                col_names = T),
                       silent = T)
        rawdata
      }
      else if((input$dataclusteringfileformat == "\t" && input$clusteringheader == "No")){
        rawdata <- try(read_tsv(input$uploaded_clustering$datapath,
                                col_names = F),
                       silent = T)
        rawdata
      }
    }
  })
  ########################################### clustering table
  output$clusteringdatashow <- renderDT({
    if(!is.null(input$uploaded_clustering) && input$clusteringprepross) {
      uploaded_clustering_table <- uploaded_clustering_data() %>%
        datatable(
          fillContainer = F,
          options = list(scrollX = TRUE),
          extensions = "AutoFill",
          style = "auto",
          rownames = FALSE)
      uploaded_clustering_table
    }
  })
  ###### select numeric variables 
  observeEvent(uploaded_clustering_data(), {
    if(!is.null(uploaded_clustering_data())){
      dftype <- sapply(uploaded_clustering_data(), class)
      isnumeric <- grep("numeric", dftype)
      clustering_vars <- names(uploaded_clustering_data())[isnumeric]
      updatePickerInput(
        session,
        "clusteringvariables",
        choices = clustering_vars)
    }
  })
  ################ CLUSTERING analysis
  clustering_analysis <- reactive({
    if(!is.null(uploaded_clustering_data())){
      uploaded_clustering_data <- isolate(uploaded_clustering_data())
      vars <- input$clusteringvariables
      var_name <- which(names(uploaded_clustering_data) %in% vars)
      if(length(var_name) > 0){
        clustering_df <- data.frame(uploaded_clustering_data[, vars])
        clustering_df <- scale(clustering_df)
        clustering_df <- data.frame(clustering_df)
        clustering_df
      }
    }
  })
  ########################## hide elbow
  output$hide_elbow_plot <- reactive({
    hideelbow <- function(){
      if(input$clusteringtype == "K-Means" && !is.null(clustering_analysis())){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    hideelbow()
  })
  outputOptions(output, 'hide_elbow_plot', suspendWhenHidden=FALSE)
  #### hide hierarchy 
  output$hide_hierarchy_plot <- reactive({
    hidehierarchy <- function(){
      if(input$clusteringtype == "Hierarchical" && !is.null(clustering_analysis())){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    hidehierarchy()
  })
  outputOptions(output, 'hide_hierarchy_plot', suspendWhenHidden=FALSE)
  
  
  ############################################# Elbow plot #################################
   output$clustering_elbow <- renderPlot({
     if(input$clusteringtype == "K-Means"){
     if(!is.null(clustering_analysis()) && input$clustering_elbomethod){
       clustering_analysis <- isolate(clustering_analysis())
       set.seed(123)
       par(mar=c(3,3,2,2))
       wcss <- vector()
       for(i in 1:10){
         wcss[i] <- sum(kmeans(clustering_analysis, i)$withinss)
       }
       p <- ggplot() + geom_line(aes(x = 1:10, y = wcss), color = "#DC7633", size = 1) +
         geom_point(aes(x = 1:10, y = wcss), color = "#7E5109") +
         xlab('Number of clusters') + ylab('WCSS') + scale_x_continuous(breaks = seq(1, 10, 1))+
         theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                 axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                                 axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                                 axis.text.y = element_text(family = "Times",colour = "black"),
                                 plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                                 axis.title.y = element_text(family = "Times", size = rel(1.4)),
                                 axis.title.x = element_text(family = "Times", size = rel(1.4))) +
         labs(subtitle = 'The Elbow Method')
       p
     }
     }
   })

  ############################# clustering plot K-Means ##################################
  output$clustering_kmean_plot <- renderPlot({
    if(input$clusteringtype == "K-Means"){
      if(!is.null(uploaded_clustering_data()) && input$clustering_start){
        vars <- input$clusteringvariables
        var_name <- which(names(uploaded_clustering_data()) %in% vars)
        if(length(var_name) > 0){
          clustering_df <- data.frame(uploaded_clustering_data()[, vars])
          clustering_df <- data.frame(clustering_df)
        }
        if(length(clustering_df) > 2){
          set.seed(123)
          pca <- princomp(x = clustering_df,  cor = TRUE, scores = TRUE, covmat = NULL)
          clustering_df <- predict(pca, clustering_df)
          clustering_df <- data.frame(clustering_df[,c(1,2)])
          cls <- kmeans(x = clustering_df, centers = input$clustering_clusternumber, nstart = 25)
          clustering_df$Clusters <- as.character(cls$cluster)
          varpca <- data.frame(pca$sdev^2 / sum(pca$sdev^2))
          colnames(varpca) <- 'variation'
          varpca <- varpca[order(varpca$variation, decreasing = T),]
          my_pal <- c("#1B9E77", "#6E2C00","#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#9A7D0A")
          p <- ggplot(aes(x = Comp.1, y = Comp.2, color = Clusters, fill = Clusters), data =  clustering_df) +
            geom_point(size = 4, shape = 21) +
            xlab(sprintf("PC1 (%2.2f%%)", varpca[1]*100)) + ylab(sprintf("PC2 (%2.2f%%)", varpca[2]*100)) +
            scale_color_manual(values=c(my_pal)) + stat_ellipse() +
            scale_fill_manual(values=c(paste(my_pal, "66", sep = ""))) +
            theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                    axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                                    axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                                    axis.text.y = element_text(family = "Times",colour = "black"),
                                    plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                                    axis.title.y = element_text(family = "Times", size = rel(1.4)),
                                    axis.title.x = element_text(family = "Times", size = rel(1.4))) +
            labs(subtitle = 'Cluster plot')
          p
        }
        else if(length(clustering_df) == 2){
          set.seed(123)
          cls <- kmeans(x = clustering_df, centers = input$clustering_clusternumber, nstart = 25)
          clustering_df$Clusters <- as.character(cls$cluster)
          my_pal <- c("#1B9E77", "#6E2C00","#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#9A7D0A")
          p <- ggplot(aes(x = clustering_df[,1], y = clustering_df[,2], color = Clusters, fill = Clusters), data =  clustering_df) + 
            geom_point(size = 4, shape = 21) +
            xlab(names(clustering_df)[1]) + ylab(names(clustering_df)[2]) +
            scale_color_manual(values=c(my_pal)) + stat_ellipse() +
            scale_fill_manual(values=c(paste(my_pal, "66", sep = ""))) +
            theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                                    axis.text = element_text(family = "Times",size = 13 , colour = "black"),
                                    axis.text.x = element_text(family = "Times",colour = "black", size = 13),
                                    axis.text.y = element_text(family = "Times",colour = "black"),
                                    plot.subtitle = element_text(family = "Times",size = 20, colour = "black", hjust = 0.5),
                                    axis.title.y = element_text(family = "Times", size = rel(1.4)),
                                    axis.title.x = element_text(family = "Times", size = rel(1.4))) +
            labs(subtitle = 'Cluster plot')
          p
        }
    
        }
      }
      })
  ############################# clustering plot Hierarchical ##################################
  output$clustering_hierarchical_plot <- renderPlot({
    if(input$clusteringtype == "Hierarchical"){
      if(!is.null(uploaded_clustering_data()) && input$clustering_hierarchical_plot_show){
        uploaded_clustering_data <- isolate(uploaded_clustering_data())
        uploaded_clustering_data <- data.frame(uploaded_clustering_data)
        vars <- input$clusteringvariables
        var_name <- which(names(uploaded_clustering_data) %in% vars)
        if(length(var_name) > 0){
          clustering_df <- data.frame(uploaded_clustering_data[, vars])
          clustering_df <- scale(clustering_df)
          if(input$clustering_rowname == "TRUE"){
          rownames(clustering_df) <- uploaded_clustering_data[,1]
        }
        }
          set.seed(123)
          res.dist <- dist(clustering_df, method = input$clusteringdistancemethod)
          res.hc <- hclust(d = res.dist, method = input$clusteringmethod)
          my_pal <- c("#1B9E77", "#6E2C00","#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#9A7D0A")
          if(input$dendrogramposition == "Vertical"){
            horiz <- FALSE
          } else{
            horiz <- TRUE
          }
          if(input$clustering_dendrogramborder == "FALSE"){
            rect <- FALSE 
          } else{
            rect <- TRUE
          }
          fviz_dend(res.hc, k = input$clustering_numberofcut,
                    cex = 0.8,
                    k_colors = my_pal[1:input$clustering_numberofcut],
                    type = input$dendrogramtype,
                    phylo_layout = input$dendrogram_phylolayout,
                    color_labels_by_k = TRUE,
                    rect = rect,
                    rect_border = "gray",
                    repel = TRUE,
                    horiz = horiz,
                    main = "",
                    ggtheme = theme_classic()
          )

        }
    }
    
  })
  
})
