
library(shiny)
library(partykit)
library(ggplot2)
library(ggparty)
library(quantmod)
library(dplyr)
library(reshape2)
library(plyr)
library(dendsort)
library(DT)
library(strucchange)
library(data.table)
library(tidyverse)
library(circlize)
library(seriation)
library(devtools)
#install.packages("http://cran.rstudio.com/src/contrib/Archive/rjson/rjson_0.2.13.tar.gz", repos=NULL, type="source")
devtools::install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)
options(shiny.maxRequestSize=30*1024^2) 
# Define server logic 
shinyServer(function(input, output, session) {
  df_upload <- reactive({
    inFile <-  input$target_upload
    if (is.null(inFile)){
      return(NULL)
    }
    ## reading file & deleting zero series
    pmt <- read.csv(inFile$datapath, header = TRUE) %>% 
      as.data.frame()
    return(pmt)
  })
 
  observe({
    data_set <- df_upload()
    data_set1 <- data_set[,!colnames(data_set) %in% c('Series','cat.col',	'Date', 'latitude', 'longitude')]
  opts <- names(data_set1)

  # Update the checkboxGroupInput
  updateCheckboxGroupInput(
    session, "SplitVariables1", choices = opts, selected = opts)
  })

  observe({
    data_set <- df_upload()
    data_set1 <- data_set[,!colnames(data_set) %in% c('Series','cat.col',	'Date', 'latitude', 'longitude')]
    opts <- names(data_set1) 
    # Update the checkboxGroupInput
    updateCheckboxGroupInput(
      session, "SplitVariables2", choices = opts, selected = opts)
  })

  ## Series structures
  values <- reactiveValues()
  observe({
    if (is.null(df_upload()))
      return(NULL)
    values$nrows <- nrow(df_upload())
    values$nseries <- length(unique(df_upload()$cat.col))
    values$serieslength <-  nrow(df_upload())/length(unique(df_upload()$cat.col))
  })
  
  df_change <- reactive ({
    if (is.null(df_upload()))
      return(NULL)
    dattr <- df_upload() 
    nrowst <- nrow(dattr)
    frequency <- as.integer(input$Frequency)
    nseriest <- length(unique(dattr$cat.col))
    serieslengtht <-  nrow(dattr)/length(unique(dattr$cat.col))
    dattr <- dplyr::group_by(dattr, Date) %>%
      plyr::mutate(Series = ts(dattr$Series, frequency = frequency)) %>%
      plyr::mutate(season = factor(cycle(Series)), trend = rep(1:serieslengtht, nseriest))%>%
      as.data.frame()
    
    ## Normalizing the series 
    dattr <- dattr %>%
      plyr::ddply("cat.col", transform, Series.std = scale(Series)) %>%
      as.data.frame()

    ## lags added
    category_sort <- sort(unique(dattr$cat.col))
    lag_making <- list()
    for (i in seq_along(category_sort))
      lag_making[[i]] <-
      quantmod::Lag(dattr$Series.std[dattr$cat.col == category_sort[i]], 1:frequency)
    lag_making <- do.call(rbind.data.frame, lag_making)
    dattr <- dattr[order(dattr$cat.col), ]
    dattr <- cbind.data.frame(dattr, lag_making)
    return(dattr)
  }) 
#####################################   
## First set of results (first tab)
####################################
fit1 <- reactive({
  if (is.null(df_change()))
    return(NULL)
    req(input$SplitVariables1)
    frequency <- as.integer(input$Frequency)
    var <- input$SplitVariables1
    var1 <- as.vector(unlist(var))
    form <- "Series.std ~ trend +  season"
    for (i in 1:frequency)
      form <- paste(form , " + ", paste("Lag", i, sep = '.'))
    form <- paste(form , '|')
    for (i in 1:(length(var1)-1))
      form <- paste0(form, var1[i], " + ")
    form <- paste0(form, var1[length(var1)])
    formula <- as.formula(form)

    ## category columns
    df1 <- df_change()
    colsfac <- df1 %>% 
      select(where(is.character)) %>%
      select(one_of(var))
    df1 <- df1 %>% mutate_at(c(colnames(colsfac)), list(~factor(.)))
    
    ### defining fit function
    linear <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
        glm(y ~ 0 + x, family = gaussian, start = start, ...)
    }
    depth <- input$Depth1
    ## running MOB tree
    MOBtree <- mob( formula, data = df1, fit = linear,  na.action = na.exclude, control =
                      mob_control(prune = input$Prune1,  maxdepth = depth, alpha = 0.01))

      })
  ## title for MSE&AIC
  output$titleMSEAIC1 = renderText({})
  ## MSE and AIC for different MOB tree depth
   output$MSEAIC1 <- renderTable({
     if (is.null(df_change()))
       return(NULL)
    x <- na.omit(df_change())$Series.std
    tab <- rbind("MSE" = round(mean((x - predict(fit1(), type = "response")) ^ 2), digits = 4),
    "AIC" = round(AIC(fit1()), digits = 0))
    cbind(c('MSE', 'AIC'), tab)
  }, colnames = FALSE, align = "l")
   ## title for first heatmap
  output$titleHeatmap11 = renderText({})
 ## MOB tree and heatmap plotting
 output$MOBTree11 <- renderPlot({
   if (is.null(df_change()))
     return(NULL)
  split.pmt <- split(na.omit(df_change()), predict(fit1(), type = "node"))
  names(split.pmt) <- c(length(split.pmt): 1)
  split.pmt <- split.pmt[sort(names(split.pmt))]
     new.split.pmt <- list()
     for(i in 1: length(split.pmt)){
       unique.name <- unique(split.pmt[[i]]$cat.col)
       new.split.pmt[[i]] <- df_change()[df_change()$cat.col %in% unique.name,]
       new.split.pmt[[i]]$cluster <-  i
     }
     new_matrixc <- NULL
     for(i in 1:length(new.split.pmt)){
       new_matrix <- as.data.frame(t(matrix(new.split.pmt[[i]]$Series.std, nrow = (values$serieslength), ncol = length(unique(new.split.pmt[[i]]$cat.col)))))
       colnames(new_matrix) <- new.split.pmt[[i]][1:(values$serieslength),]$Date
       new_matrix <- cbind.data.frame('#Series' = paste0('S', nrow(new_matrix), sep = ''), new_matrix)
       new_matrix <- cbind.data.frame('ID' = paste0('C', i, sep = ''), new_matrix)
       new_matrix <- cbind.data.frame('latitude' = unique(new.split.pmt[[i]]$latitude), new_matrix)
       new_matrix <- cbind.data.frame('longitude' = unique(new.split.pmt[[i]]$longitude), new_matrix)
       new_matrixc <- bind_rows(new_matrixc, new_matrix)
     }
     #col_fun = colorRamp2(c(-2, 0, 2), c("darkgreen", "white", "darkred"))
     col_fun = colorRamp2(c(-2, 0, 2), c(input$colorlow1, "white", input$colorhigh1))
     col_fun(seq(-3, 3))
     
     ## Two option for reordering 1- seriation 2- hclust
     o1 = seriation::seriate(dist(as.matrix(new_matrixc[,-c(1:4)])), method = "HC")
    # heatmap
    frequency <- as.integer(input$Frequency)
    if(frequency == 7){
      Weekday = c(as.character(weekdays(lubridate::ymd(colnames(new_matrixc[,-c(1:4)])))))
      column_ha = HeatmapAnnotation(Weekday = Weekday, 
                                    annotation_legend_param = list(nrow = 2),
                                    col = list(Weekday = c("Monday"="#8DD3C7", "Tuesday"="#FFFFB3", 'Wednesday'="#BEBADA" , 'Thursday'="#FB8072",
                                                           'Friday'="#80B1D3", 'Saturday'="#FDB462", 'Sunday'="#B3DE69" )))
      
      Month = c(as.character(lubridate::month(lubridate::ymd(colnames(new_matrixc[,-c(1:2)])), label = TRUE, abbr = FALSE)))
      column_ha2 = HeatmapAnnotation(Month = Month, 
                                     annotation_legend_param = list(nrow = 2),
                                     col = list(Month = c("January"="#8DD3C7", "February"="#FFFFB3", 'March'="#BEBADA" , 'April'="#FB8072",
                                                          'May'="#80B1D3", 'June'="#FDB462", 'July'="#B3DE69", 'August'="#FCCDE5",
                                                          'September'="#D9D9D9", 'October'="#BC80BD", 'November'="#CCEBC5", 'December'="#FFED6F" )))
      Year = c(lubridate::year(lubridate::ymd(colnames(new_matrixc[,-c(1:4)]))))
      column_ha3 = HeatmapAnnotation(Year = Year, 
                                     annotation_legend_param = list(legend_direction = "horizontal", at = c(min(Year), median(Year), max(Year)), 
                                                                    labels = c(min(Year), median(Year), max(Year))),
                                     col = list(Year = colorRamp2(c(min(Year), median(Year), max(Year)), c("cornsilk", "blue",  "orange"))))
    }
    if(frequency == 12){
      Month = c(as.character(lubridate::month(lubridate::ym(colnames(new_matrixc[,-c(1:4)])), label = TRUE, abbr = FALSE)))
      column_ha = HeatmapAnnotation(Month = Month, 
                                    #annotation_legend_param = list(labels = unique(Month), nrow = 2),
                                    annotation_legend_param = list(nrow = 2),
                                    col = list(Month = c("January"="#8DD3C7", "February"="#FFFFB3", 'March'="#BEBADA" , 'April'="#FB8072",
                                                         'May'="#80B1D3", 'June'="#FDB462", 'July'="#B3DE69", 'August'="#FCCDE5",
                                                         'September'="#D9D9D9", 'October'="#BC80BD", 'November'="#CCEBC5", 'December'="#FFED6F" )))
      
      Year = c(lubridate::year(lubridate::ym(colnames(new_matrixc[,-c(1:4)]))))
      column_ha2 = HeatmapAnnotation(Year = Year, 
                                     annotation_legend_param = list(legend_direction = "horizontal", at = c(min(Year), median(Year), max(Year)), 
                                                                    labels = c(min(Year), median(Year), max(Year))),
                                     col = list(Year = colorRamp2(c(min(Year), median(Year), max(Year)), c("cornsilk", "blue",  "orange"))))
    }
    Latitude <- c(new_matrixc[,1])
    row_ha1 = rowAnnotation(Latitude = Latitude, 
                            annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Latitude)), round(mean(Latitude))
                                                                                                   , round(max(Latitude))), 
                                                           labels = c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude)))),
                            col = list(Latitude = colorRamp2(c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude))), c("cornsilk", "blue",  "orange"))))
    Longitude <- c(new_matrixc[,2])
    row_ha2 = rowAnnotation(Longitude = Longitude, 
                            annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), 
                                                           labels = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude)))),
                            col = list(Longitude = colorRamp2(c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), c("cornsilk", "blue",  "orange"))))
    if(frequency == 7){
      heatmap.plot <- Heatmap(as.matrix(new_matrixc[,-c(1:4)]),  #row_dend_reorder = FALSE,
                              row_order = get_order(o1), 
                              name = "Heatmap",  column_dend_reorder = FALSE, 
                              cluster_columns = FALSE, cluster_rows = FALSE, show_column_names = FALSE, row_split = new_matrixc[, c(3:4)], 
                              row_title_gp = gpar(fill = c("#B3DE69"),col = c("black"), font = 2), 
                              bottom_annotation = c(column_ha, column_ha2, column_ha3),
                              left_annotation = c(row_ha1, row_ha2),
                              col = col_fun, border = TRUE,
                              na_col = "gray27", 
                              heatmap_legend_param = list(
                                legend_direction = "horizontal"
                              ))
    }
    if(frequency == 12){
      heatmap.plot <- Heatmap(as.matrix(new_matrixc[,-c(1:4)]),  #row_dend_reorder = FALSE,
                              row_order = get_order(o1), 
                              name = "Heatmap",  column_dend_reorder = FALSE, 
                              cluster_columns = FALSE, cluster_rows = FALSE, show_column_names = FALSE, row_split = new_matrixc[, c(3:4)], 
                              row_title_gp = gpar(fill = c("#B3DE69"),col = c("black"), font = 2), 
                              bottom_annotation = c(column_ha, column_ha2),
                              left_annotation = c(row_ha1, row_ha2),
                              col = col_fun, border = TRUE,
                              na_col = "gray27", 
                              heatmap_legend_param = list(
                                legend_direction = "horizontal"
                              ))
    }
    
   
    
    if ( input$Depth1 ==1 ){
      plot(heatmap.plot)
    }
    else{
      ## MOB tree
      tree.plot <- ggparty(fit1()) +
        geom_edge() +
        coord_flip() +
        geom_edge_label( size = 6, fontface = "bold", nudge_y =  -0.005,  label.padding = unit(1, "lines"), max_length = 10 ) +
        geom_node_label(
          line_list = list(aes(label = splitvar),
                           aes(label = paste("p =",
                                             formatC(p.value,
                                                     format = "f",
                                                     digits = 2))),
                           aes(label = "")
          ),
          line_gpar = list(list(size = 20),
                           list(size = 15),
                           list(size = 2)
          ),
          # only inner nodes
          ids = "inner") +
        geom_node_info()
      ## printing both plots
      grob = grid.grabExpr(draw(heatmap.plot, heatmap_legend_side = "bottom",  annotation_legend_side = "bottom")) 
      grid.newpage()
      pushViewport(viewport(x = 0.25, y = 0.5, width = 0.5, height = 0.9))
      grid.draw(grob)
      pushViewport(viewport(x = 1.45, y = 0.6, width = 1, height = 0.8))
      print(tree.plot, newpage = FALSE)
    }
  } )
   output$titleHeatmap21 = renderText({})
   ## heatmap by day of week
   output$MOBTree21 <- renderPlot({
     if (is.null(df_change()))
       return(NULL)
     split.pmt <- split(na.omit(df_change()), predict(fit1(), type = "node"))
     names(split.pmt) <- c(length(split.pmt): 1)
     split.pmt <- split.pmt[sort(names(split.pmt))]
     new.split.pmt <- list()
     for(i in 1: length(split.pmt)){
       unique.name <- unique(split.pmt[[i]]$cat.col)
       new.split.pmt[[i]] <- df_change()[df_change()$cat.col %in% unique.name,]
       new.split.pmt[[i]]$cluster <-  i
     }
     frequency <- as.integer(input$Frequency)
     if(frequency == 7){
     order.times <- order.times <- function(df){
       test <- df %>%
         mutate('date' = lubridate::ymd(as.Date(Date)))%>%
         mutate('times' = months(date)) %>%
         mutate('times2' = weekdays(date)) %>%
         group_by(cat.col) %>%
         arrange(factor(times, levels = month.name)) 
       test3 <- df %>%
         mutate('date' = lubridate::ymd(as.Date(Date)))%>%
         mutate('times' = months(date)) %>%
         mutate('times2' = weekdays(date)) %>%
         group_by(cat.col) %>%
         arrange(factor(times2, levels = DescTools::day.name))
       test1 <- test %>%
         arrange(cat.col)
       
       split1 <- split(test1, test1$cat.col, drop = TRUE) 
       
       for(i in 1:length(split1)){
         split1[[i]] <- split1[[i]] %>%
           mutate('times2' = make.unique(times, sep = '_'))
       }
       
       test11 <- test3 %>%
         arrange(cat.col)
       
       split11 <- split(test11, test1$cat.col, drop = TRUE) 
       
       for(i in 1:length(split11)){
         split11[[i]] <- split11[[i]] %>%
           mutate('times' = make.unique(times, sep = '_'))
       }
       
       test2 <- do.call(rbind.data.frame, split1)
       test22 <- do.call(rbind.data.frame, split11)
       return(list(test22, test2))
     }
     ordar.times.pmt <- lapply(new.split.pmt, order.times)
     
     new_matrixc <- NULL
     for(i in 1:length(ordar.times.pmt)){
       new_matrix <- as.data.frame(t(matrix(ordar.times.pmt[[i]][[1]]$Series.std, nrow = (values$serieslength), ncol = length(unique(ordar.times.pmt[[i]][[1]]$cat.col)))))
       colnames(new_matrix) <- ordar.times.pmt[[i]][[1]][1:(values$serieslength),]$Date
       new_matrix <- cbind.data.frame('#Series' = paste0('S', nrow(new_matrix), sep = ''), new_matrix)
       new_matrix <- cbind.data.frame('ID' = paste0('C', i, sep = ''), new_matrix)
       new_matrix <- cbind.data.frame('latitude' = unique(new.split.pmt[[i]]$latitude), new_matrix)
       new_matrix <- cbind.data.frame('longitude' = unique(new.split.pmt[[i]]$longitude), new_matrix)
       new_matrixc <- bind_rows(new_matrixc, new_matrix)
     }

     }
  if(frequency == 12){
         order.times <- order.times <- function(df){
         test <- df %>%
           mutate('date' = lubridate::ym(Date))%>%
           mutate('times' = months(date)) %>%
           group_by(cat.col) %>%
           arrange(factor(times, levels = month.name)) 
       test1 <- test %>%
         arrange(cat.col)
       
       split1 <- split(test1, test1$cat.col, drop = TRUE) 
       
       for(i in 1:length(split1)){
         split1[[i]] <- split1[[i]] %>%
           mutate('times2' = make.unique(times, sep = '_'))
       }
       
       test2 <- do.call(rbind.data.frame, split1)
       return(test2)
         }
         ordar.times.pmt <- lapply(new.split.pmt, order.times)
         
         new_matrixc <- NULL
         for(i in 1:length(ordar.times.pmt)){
           new_matrix <- as.data.frame(t(matrix(ordar.times.pmt[[i]]$Series.std, nrow = (values$serieslength), ncol = length(unique(ordar.times.pmt[[i]]$cat.col)))))
           colnames(new_matrix) <- ordar.times.pmt[[i]][1:(values$serieslength),]$Date
           new_matrix <- cbind.data.frame('#Series' = paste0('S', nrow(new_matrix), sep = ''), new_matrix)
           new_matrix <- cbind.data.frame('ID' = paste0('C', i, sep = ''), new_matrix)
           new_matrix <- cbind.data.frame('latitude' = unique(new.split.pmt[[i]]$latitude), new_matrix)
           new_matrix <- cbind.data.frame('longitude' = unique(new.split.pmt[[i]]$longitude), new_matrix)
           new_matrixc <- bind_rows(new_matrixc, new_matrix)
         }
       }
     

     
     col_fun = colorRamp2(c(-2, 0, 2), c(input$colorlow1, "white", input$colorhigh1))
     col_fun(seq(-3, 3))
     
     ## Two option for reordering 1- seriation 2- hclust
    
     frequency <- as.integer(input$Frequency)
     if(frequency == 7){
       o1 = seriation:: seriate(dist(as.matrix(new_matrixc[,-c(1:4)])), method = "HC")
       Weekday = c(as.character(weekdays(lubridate::ymd(colnames(new_matrixc[,-c(1:4)])))))
       column_ha = HeatmapAnnotation(Weekday = Weekday, 
                                     annotation_legend_param = list(nrow = 2),
                                     col = list(Weekday = c("Monday"="#8DD3C7", "Tuesday"="#FFFFB3", 'Wednesday'="#BEBADA" , 'Thursday'="#FB8072",
                                                            'Friday'="#80B1D3", 'Saturday'="#FDB462", 'Sunday'="#B3DE69" )))
       
       Month = c(as.character(lubridate::month(lubridate::ymd(colnames(new_matrixc[,-c(1:4)])), label = TRUE, abbr = FALSE)))
       column_ha2 = HeatmapAnnotation(Month = Month, 
                                      annotation_legend_param = list(nrow = 2),
                                      col = list(Month = c("January"="#8DD3C7", "February"="#FFFFB3", 'March'="#BEBADA" , 'April'="#FB8072",
                                                           'May'="#80B1D3", 'June'="#FDB462", 'July'="#B3DE69", 'August'="#FCCDE5",
                                                           'September'="#D9D9D9", 'October'="#BC80BD", 'November'="#CCEBC5", 'December'="#FFED6F" )))
       Year = c(lubridate::year(lubridate::ymd(colnames(new_matrixc[,-c(1:4)]))))
       column_ha3 = HeatmapAnnotation(Year = Year, 
                                      annotation_legend_param = list(legend_direction = "horizontal", at = c(min(Year), median(Year), max(Year)), 
                                                                     labels = c(min(Year), median(Year), max(Year))),
                                      col = list(Year = colorRamp2(c(min(Year), median(Year), max(Year)), c("cornsilk", "blue",  "orange"))))
     }
     if(frequency == 12){
       o1 = seriation:: seriate(dist(as.matrix(new_matrixc[,-c(1:4)])), method = "HC")
     ## Annotations
     Month = c(as.character(lubridate::month(lubridate::ym(colnames(new_matrixc[,-c(1:4)])), label = TRUE, abbr = FALSE)))
     column_ha = HeatmapAnnotation(Month = Month, 
                                   #annotation_legend_param = list(labels = unique(Month), nrow = 2),
                                   annotation_legend_param = list(nrow = 2),
                                   col = list(Month = c("January"="#8DD3C7", "February"="#FFFFB3", 'March'="#BEBADA" , 'April'="#FB8072",
                                                        'May'="#80B1D3", 'June'="#FDB462", 'July'="#B3DE69", 'August'="#FCCDE5",
                                                        'September'="#D9D9D9", 'October'="#BC80BD", 'November'="#CCEBC5", 'December'="#FFED6F" )))
     
     Year = c(lubridate::year(lubridate::ym(colnames(new_matrixc[,-c(1:4)]))))
     column_ha2 = HeatmapAnnotation(Year = Year, 
                                    annotation_legend_param = list(legend_direction = "horizontal", at = c(min(Year), median(Year), max(Year)), 
                                                                   labels = c(min(Year), median(Year), max(Year))),
                                    col = list(Year = colorRamp2(c(min(Year), median(Year), max(Year)), c("cornsilk", "blue",  "orange"))))
     }
     
     Latitude <- c(new_matrixc[,1])
     row_ha1 = rowAnnotation(Latitude = Latitude, 
                             annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Latitude)), round(mean(Latitude))
                                                                                                    , round(max(Latitude))), 
                                                            labels = c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude)))),
                             col = list(Latitude = colorRamp2(c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude))), c("cornsilk", "blue",  "orange"))))
     Longitude <- c(new_matrixc[,2])
     row_ha2 = rowAnnotation(Longitude = Longitude, 
                             annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), 
                                                            labels = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude)))),
                             col = list(Longitude = colorRamp2(c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), c("cornsilk", "blue",  "orange"))))
     if(frequency == 7){
       heatmap.plot2 <-  Heatmap(as.matrix(new_matrixc[,-c(1:4)]),  #row_dend_reorder = FALSE,
               row_order = get_order(o1), 
               name = "Heatmap",  column_dend_reorder = FALSE, 
               cluster_columns = FALSE, cluster_rows = FALSE, show_column_names = FALSE, row_split = new_matrixc[, c(3:4)], 
               row_title_gp = gpar(fill = c("#B3DE69"),col = c("black"), font = 2), 
               bottom_annotation = c(column_ha, column_ha2, column_ha3),
               left_annotation = c(row_ha1, row_ha2),
               col = col_fun, border = TRUE,
               na_col = "gray27", 
               heatmap_legend_param = list(
                 legend_direction = "horizontal"
               ))
     }
     if(frequency == 12){
     heatmap.plot2 <- Heatmap(as.matrix(new_matrixc[,-c(1:4)]),  #row_dend_reorder = FALSE,
                              row_order = get_order(o1), 
                              name = "Heatmap",  column_dend_reorder = FALSE, 
                              cluster_columns = FALSE, cluster_rows = FALSE, show_column_names = FALSE, row_split = new_matrixc[, c(3:4)], 
                              row_title_gp = gpar(fill = c("#B3DE69"),col = c("black"), font = 2), 
                              bottom_annotation = c(column_ha, column_ha2),
                              left_annotation = c(row_ha1, row_ha2),
                              col = col_fun, border = TRUE,
                              na_col = "gray27", 
                              heatmap_legend_param = list(
                                legend_direction = "horizontal"
                              ))
     }
     
   if ( input$Depth1 ==1 ){
       plot(heatmap.plot2)
     }
     else{
       ## MOB tree
       tree.plot <- ggparty(fit1()) +
         geom_edge() +
         coord_flip() +
         geom_edge_label( size = 6, fontface = "bold", nudge_y =  -0.005,  label.padding = unit(1, "lines"), max_length = 10 ) +
         geom_node_label(
           line_list = list(aes(label = splitvar),
                            aes(label = paste("p =",
                                              formatC(p.value,
                                                      format = "f",
                                                      digits = 2))),
                            aes(label = "")
           ),
           line_gpar = list(list(size = 20),
                            list(size = 15),
                            list(size = 2)
           ),
           # only inner nodes
           ids = "inner") +
         geom_node_info()
       if(frequency == 7){
         # printing both plots
         grob1 = grid.grabExpr(draw(heatmap.plot2, heatmap_legend_side = "bottom",  annotation_legend_side = "bottom")) 
         grid.newpage()
         pushViewport(viewport(x = 0.25, y = 0.5, width = 0.5, height = 0.9))
         grid.draw(grob1)
         pushViewport(viewport(x = 1.45, y = 0.6, width = 1, height = 0.8))
         print(tree.plot, newpage = FALSE)
       }
       if(frequency == 12){
       # printing both plots
       grob = grid.grabExpr(draw(heatmap.plot2, heatmap_legend_side = "bottom",  annotation_legend_side = "bottom")) 
       grid.newpage()
       pushViewport(viewport(x = 0.25, y = 0.5, width = 0.5, height = 0.9))
       grid.draw(grob)
       pushViewport(viewport(x = 1.45, y = 0.6, width = 1, height = 0.8))
       print(tree.plot, newpage = FALSE)
       }
     }
   })
   
   output$titleHeatmap222 = renderText({})
   ## heatmap by day of week
   output$MOBTree222 <- renderPlot({
     if (is.null(df_change()))
       return(NULL)
     split.pmt <- split(na.omit(df_change()), predict(fit1(), type = "node"))
     names(split.pmt) <- c(length(split.pmt): 1)
     split.pmt <- split.pmt[sort(names(split.pmt))]
     new.split.pmt <- list()
     for(i in 1: length(split.pmt)){
       unique.name <- unique(split.pmt[[i]]$cat.col)
       new.split.pmt[[i]] <- df_change()[df_change()$cat.col %in% unique.name,]
       new.split.pmt[[i]]$cluster <-  i
     }
     frequency <- as.integer(input$Frequency)
     if(frequency == 7){
       order.times <- order.times <- function(df){
         test <- df %>%
           mutate('date' = lubridate::ymd(as.Date(Date)))%>%
           mutate('times' = months(date)) %>%
           mutate('times2' = weekdays(date)) %>%
           group_by(cat.col) %>%
           arrange(factor(times, levels = month.name)) 
         test3 <- df %>%
           mutate('date' = lubridate::ymd(as.Date(Date)))%>%
           mutate('times' = months(date)) %>%
           mutate('times2' = weekdays(date)) %>%
           group_by(cat.col) %>%
           arrange(factor(times2, levels = DescTools::day.name))
         test1 <- test %>%
           arrange(cat.col)
         
         split1 <- split(test1, test1$cat.col, drop = TRUE) 
         
         for(i in 1:length(split1)){
           split1[[i]] <- split1[[i]] %>%
             mutate('times2' = make.unique(times, sep = '_'))
         }
         
         test11 <- test3 %>%
           arrange(cat.col)
         
         split11 <- split(test11, test1$cat.col, drop = TRUE) 
         
         for(i in 1:length(split11)){
           split11[[i]] <- split11[[i]] %>%
             mutate('times' = make.unique(times, sep = '_'))
         }
         
         test2 <- do.call(rbind.data.frame, split1)
         test22 <- do.call(rbind.data.frame, split11)
         return(list(test22, test2))
       }
       ordar.times.pmt <- lapply(new.split.pmt, order.times)

       new_matrixc <- NULL
       for(i in 1:length(ordar.times.pmt)){
         new_matrix <- as.data.frame(t(matrix(ordar.times.pmt[[i]][[2]]$Series.std, nrow = (values$serieslength), ncol = length(unique(ordar.times.pmt[[i]][[2]]$cat.col)))))
         colnames(new_matrix) <- ordar.times.pmt[[i]][[2]][1:(values$serieslength),]$Date
         new_matrix <- cbind.data.frame('#Series' = paste0('S', nrow(new_matrix), sep = ''), new_matrix)
         new_matrix <- cbind.data.frame('ID' = paste0('C', i, sep = ''), new_matrix)
         new_matrix <- cbind.data.frame('latitude' = unique(new.split.pmt[[i]]$latitude), new_matrix)
         new_matrix <- cbind.data.frame('longitude' = unique(new.split.pmt[[i]]$longitude), new_matrix)
         new_matrixc <- bind_rows(new_matrixc, new_matrix)
       }
     }
     if(frequency == 12){
      return(NULL)
     }
     
     col_fun = colorRamp2(c(-2, 0, 2), c(input$colorlow1, "white", input$colorhigh1))
     col_fun(seq(-3, 3))
     
     ## Two option for reordering 1- seriation 2- hclust
     
     frequency <- as.integer(input$Frequency)
     if(frequency == 7){
       o1 = seriation:: seriate(dist(as.matrix(new_matrixc[,-c(1:4)])), method = "HC")
       Weekday = c(as.character(weekdays(lubridate::ymd(colnames(new_matrixc[,-c(1:4)])))))
       column_ha = HeatmapAnnotation(Weekday = Weekday, 
                                     annotation_legend_param = list(nrow = 2),
                                     col = list(Weekday = c("Monday"="#8DD3C7", "Tuesday"="#FFFFB3", 'Wednesday'="#BEBADA" , 'Thursday'="#FB8072",
                                                            'Friday'="#80B1D3", 'Saturday'="#FDB462", 'Sunday'="#B3DE69" )))
       
       Month = c(as.character(lubridate::month(lubridate::ymd(colnames(new_matrixc[,-c(1:4)])), label = TRUE, abbr = FALSE)))
       column_ha2 = HeatmapAnnotation(Month = Month, 
                                      annotation_legend_param = list(nrow = 2),
                                      col = list(Month = c("January"="#8DD3C7", "February"="#FFFFB3", 'March'="#BEBADA" , 'April'="#FB8072",
                                                           'May'="#80B1D3", 'June'="#FDB462", 'July'="#B3DE69", 'August'="#FCCDE5",
                                                           'September'="#D9D9D9", 'October'="#BC80BD", 'November'="#CCEBC5", 'December'="#FFED6F" )))
       Year = c(lubridate::year(lubridate::ymd(colnames(new_matrixc[,-c(1:4)]))))
       column_ha3 = HeatmapAnnotation(Year = Year, 
                                      annotation_legend_param = list(legend_direction = "horizontal", at = c(min(Year), median(Year), max(Year)), 
                                                                     labels = c(min(Year), median(Year), max(Year))),
                                      col = list(Year = colorRamp2(c(min(Year), median(Year), max(Year)), c("cornsilk", "blue",  "orange"))))
     }
     if(frequency == 12){
      return(NULL)
     }
     
     Latitude <- c(new_matrixc[,1])
     row_ha1 = rowAnnotation(Latitude = Latitude, 
                             annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Latitude)), round(mean(Latitude))
                                                                                                    , round(max(Latitude))), 
                                                            labels = c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude)))),
                             col = list(Latitude = colorRamp2(c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude))), c("cornsilk", "blue",  "orange"))))
     Longitude <- c(new_matrixc[,2])
     row_ha2 = rowAnnotation(Longitude = Longitude, 
                             annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), 
                                                            labels = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude)))),
                             col = list(Longitude = colorRamp2(c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), c("cornsilk", "blue",  "orange"))))
     if(frequency == 7){
       heatmap.plot22 <-  Heatmap(as.matrix(new_matrixc[,-c(1:4)]),  #row_dend_reorder = FALSE,
                                  row_order = get_order(o1), 
                                  name = "Heatmap",  column_dend_reorder = FALSE, 
                                  cluster_columns = FALSE, cluster_rows = FALSE, show_column_names = FALSE, row_split = new_matrixc[, c(3:4)], 
                                  row_title_gp = gpar(fill = c("#B3DE69"),col = c("black"), font = 2), 
                                  bottom_annotation = c(column_ha, column_ha2, column_ha3),
                                  left_annotation = c(row_ha1, row_ha2),
                                  col = col_fun, border = TRUE,
                                  na_col = "gray27", 
                                  heatmap_legend_param = list(
                                    legend_direction = "horizontal"
                                  ))
     }
     if(frequency == 12){
        return(NULL)
     }
     
     if ( input$Depth1 ==1 ){
       plot(heatmap.plot22)
     }
     else{
       ## MOB tree
       tree.plot <- ggparty(fit1()) +
         geom_edge() +
         coord_flip() +
         geom_edge_label( size = 6, fontface = "bold", nudge_y =  -0.005,  label.padding = unit(1, "lines"), max_length = 10 ) +
         geom_node_label(
           line_list = list(aes(label = splitvar),
                            aes(label = paste("p =",
                                              formatC(p.value,
                                                      format = "f",
                                                      digits = 2))),
                            aes(label = "")
           ),
           line_gpar = list(list(size = 20),
                            list(size = 15),
                            list(size = 2)
           ),
           # only inner nodes
           ids = "inner") +
         geom_node_info()
       if(frequency == 7){
         grob2 = grid.grabExpr(draw(heatmap.plot22, heatmap_legend_side = "bottom",  annotation_legend_side = "bottom")) 
         grid.newpage()
         pushViewport(viewport(x = 0.25, y = 0.5, width = 0.5, height = 0.9))
         grid.draw(grob2)
         pushViewport(viewport(x = 1.45, y = 0.6, width = 1, height = 0.8))
         print(tree.plot, newpage = FALSE)
       }
       if(frequency == 12){
          return(NULL)
       }
     }
   })
   
   ## title line chart
  output$titleline1 = renderText({})
  ## all series plot
  output$ClusterSeries1 <- renderPlot({
    if (is.null(df_change()))
      return(NULL)
    split.pmt <- split(na.omit(df_change()), predict(fit1(), type = "node"))
    split.pmt <- rev(split.pmt)
    new.split.pmt <- list()
    # name1 <- c(length(split.pmt):1)
    name1 <- c(1:length(split.pmt))
    for(i in 1: length(split.pmt)){
      unique.name <- unique(split.pmt[[i]]$cat.col)
      new.split.pmt[[i]] <- df_change()[df_change()$cat.col %in% unique.name,]
      new.split.pmt[[i]]$cluster <-  paste0 ("Cluster_", name1[i], collapse = ".")
    }
    pmt2 <- do.call("rbind", new.split.pmt)
    ggplot(data = pmt2, aes( x = trend, y = Series.std, group = cat.col)) +
      geom_line(size = 1, col = "gray") +
      stat_summary(fun = mean,geom="line",lwd=1,aes(group=1), col = "red", linetype = "solid") +
      xlab("") + ylab("") +
      facet_wrap(cluster~., ncol = 2)+ 
      theme_light()+
      theme(text  = element_text(size = 20), title = element_text(colour = "darkgreen", size = 15), 
            axis.text.y=element_text(size = 10), axis.text.x=element_text(size = 10))
  })
  ## title coefficient plot
  output$titleline1 = renderText({})
  ## coefficient plot
  output$Coefficientplot1 <- renderPlot({
    if (is.null(df_change()))
      return(NULL)
    if(input$Depth1 == 1){
      coef01 <- as.data.frame(t(coef(fit1())))
    }
    else{
      coef01 <- as.data.frame(coef(fit1()))
      coef01 <- apply(coef01,2,rev)

    }
    id <- c()
    name1 <- c(1:nrow(coef01))
    for(i in 1: nrow(coef01)){
      id[i] <-  paste0 ("Cluster_", name1[i], collapse = ".")
    }
    coef <- cbind.data.frame(coef01, id)
    frequency <- as.integer(input$Frequency)
    if(frequency == 7){
      colnames(coef) <- c("Intercept","trend", "Sun","Mon","Tus","Wed","Thr","Fri", "Lag1","Lag2","Lag3","Lag4",
                          "Lag5","Lag6","Lag7","Clusters")
      
      long.data <- reshape:: melt( data.frame(coef, row = 1:nrow(coef)), id.vars = c("Clusters", "row"),
                                   measure.vars = c("Intercept","trend", "Sun","Mon","Tus","Wed","Thr","Fri", "Lag1","Lag2","Lag3","Lag4",
                                                    "Lag5","Lag6","Lag7"), variable.name = "Variable", value.name = "Value")
    }
    if(frequency == 12){
      colnames(coef) <- c("Intercept","trend", "Feb","Mar","Apr","May","Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Lag1","Lag2","Lag3","Lag4",
                          "Lag5","Lag6","Lag7", "Lag8","Lag9","Lag10", "Lag11","Lag12", "Clusters")
      
      long.data <- reshape:: melt( data.frame(coef, row = 1:nrow(coef)), id.vars = c("Clusters", "row"),
                                   measure.vars = c("Intercept","trend", "Feb","Mar","Apr","May","Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Lag1","Lag2","Lag3","Lag4",
                                                    "Lag5","Lag6","Lag7", "Lag8","Lag9","Lag10", "Lag11","Lag12"), variable.name = "Variable", value.name = "Value")
    }
    long.data$Cluster <- paste('Cluster', long.data$row, sep = '')
    
    ggplot(long.data, aes( x = variable, y = value, group = row, color = Cluster)) + geom_line(size = 0.8) +
      geom_point(size = 1, shape = 15, colour = "gray50") +
      ylab("Coefficients")+
      xlab("")+
      guides(fill=guide_legend(nrow=1)) +
      theme_gray()+
      theme(panel.grid.major.x = element_line(colour = "grey99"),
            axis.title.x = element_text(size = rel(2)),
            axis.text.x = element_text(angle=90,  vjust=.8, hjust=0.8,size = rel(2)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.5)),
            legend.direction = "horizontal",
            legend.position = "bottom",
            legend.key.size = unit(1.5, "cm"))
  })
  output$info1 <- renderText({
    paste0("y=", input$plot_click$y)
  })
# ####################################
# ## Second set of results (second tab)
# ###################################
  fit2 <- reactive({
    if (is.null(df_change()))
      return(NULL)
    req(input$SplitVariables2)
    frequency <- as.integer(input$Frequency)
    var <- input$SplitVariables2
    var1 <- as.vector(unlist(var))
    form <- "Series.std ~ trend +  season"
    for (i in 1:frequency)
      form <- paste(form , " + ", paste("Lag", i, sep = '.'))
    form <- paste(form , '|')
    for (i in 1:(length(var1)-1))
      form <- paste0(form, var1[i], " + ")
    form <- paste0(form, var1[length(var1)])
    formula <- as.formula(form)
    
    ## category columns
    df1 <- df_change()
    colsfac <- df1 %>% 
      select(where(is.character)) %>%
      select(one_of(var))
    df1 <- df1 %>% mutate_at(c(colnames(colsfac)), list(~factor(.)))
    
    ### defining fit function
    linear <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
      glm(y ~ 0 + x, family = gaussian, start = start, ...)
    }
    depth <- input$Depth2
    MOBtree <- mob( formula, data = df1, fit = linear,  na.action = na.exclude, control =
                      mob_control(prune = input$Prune2,  maxdepth = depth, alpha = 0.01))

  })
  ## title MSE&AIC
  output$titleMSEAIC2 = renderText({})
  ## MSE and AIC for different MOB depth
  output$MSEAIC2 <- renderTable({
    if (is.null(df_change()))
      return(NULL)
    x <- na.omit(df_change())$Series.std
    tab <- rbind("MSE" = round(mean((x - predict(fit2(), type = "response")) ^ 2), digits = 4),
                 "AIC" = round(AIC(fit2()), digits = 0))
    cbind(c('MSE', 'AIC'), tab)
  }, colnames = FALSE, align = "l")
  output$titleHeatmap12 = renderText({})
  ## title for first MOB-heatmap
  output$MOBTree12 <- renderPlot({
    if (is.null(df_change()))
      return(NULL)
    split.pmt <- split(na.omit(df_change()), predict(fit2(), type = "node"))
    names(split.pmt) <- c(length(split.pmt): 1)
    split.pmt <- split.pmt[sort(names(split.pmt))]
    new.split.pmt <- list()
    for(i in 1: length(split.pmt)){
      unique.name <- unique(split.pmt[[i]]$cat.col)
      new.split.pmt[[i]] <- df_change()[df_change()$cat.col %in% unique.name,]
      new.split.pmt[[i]]$cluster <-  i
    }
    new_matrixc <- NULL
    for(i in 1:length(new.split.pmt)){
      new_matrix <- as.data.frame(t(matrix(new.split.pmt[[i]]$Series.std, nrow = (values$serieslength), ncol = length(unique(new.split.pmt[[i]]$cat.col)))))
      colnames(new_matrix) <- new.split.pmt[[i]][1:(values$serieslength),]$Date
      new_matrix <- cbind.data.frame('#Series' = paste0('S', nrow(new_matrix), sep = ''), new_matrix)
      new_matrix <- cbind.data.frame('ID' = paste0('C', i, sep = ''), new_matrix)
      new_matrix <- cbind.data.frame('latitude' = unique(new.split.pmt[[i]]$latitude), new_matrix)
      new_matrix <- cbind.data.frame('longitude' = unique(new.split.pmt[[i]]$longitude), new_matrix)
      new_matrixc <- bind_rows(new_matrixc, new_matrix)
    }
    col_fun = colorRamp2(c(-2, 0, 2), c(input$colorlow1, "white", input$colorhigh1))
    col_fun(seq(-3, 3))
    
    ## Two option for reordering 1- seriation 2- hclust
    o1 = seriation::seriate(dist(as.matrix(new_matrixc[,-c(1:4)])), method = "HC")
    # heatmap
    frequency <- as.integer(input$Frequency)
    if(frequency == 7){
      Weekday = c(as.character(weekdays(lubridate::ymd(colnames(new_matrixc[,-c(1:4)])))))
      column_ha = HeatmapAnnotation(Weekday = Weekday, 
                                    annotation_legend_param = list(nrow = 2),
                                    col = list(Weekday = c("Monday"="#8DD3C7", "Tuesday"="#FFFFB3", 'Wednesday'="#BEBADA" , 'Thursday'="#FB8072",
                                                           'Friday'="#80B1D3", 'Saturday'="#FDB462", 'Sunday'="#B3DE69" )))
      
      Month = c(as.character(lubridate::month(lubridate::ymd(colnames(new_matrixc[,-c(1:2)])), label = TRUE, abbr = FALSE)))
      column_ha2 = HeatmapAnnotation(Month = Month, 
                                     annotation_legend_param = list(nrow = 2),
                                     col = list(Month = c("January"="#8DD3C7", "February"="#FFFFB3", 'March'="#BEBADA" , 'April'="#FB8072",
                                                          'May'="#80B1D3", 'June'="#FDB462", 'July'="#B3DE69", 'August'="#FCCDE5",
                                                          'September'="#D9D9D9", 'October'="#BC80BD", 'November'="#CCEBC5", 'December'="#FFED6F" )))
      Year = c(lubridate::year(lubridate::ymd(colnames(new_matrixc[,-c(1:4)]))))
      column_ha3 = HeatmapAnnotation(Year = Year, 
                                     annotation_legend_param = list(legend_direction = "horizontal", at = c(min(Year), median(Year), max(Year)), 
                                                                    labels = c(min(Year), median(Year), max(Year))),
                                     col = list(Year = colorRamp2(c(min(Year), median(Year), max(Year)), c("cornsilk", "blue",  "orange"))))
    }
    if(frequency == 12){
      Month = c(as.character(lubridate::month(lubridate::ym(colnames(new_matrixc[,-c(1:4)])), label = TRUE, abbr = FALSE)))
      column_ha = HeatmapAnnotation(Month = Month, 
                                    annotation_legend_param = list(nrow = 2),
                                    col = list(Month = c("January"="#8DD3C7", "February"="#FFFFB3", 'March'="#BEBADA" , 'April'="#FB8072",
                                                         'May'="#80B1D3", 'June'="#FDB462", 'July'="#B3DE69", 'August'="#FCCDE5",
                                                         'September'="#D9D9D9", 'October'="#BC80BD", 'November'="#CCEBC5", 'December'="#FFED6F" )))
      
      Year = c(lubridate::year(lubridate::ym(colnames(new_matrixc[,-c(1:4)]))))
      column_ha2 = HeatmapAnnotation(Year = Year, 
                                     annotation_legend_param = list(legend_direction = "horizontal", at = c(min(Year), median(Year), max(Year)), 
                                                                    labels = c(min(Year), median(Year), max(Year))),
                                     col = list(Year = colorRamp2(c(min(Year), median(Year), max(Year)), c("cornsilk", "blue",  "orange"))))
    }
    Latitude <- c(new_matrixc[,1])
    row_ha1 = rowAnnotation(Latitude = Latitude, 
                            annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Latitude)), round(mean(Latitude))
                                                                                                   , round(max(Latitude))), 
                                                           labels = c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude)))),
                            col = list(Latitude = colorRamp2(c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude))), c("cornsilk", "blue",  "orange"))))
    Longitude <- c(new_matrixc[,2])
    row_ha2 = rowAnnotation(Longitude = Longitude, 
                            annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), 
                                                           labels = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude)))),
                            col = list(Longitude = colorRamp2(c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), c("cornsilk", "blue",  "orange"))))
    
    if(frequency == 7){
      heatmap.plot <- Heatmap(as.matrix(new_matrixc[,-c(1:4)]), 
                              row_order = get_order(o1), 
                              name = "Heatmap",  column_dend_reorder = FALSE, 
                              cluster_columns = FALSE, cluster_rows = FALSE, show_column_names = FALSE, row_split = new_matrixc[, c(3:4)], 
                              row_title_gp = gpar(fill = c("#B3DE69"),col = c("black"), font = 2), 
                              bottom_annotation = c(column_ha, column_ha2, column_ha3),
                              left_annotation = c(row_ha1, row_ha2),
                              col = col_fun, border = TRUE,
                              na_col = "gray27", 
                              heatmap_legend_param = list(
                                legend_direction = "horizontal"
                              ))
    }
    
    if(frequency == 12){
      heatmap.plot <- Heatmap(as.matrix(new_matrixc[,-c(1:4)]), 
                              row_order = get_order(o1), 
                              name = "Heatmap",  column_dend_reorder = FALSE, 
                              cluster_columns = FALSE, cluster_rows = FALSE, show_column_names = FALSE, row_split = new_matrixc[, c(3:4)], 
                              row_title_gp = gpar(fill = c("#B3DE69"),col = c("black"), font = 2), 
                              bottom_annotation = c(column_ha, column_ha2),
                              left_annotation = c(row_ha1, row_ha2),
                              col = col_fun, border = TRUE,
                              na_col = "gray27", 
                              heatmap_legend_param = list(
                                legend_direction = "horizontal"
                              ))
    }
    
    if ( input$Depth1 ==1 ){
      plot(heatmap.plot)
    }
    else{
      ## MOB tree
      tree.plot <- ggparty(fit2()) +
        geom_edge() +
        coord_flip() +
        geom_edge_label( size = 6, fontface = "bold", nudge_y =  -0.005,  label.padding = unit(1, "lines"), max_length = 10 ) +
        geom_node_label(
          line_list = list(aes(label = splitvar),
                           aes(label = paste("p =",
                                             formatC(p.value,
                                                     format = "f",
                                                     digits = 2))),
                           aes(label = "")
          ),
          line_gpar = list(list(size = 20),
                           list(size = 15),
                           list(size = 2)
          ),
          # only inner nodes
          ids = "inner") +
        geom_node_info()
      ## printing both plots
      grob = grid.grabExpr(draw(heatmap.plot, heatmap_legend_side = "bottom",  annotation_legend_side = "bottom")) 
      grid.newpage()
      pushViewport(viewport(x = 0.25, y = 0.5, width = 0.5, height = 0.9))
      grid.draw(grob)
      pushViewport(viewport(x = 1.45, y = 0.6, width = 1, height = 0.8))
      print(tree.plot, newpage = FALSE)
    }
  } )
  output$titleHeatmap22 = renderText({})
  ## heatmap by day of week
  output$MOBTree22 <- renderPlot({
    if (is.null(df_change()))
      return(NULL)
    split.pmt <- split(na.omit(df_change()), predict(fit2(), type = "node"))
    names(split.pmt) <- c(length(split.pmt): 1)
    split.pmt <- split.pmt[sort(names(split.pmt))]
    new.split.pmt <- list()
    for(i in 1: length(split.pmt)){
      unique.name <- unique(split.pmt[[i]]$cat.col)
      new.split.pmt[[i]] <- df_change()[df_change()$cat.col %in% unique.name,]
      new.split.pmt[[i]]$cluster <-  i
    }
    frequency <- as.integer(input$Frequency)
    if(frequency == 7){
      order.times <- order.times <- function(df){
        test <- df %>%
          mutate('date' = lubridate::ymd(as.Date(Date)))%>%
          mutate('times' = months(date)) %>%
          mutate('times2' = weekdays(date)) %>%
          group_by(cat.col) %>%
          arrange(factor(times, levels = month.name)) 
        test3 <- df %>%
          mutate('date' = lubridate::ymd(as.Date(Date)))%>%
          mutate('times' = months(date)) %>%
          mutate('times2' = weekdays(date)) %>%
          group_by(cat.col) %>%
          arrange(factor(times2, levels = DescTools::day.name))
        test1 <- test %>%
          arrange(cat.col)
        
        split1 <- split(test1, test1$cat.col, drop = TRUE) 
        
        for(i in 1:length(split1)){
          split1[[i]] <- split1[[i]] %>%
            mutate('times2' = make.unique(times, sep = '_'))
        }
        
        test11 <- test3 %>%
          arrange(cat.col)
        
        split11 <- split(test11, test1$cat.col, drop = TRUE) 
        
        for(i in 1:length(split11)){
          split11[[i]] <- split11[[i]] %>%
            mutate('times' = make.unique(times, sep = '_'))
        }
        
        test2 <- do.call(rbind.data.frame, split1)
        test22 <- do.call(rbind.data.frame, split11)
        return(list(test22, test2))
      }
      ordar.times.pmt <- lapply(new.split.pmt, order.times)
      
      new_matrixc <- NULL
      for(i in 1:length(ordar.times.pmt)){
        new_matrix <- as.data.frame(t(matrix(ordar.times.pmt[[i]][[1]]$Series.std, nrow = (values$serieslength), ncol = length(unique(ordar.times.pmt[[i]][[1]]$cat.col)))))
        colnames(new_matrix) <- ordar.times.pmt[[i]][[1]][1:(values$serieslength),]$Date
        new_matrix <- cbind.data.frame('#Series' = paste0('S', nrow(new_matrix), sep = ''), new_matrix)
        new_matrix <- cbind.data.frame('ID' = paste0('C', i, sep = ''), new_matrix)
        new_matrix <- cbind.data.frame('latitude' = unique(new.split.pmt[[i]]$latitude), new_matrix)
        new_matrix <- cbind.data.frame('longitude' = unique(new.split.pmt[[i]]$longitude), new_matrix)
        new_matrixc <- bind_rows(new_matrixc, new_matrix)
      }
      
    }
    if(frequency == 12){
      order.times <- order.times <- function(df){
        test <- df %>%
          mutate('date' = lubridate::ym(Date))%>%
          mutate('times' = months(date)) %>%
          group_by(cat.col) %>%
          arrange(factor(times, levels = month.name)) 
        test1 <- test %>%
          arrange(cat.col)
        
        split1 <- split(test1, test1$cat.col, drop = TRUE) 
        
        for(i in 1:length(split1)){
          split1[[i]] <- split1[[i]] %>%
            mutate('times2' = make.unique(times, sep = '_'))
        }
        
        test2 <- do.call(rbind.data.frame, split1)
        return(test2)
      }
      ordar.times.pmt <- lapply(new.split.pmt, order.times)
      
      new_matrixc <- NULL
      for(i in 1:length(ordar.times.pmt)){
        new_matrix <- as.data.frame(t(matrix(ordar.times.pmt[[i]]$Series.std, nrow = (values$serieslength), ncol = length(unique(ordar.times.pmt[[i]]$cat.col)))))
        colnames(new_matrix) <- ordar.times.pmt[[i]][1:(values$serieslength),]$Date
        new_matrix <- cbind.data.frame('#Series' = paste0('S', nrow(new_matrix), sep = ''), new_matrix)
        new_matrix <- cbind.data.frame('ID' = paste0('C', i, sep = ''), new_matrix)
        new_matrix <- cbind.data.frame('latitude' = unique(new.split.pmt[[i]]$latitude), new_matrix)
        new_matrix <- cbind.data.frame('longitude' = unique(new.split.pmt[[i]]$longitude), new_matrix)
        new_matrixc <- bind_rows(new_matrixc, new_matrix)
      }
    }
    
    
    
    col_fun = colorRamp2(c(-2, 0, 2), c(input$colorlow1, "white", input$colorhigh1))
    col_fun(seq(-3, 3))
    
    ## Two option for reordering 1- seriation 2- hclust
    
    frequency <- as.integer(input$Frequency)
    if(frequency == 7){
      o1 = seriation:: seriate(dist(as.matrix(new_matrixc[,-c(1:4)])), method = "HC")
      Weekday = c(as.character(weekdays(lubridate::ymd(colnames(new_matrixc[,-c(1:4)])))))
      column_ha = HeatmapAnnotation(Weekday = Weekday, 
                                    annotation_legend_param = list(nrow = 2),
                                    col = list(Weekday = c("Monday"="#8DD3C7", "Tuesday"="#FFFFB3", 'Wednesday'="#BEBADA" , 'Thursday'="#FB8072",
                                                           'Friday'="#80B1D3", 'Saturday'="#FDB462", 'Sunday'="#B3DE69" )))
      
      Month = c(as.character(lubridate::month(lubridate::ymd(colnames(new_matrixc[,-c(1:4)])), label = TRUE, abbr = FALSE)))
      column_ha2 = HeatmapAnnotation(Month = Month, 
                                     annotation_legend_param = list(nrow = 2),
                                     col = list(Month = c("January"="#8DD3C7", "February"="#FFFFB3", 'March'="#BEBADA" , 'April'="#FB8072",
                                                          'May'="#80B1D3", 'June'="#FDB462", 'July'="#B3DE69", 'August'="#FCCDE5",
                                                          'September'="#D9D9D9", 'October'="#BC80BD", 'November'="#CCEBC5", 'December'="#FFED6F" )))
      Year = c(lubridate::year(lubridate::ymd(colnames(new_matrixc[,-c(1:4)]))))
      column_ha3 = HeatmapAnnotation(Year = Year, 
                                     annotation_legend_param = list(legend_direction = "horizontal", at = c(min(Year), median(Year), max(Year)), 
                                                                    labels = c(min(Year), median(Year), max(Year))),
                                     col = list(Year = colorRamp2(c(min(Year), median(Year), max(Year)), c("cornsilk", "blue",  "orange"))))
    }
    if(frequency == 12){
      o1 = seriation:: seriate(dist(as.matrix(new_matrixc[,-c(1:4)])), method = "HC")
      ## Annotations
      Month = c(as.character(lubridate::month(lubridate::ym(colnames(new_matrixc[,-c(1:4)])), label = TRUE, abbr = FALSE)))
      column_ha = HeatmapAnnotation(Month = Month, 
                                    annotation_legend_param = list(nrow = 2),
                                    col = list(Month = c("January"="#8DD3C7", "February"="#FFFFB3", 'March'="#BEBADA" , 'April'="#FB8072",
                                                         'May'="#80B1D3", 'June'="#FDB462", 'July'="#B3DE69", 'August'="#FCCDE5",
                                                         'September'="#D9D9D9", 'October'="#BC80BD", 'November'="#CCEBC5", 'December'="#FFED6F" )))
      
      Year = c(lubridate::year(lubridate::ym(colnames(new_matrixc[,-c(1:4)]))))
      column_ha2 = HeatmapAnnotation(Year = Year, 
                                     annotation_legend_param = list(legend_direction = "horizontal", at = c(min(Year), median(Year), max(Year)), 
                                                                    labels = c(min(Year), median(Year), max(Year))),
                                     col = list(Year = colorRamp2(c(min(Year), median(Year), max(Year)), c("cornsilk", "blue",  "orange"))))
    }
    
    Latitude <- c(new_matrixc[,1])
    row_ha1 = rowAnnotation(Latitude = Latitude, 
                            annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Latitude)), round(mean(Latitude))
                                                                                                   , round(max(Latitude))), 
                                                           labels = c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude)))),
                            col = list(Latitude = colorRamp2(c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude))), c("cornsilk", "blue",  "orange"))))
    Longitude <- c(new_matrixc[,2])
    row_ha2 = rowAnnotation(Longitude = Longitude, 
                            annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), 
                                                           labels = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude)))),
                            col = list(Longitude = colorRamp2(c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), c("cornsilk", "blue",  "orange"))))
    if(frequency == 7){
      heatmap.plot2 <-  Heatmap(as.matrix(new_matrixc[,-c(1:4)]),  #row_dend_reorder = FALSE,
                                 row_order = get_order(o1), 
                                 name = "Heatmap",  column_dend_reorder = FALSE, 
                                 cluster_columns = FALSE, cluster_rows = FALSE, show_column_names = FALSE, row_split = new_matrixc[, c(3:4)], 
                                 row_title_gp = gpar(fill = c("#B3DE69"),col = c("black"), font = 2), 
                                 bottom_annotation = c(column_ha, column_ha2, column_ha3),
                                 left_annotation = c(row_ha1, row_ha2),
                                 col = col_fun, border = TRUE,
                                 na_col = "gray27", 
                                 heatmap_legend_param = list(
                                   legend_direction = "horizontal"
                                 ))
    }
    if(frequency == 12){
      heatmap.plot2 <- Heatmap(as.matrix(new_matrixc[,-c(1:4)]),  #row_dend_reorder = FALSE,
                               row_order = get_order(o1), 
                               name = "Heatmap",  column_dend_reorder = FALSE, 
                               cluster_columns = FALSE, cluster_rows = FALSE, show_column_names = FALSE, row_split = new_matrixc[, c(3:4)], 
                               row_title_gp = gpar(fill = c("#B3DE69"),col = c("black"), font = 2), 
                               bottom_annotation = c(column_ha, column_ha2),
                               left_annotation = c(row_ha1, row_ha2),
                               col = col_fun, border = TRUE,
                               na_col = "gray27", 
                               heatmap_legend_param = list(
                                 legend_direction = "horizontal"
                               ))
    }
    
    if ( input$Depth1 ==1 ){
      plot(heatmap.plot2)
    }
    else{
      ## MOB tree
      tree.plot <- ggparty(fit2()) +
        geom_edge() +
        coord_flip() +
        geom_edge_label( size = 6, fontface = "bold", nudge_y =  -0.005,  label.padding = unit(1, "lines"), max_length = 10 ) +
        geom_node_label(
          line_list = list(aes(label = splitvar),
                           aes(label = paste("p =",
                                             formatC(p.value,
                                                     format = "f",
                                                     digits = 2))),
                           aes(label = "")
          ),
          line_gpar = list(list(size = 20),
                           list(size = 15),
                           list(size = 2)
          ),
          # only inner nodes
          ids = "inner") +
        geom_node_info()
      if(frequency == 7){
        # printing both plots
        grob1 = grid.grabExpr(draw(heatmap.plot2, heatmap_legend_side = "bottom",  annotation_legend_side = "bottom")) 
        grid.newpage()
        pushViewport(viewport(x = 0.25, y = 0.5, width = 0.5, height = 0.9))
        grid.draw(grob1)
        pushViewport(viewport(x = 1.45, y = 0.6, width = 1, height = 0.8))
        print(tree.plot, newpage = FALSE)
      }
      if(frequency == 12){
        # printing both plots
        grob = grid.grabExpr(draw(heatmap.plot2, heatmap_legend_side = "bottom",  annotation_legend_side = "bottom")) 
        grid.newpage()
        pushViewport(viewport(x = 0.25, y = 0.5, width = 0.5, height = 0.9))
        grid.draw(grob)
        pushViewport(viewport(x = 1.45, y = 0.6, width = 1, height = 0.8))
        print(tree.plot, newpage = FALSE)
      }
    }
  })
  
  output$titleHeatmap223 = renderText({})
  ## heatmap by day of week
  output$MOBTree223 <- renderPlot({
    if (is.null(df_change()))
      return(NULL)
    split.pmt <- split(na.omit(df_change()), predict(fit2(), type = "node"))
    names(split.pmt) <- c(length(split.pmt): 1)
    split.pmt <- split.pmt[sort(names(split.pmt))]
    new.split.pmt <- list()
    for(i in 1: length(split.pmt)){
      unique.name <- unique(split.pmt[[i]]$cat.col)
      new.split.pmt[[i]] <- df_change()[df_change()$cat.col %in% unique.name,]
      new.split.pmt[[i]]$cluster <-  i
    }
    frequency <- as.integer(input$Frequency)
    if(frequency == 7){
      order.times <- order.times <- function(df){
        test <- df %>%
          mutate('date' = lubridate::ymd(as.Date(Date)))%>%
          mutate('times' = months(date)) %>%
          mutate('times2' = weekdays(date)) %>%
          group_by(cat.col) %>%
          arrange(factor(times, levels = month.name)) 
        test3 <- df %>%
          mutate('date' = lubridate::ymd(as.Date(Date)))%>%
          mutate('times' = months(date)) %>%
          mutate('times2' = weekdays(date)) %>%
          group_by(cat.col) %>%
          arrange(factor(times2, levels = DescTools::day.name))
        test1 <- test %>%
          arrange(cat.col)
        
        split1 <- split(test1, test1$cat.col, drop = TRUE) 
        
        for(i in 1:length(split1)){
          split1[[i]] <- split1[[i]] %>%
            mutate('times2' = make.unique(times, sep = '_'))
        }
        
        test11 <- test3 %>%
          arrange(cat.col)
        
        split11 <- split(test11, test1$cat.col, drop = TRUE) 
        
        for(i in 1:length(split11)){
          split11[[i]] <- split11[[i]] %>%
            mutate('times' = make.unique(times, sep = '_'))
        }
        
        test2 <- do.call(rbind.data.frame, split1)
        test22 <- do.call(rbind.data.frame, split11)
        return(list(test22, test2))
      }
      ordar.times.pmt <- lapply(new.split.pmt, order.times)
      
      new_matrixc <- NULL
      for(i in 1:length(ordar.times.pmt)){
        new_matrix <- as.data.frame(t(matrix(ordar.times.pmt[[i]][[2]]$Series.std, nrow = (values$serieslength), ncol = length(unique(ordar.times.pmt[[i]][[2]]$cat.col)))))
        colnames(new_matrix) <- ordar.times.pmt[[i]][[2]][1:(values$serieslength),]$Date
        new_matrix <- cbind.data.frame('#Series' = paste0('S', nrow(new_matrix), sep = ''), new_matrix)
        new_matrix <- cbind.data.frame('ID' = paste0('C', i, sep = ''), new_matrix)
        new_matrix <- cbind.data.frame('latitude' = unique(new.split.pmt[[i]]$latitude), new_matrix)
        new_matrix <- cbind.data.frame('longitude' = unique(new.split.pmt[[i]]$longitude), new_matrix)
        new_matrixc <- bind_rows(new_matrixc, new_matrix)
      }
    }
    if(frequency == 12){
      return(NULL)
    }
    
    col_fun = colorRamp2(c(-2, 0, 2), c(input$colorlow1, "white", input$colorhigh1))
    col_fun(seq(-3, 3))
    
    ## Two option for reordering 1- seriation 2- hclust
    
    frequency <- as.integer(input$Frequency)
    if(frequency == 7){
      o1 = seriation:: seriate(dist(as.matrix(new_matrixc[,-c(1:4)])), method = "HC")
      Weekday = c(as.character(weekdays(lubridate::ymd(colnames(new_matrixc[,-c(1:4)])))))
      column_ha = HeatmapAnnotation(Weekday = Weekday, 
                                    annotation_legend_param = list(nrow = 2),
                                    col = list(Weekday = c("Monday"="#8DD3C7", "Tuesday"="#FFFFB3", 'Wednesday'="#BEBADA" , 'Thursday'="#FB8072",
                                                           'Friday'="#80B1D3", 'Saturday'="#FDB462", 'Sunday'="#B3DE69" )))
      
      Month = c(as.character(lubridate::month(lubridate::ymd(colnames(new_matrixc[,-c(1:4)])), label = TRUE, abbr = FALSE)))
      column_ha2 = HeatmapAnnotation(Month = Month, 
                                     annotation_legend_param = list(nrow = 2),
                                     col = list(Month = c("January"="#8DD3C7", "February"="#FFFFB3", 'March'="#BEBADA" , 'April'="#FB8072",
                                                          'May'="#80B1D3", 'June'="#FDB462", 'July'="#B3DE69", 'August'="#FCCDE5",
                                                          'September'="#D9D9D9", 'October'="#BC80BD", 'November'="#CCEBC5", 'December'="#FFED6F" )))
      Year = c(lubridate::year(lubridate::ymd(colnames(new_matrixc[,-c(1:4)]))))
      column_ha3 = HeatmapAnnotation(Year = Year, 
                                     annotation_legend_param = list(legend_direction = "horizontal", at = c(min(Year), median(Year), max(Year)), 
                                                                    labels = c(min(Year), median(Year), max(Year))),
                                     col = list(Year = colorRamp2(c(min(Year), median(Year), max(Year)), c("cornsilk", "blue",  "orange"))))
    }
    if(frequency == 12){
      return(NULL)
    }
    
    Latitude <- c(new_matrixc[,1])
    row_ha1 = rowAnnotation(Latitude = Latitude, 
                            annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Latitude)), round(mean(Latitude))
                                                                                                   , round(max(Latitude))), 
                                                           labels = c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude)))),
                            col = list(Latitude = colorRamp2(c(round(min(Latitude)), round(mean(Latitude)), round(max(Latitude))), c("cornsilk", "blue",  "orange"))))
    Longitude <- c(new_matrixc[,2])
    row_ha2 = rowAnnotation(Longitude = Longitude, 
                            annotation_legend_param = list(legend_direction = "horizontal", at = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), 
                                                           labels = c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude)))),
                            col = list(Longitude = colorRamp2(c(round(min(Longitude)), round(mean(Longitude)), round(max(Longitude))), c("cornsilk", "blue",  "orange"))))
    if(frequency == 7){
      heatmap.plot2 <-  Heatmap(as.matrix(new_matrixc[,-c(1:4)]),  #row_dend_reorder = FALSE,
                                 row_order = get_order(o1), 
                                 name = "Heatmap",  column_dend_reorder = FALSE, 
                                 cluster_columns = FALSE, cluster_rows = FALSE, show_column_names = FALSE, row_split = new_matrixc[, c(3:4)], 
                                 row_title_gp = gpar(fill = c("#B3DE69"),col = c("black"), font = 2), 
                                 bottom_annotation = c(column_ha, column_ha2, column_ha3),
                                 left_annotation = c(row_ha1, row_ha2),
                                 col = col_fun, border = TRUE,
                                 na_col = "gray27", 
                                 heatmap_legend_param = list(
                                   legend_direction = "horizontal"
                                 ))
    }
    if(frequency == 12){
      return(NULL)
    }
    
    if ( input$Depth1 ==1 ){
      plot(heatmap.plot2)
    }
    else{
      ## MOB tree
      tree.plot <- ggparty(fit2()) +
        geom_edge() +
        coord_flip() +
        geom_edge_label( size = 6, fontface = "bold", nudge_y =  -0.005,  label.padding = unit(1, "lines"), max_length = 10 ) +
        geom_node_label(
          line_list = list(aes(label = splitvar),
                           aes(label = paste("p =",
                                             formatC(p.value,
                                                     format = "f",
                                                     digits = 2))),
                           aes(label = "")
          ),
          line_gpar = list(list(size = 20),
                           list(size = 15),
                           list(size = 2)
          ),
          # only inner nodes
          ids = "inner") +
        geom_node_info()
      if(frequency == 7){
        grob2 = grid.grabExpr(draw(heatmap.plot2, heatmap_legend_side = "bottom",  annotation_legend_side = "bottom")) 
        grid.newpage()
        pushViewport(viewport(x = 0.25, y = 0.5, width = 0.5, height = 0.9))
        grid.draw(grob2)
        pushViewport(viewport(x = 1.45, y = 0.6, width = 1, height = 0.8))
        print(tree.plot, newpage = FALSE)
      }
      if(frequency == 12){
        return(NULL)
      }
    }
  })
#   
#   ## title line charts
  output$titleline2 = renderText({})
  ## all series plot (line charts)
  output$ClusterSeries2 <- renderPlot({
    if (is.null(df_change()))
      return(NULL)
    split.pmt <- split(na.omit(df_change()), predict(fit2(), type = "node"))
    split.pmt <- rev(split.pmt)
    new.split.pmt <- list()
    name1 <- c(1:length(split.pmt))
    for(i in 1: length(split.pmt)){
      unique.name <- unique(split.pmt[[i]]$cat.col)
      new.split.pmt[[i]] <- df_change()[df_change()$cat.col %in% unique.name,]
      new.split.pmt[[i]]$cluster <-  paste0 ("Cluster_", name1[i], collapse = ".")
    }
    pmt2 <- do.call("rbind", new.split.pmt)
    ggplot(data = pmt2, aes( x = trend, y = Series.std, group = cat.col)) +
      geom_line(size = 1, col = "gray") +
      stat_summary(fun = mean,geom="line",lwd=1,aes(group=1), col = "red", linetype = "solid") +
      xlab("") + ylab("") +
      facet_wrap(cluster~., ncol = 2)+ 
      theme_light()+
      theme(text  = element_text(size = 20), title = element_text(colour = "darkgreen", size = 15), 
            axis.text.y=element_text(size = 10), axis.text.x=element_text(size = 10))
  })
#   ## title coefficient plot
  output$titleline2 = renderText({})
  ## coefficient plot
  output$Coefficientplot2 <- renderPlot({
    if (is.null(df_change()))
      return(NULL)
    if(input$Depth2 == 1){
      coef01 <- as.data.frame(t(coef(fit2())))
    }
    else{
      coef01 <- as.data.frame(coef(fit2()))
      coef01 <- apply(coef01,2,rev)

    }
    id <- c()
    name1 <- c(1:nrow(coef01))
    for(i in 1: nrow(coef01)){
      id[i] <-  paste0 ("Cluster_", name1[i], collapse = ".")
    }
    coef <- cbind.data.frame(coef01, id)
    frequency <- as.integer(input$Frequency)
    if(frequency == 7){
      colnames(coef) <- c("Intercept","trend", "Sun","Mon","Tus","Wed","Thr","Fri", "Lag1","Lag2","Lag3","Lag4",
                          "Lag5","Lag6","Lag7","Clusters")
      
      long.data <- reshape:: melt( data.frame(coef, row = 1:nrow(coef)), id.vars = c("Clusters", "row"),
                                   measure.vars = c("Intercept","trend", "Sun","Mon","Tus","Wed","Thr","Fri", "Lag1","Lag2","Lag3","Lag4",
                                                    "Lag5","Lag6","Lag7"), variable.name = "Variable", value.name = "Value")
    }
    if(frequency == 12){
      colnames(coef) <- c("Intercept","trend", "Feb","Mar","Apr","May","Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Lag1","Lag2","Lag3","Lag4",
                          "Lag5","Lag6","Lag7", "Lag8","Lag9","Lag10", "Lag11","Lag12", "Clusters")
      
      long.data <- reshape:: melt( data.frame(coef, row = 1:nrow(coef)), id.vars = c("Clusters", "row"),
                                   measure.vars = c("Intercept","trend", "Feb","Mar","Apr","May","Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Lag1","Lag2","Lag3","Lag4",
                                                    "Lag5","Lag6","Lag7", "Lag8","Lag9","Lag10", "Lag11","Lag12"), variable.name = "Variable", value.name = "Value")
    }
    
    long.data$Cluster <- paste('Cluster', long.data$row, sep = '')
    
    ggplot(long.data, aes( x = variable, y = value, group = row, color = Cluster)) + geom_line(size = 0.8) +
      geom_point(size = 1, shape = 15, colour = "gray50") +
      ylab("Coefficients")+
      xlab("")+
      guides(fill=guide_legend(nrow=1)) +
      theme_gray()+
      theme(panel.grid.major.x = element_line(colour = "grey99"),
            axis.title.x = element_text(size = rel(2)),
            axis.text.x = element_text(angle=90,  vjust=.8, hjust=0.8,size = rel(2)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.5)),
            legend.direction = "horizontal",
            legend.position = "bottom",
            legend.key.size = unit(1.5, "cm"))
  })
  output$info2 <- renderText({
    paste0("y=", input$plot_click$y)
  })
  
  ### screenshot
  observeEvent(input$go, {
    screenshot(scale = 2,
               filename = "screenshot")
  })
})
