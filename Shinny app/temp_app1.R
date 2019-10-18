library(shiny)
library(shinythemes)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(leaflet)
library(maps)
library(leafpop)
library(mapview)
library(DT)

DF <- read.csv("Collection_data.csv")
product_list <- sort(unique(DF$title))
exporter_list <- sort(unique(DF$EXPORTER))
country_list <- sort(unique(DF$Countries))
importer_list <- sort(unique(DF$IMPORTER))


ui <- fluidPage(
  headerPanel('Product Analysis'),
  sidebarPanel(
    selectInput("action", label = h4("Select Action"), 
                choices = c("Top 10 Countries - Heatmap", "Top 10 Countries - Barplot" ,"Top 10 Importers",
                            "Median Price","Average Price","Median and Average Comparison",
                            "Importer","Exporter","Totals"),
                selected = 1),
    uiOutput('ui1'),
    uiOutput('ui2'),
    uiOutput('ui3'),
    uiOutput('ui4'),
    uiOutput('ui5'),
    hr()
  ),
  mainPanel(
    uiOutput('result'),
    uiOutput('result2'),
    uiOutput('result3'),
    uiOutput('result4')
  )
)

server <- function(input, output){
  output$ui1 <- renderUI({
    switch(input$action,
           "Top 10 Countries - Heatmap" = selectInput("product_1", label = h4("Select Product"),
                                                      choices = product_list), 
           "Top 10 Countries - Barplot" = selectInput("product_1", label = h4("Select Product"),
                                                      choices = product_list),
           "Top 10 Importers"= selectInput("product_1", label = h4("Select Product"),
                                           choices = product_list),
           "Median Price" =  selectInput("product_1", label = h4("Select Product"), 
                                         choices = product_list),
           "Average Price"=  selectInput("product_1", label = h4("Select Product"), 
                                         choices = product_list),
           "Median and Average Comparison"=  selectInput("product_1", label = h4("Select Product"), 
                                                         choices = product_list),
           "Importer" = selectInput("importer_com", label = h4("Select Import Company"),
                                    choices = importer_list,
                                    selected = 1),
           "Exporter" = selectInput("exporter_cry", label = h4("Select Export Country"),
                                    choices = country_list,
                                    selected = 1),
           "Totals" = selectInput("product_1", label = h4("Select Product"), 
                                  choices = product_list)
    )
  })
  # output$ui2 <- renderUI({
  #   switch(input$action,
  #          "Exporter" = selectInput("co", label = h4("Select Export Company"),
  #                                   if( input$type_subex == "ALL"){
  #                                     choices = c("ALL", as.character(sort(unique(DF[DF$Countries == input$exporter_cry,]$EXPORTER))))}
  #                                   else{
  #                                     choices =  as.character(sort(unique(DF[DF$Countries == input$exporter_cry & export$Category == input$exp_product,]$Company)))
  #                                   },
  #                                   selected = 1)
  #   )
  # })
  
  ###product given country or company is selected#####
  output$ui3 <- renderUI({
    if (is.null(input$action))
      return()
    if (is.null(input$group1))
      return()
    switch(input$action,
           "Importer" = selectInput("imp_product", label = h4("Select Category"),
                                    choices = c("ALL", as.character(sort(unique(short[short$Company == input$importer,]$Category)))),
                                    selected = 1),
           "Exporter" = selectInput("exp_product", label = h4("Select Category"),
                                    choices = c("ALL", as.character(sort(unique(export[export$Countries == input$exporter,]$Category)))),
                                    selected = 1),
           "Top 10 Countries - Heatmap" = selectInput("type", label = h4("Select Category"), 
                                                      if (input$group1 != "CHEESE") {choices = sort(unique(merged[merged$Group == input$group1,]$Type))
                                                      } else {choices = sort(unique(merged[merged$Group2 == input$group2,]$Type))}),
           "Top 10 Countries - Barplot" = selectInput("type", label = h4("Select Category"), 
                                                      if (input$group1 != "CHEESE") {choices = sort(unique(merged[merged$Group == input$group1,]$Type))
                                                      } else {choices = sort(unique(merged[merged$Group2 == input$group2,]$Type))}),
           "Top 10 Importers" = selectInput("type", label = h4("Select Category"), 
                                            if (input$group1 != "CHEESE") {choices = sort(unique(merged[merged$Group == input$group1,]$Type))
                                            } else {choices = sort(unique(merged[merged$Group2 == input$group2,]$Type))}),
           "Median Price" = selectInput("type", label = h4("Select Category"), 
                                        if (input$group1 != "CHEESE") {choices = sort(unique(merged[merged$Group == input$group1,]$Type))
                                        } else {choices = sort(unique(merged[merged$Group2 == input$group2,]$Type))}),
           "Average Price" = selectInput("type", label = h4("Select Category"), 
                                         if (input$group1 != "CHEESE") {choices = sort(unique(merged[merged$Group == input$group1,]$Type))
                                         } else {choices = sort(unique(merged[merged$Group2 == input$group2,]$Type))}),
           "Median and Average Comparison" = selectInput("type", label = h4("Select Category"), 
                                                         if (input$group1 != "CHEESE") {choices = sort(unique(merged[merged$Group == input$group1,]$Type))
                                                         } else {choices = sort(unique(merged[merged$Group2 == input$group2,]$Type))}),
           "Totals" =  selectInput("type", label = h4("Select Category"), 
                                   if (input$group1 != "CHEESE") {choices = sort(unique(merged[merged$Group == input$group1,]$Type))
                                   } else {choices = sort(unique(merged[merged$Group2 == input$group2,]$Type))})
    )
    
    
  })
  
  
  
  output$ui4 <- renderUI({
    if (is.null(input$action))
      return()
    if (is.null(input$product_1))
      return()
    switch(input$action,
           "Top 10 Countries - Heatmap" = selectInput("month", label = h4("Select Month"),
                                                      choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4,
                                                                     "May"= 5, "June"=6,"July"=7, "August" = 8,
                                                                     "September" = 9, "October" = 10, "November" = 11, "December"= 12,"Annual" = 13),
                                                      selected = 1),
           "Top 10 Countries - Barplot" = selectInput("month", label = h4("Select Month"),
                                                      choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4,
                                                                     "May"= 5, "June"=6,"July"=7, "August" = 8,
                                                                     "September" = 9, "October" = 10, "November" = 11, "December"= 12,"Annual" = 13),
                                                      selected = 1),
           "Top 10 Importers"= selectInput("month", label = h4("Select Month"),
                                           choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4,
                                                          "May"= 5, "June"=6,"July"=7, "August" = 8,
                                                          "September" = 9, "October" = 10, "November" = 11, "December"= 12,"Annual" = 13),
                                           selected = 1)
    )
    
  })
  
  emptymap <- function( )({
    top_countries <- DF %>% subset(title == input$product_1) %>% subset(MONTH == input$month) %>% group_by(Countries) %>% summarise(Total = sum(as.numeric(QUANTITY.kg.))) %>% top_n(n = 10, wt = Total) 
    #top_countries <- DF %>% subset(title == input$product_1) %>% group_by(ORIGIN) %>% summarise(Total = sum(as.numeric(QUANTITY.kg.))) %>% top_n(n = 10, wt = Total) 
    top_countries=top_countries[order(top_countries$Total,decreasing = TRUE),]
    if(length(top_countries$Total) == 0) return(h4('There is no data available under this category.'))
  }) 
  
  
  
  output$result <- renderUI({
    switch (input$action,
            'Top 10 Countries - Heatmap' = emptymap(),
            'Top 10 Countries - Barplot' = plotlyOutput('top_exporter_bar'),
            'Top 10 Importers' = plotlyOutput('top_importer_pie'),
            'Median Price' = plotlyOutput('price_median'),
            'Average Price' = plotlyOutput('price_mean'),
            'Median and Average Comparison' = plotlyOutput('price_sidebyside'),
            'Importer' = h3("Summary Table: "),
            'Exporter' = h3("Summary Table: "),
            'Totals' = h3("Category : ",input$product_1)
    ) 
  })
  output$result2 <- renderUI({
    switch (input$action,
            'Importer' = tableOutput('summary_table'),
            'Exporter' =  tableOutput('summary_table2'),
            'Top 10 Countries - Heatmap' = leafletOutput('top_exporter_map',height = 450),
            'Totals' = plotlyOutput('price_sidebyside')
            
    ) 
  })
  output$result3 <- renderUI({
    switch (input$action,
            'Importer' =  h3("Company Name: ",input$importer),
            'Exporter' =  h3("Export Country: ",input$exporter),
            'Totals' = h3("Summary Table, Year 2019")
            
            
    ) 
  })
  output$result4 <- renderUI({
    switch (input$action,
            'Importer' = tableOutput('comp_info'),
            'Exporter' = tableOutput('export_info'),
            'Totals' = tableOutput('totals')
    ) 
  })
  
  
  ####### Graph and output table ###############
  ###exporter info######
  
  
  
  ###Summary and table #####
  
  
  
  
  
  ####Top_importer_10####
  output$top_importer_pie <- renderPlotly ({
    top_importer <- DF %>% subset(title == input$product_1) %>% subset(MONTH == input$month) %>% group_by(IMPORTER) %>% summarise(Total = sum(QUANTITY.kg.)) %>% top_n(n = 10, wt = Total)
    top_importer <- top_importer[order(top_importer$Total,decreasing = TRUE),]
    if( length(top_importer$IMPORTER) == 0) return( plot_ly( type = "pie" ) %>% layout( title = "There is no data available under this category."))
    plot_ly(
      top_importer,
      labels = ~IMPORTER, values = ~Total, type = 'pie',
      colors = 'Blues',
      showlegend = F
    ) %>% layout( title = "Top Importers")
  })
  
  ###top_10_exporter_bar####
  output$top_exporter_bar <- renderPlotly ({
    top_countries <- DF %>% subset(title == input$product_1) %>% subset(MONTH == input$month) %>% group_by(Countries) %>% summarise(Total = sum(QUANTITY.kg.)) %>% top_n(n = 10, wt = Total)
    top_countries$Countries <- factor(top_countries$Countries, levels = unique(top_countries$Countries))
    top_countries <- top_countries[order(-top_countries$Total),]
    if (length(top_countries$Countries) == 0) return( plot_ly(  type = "bar") %>% layout( xaxis = list(title = "", range = c(0,10)), yaxis = list(title = "", range = c(0,10)),title = "There is no data available under this category.") )
    plot_ly(top_countries,
            x = ~Countries,
            y = ~Total,
            type = "bar"
    ) %>%
      layout(
        title = "Top Export Countries",
        yaxis = list(title = 'Quantity (kg)')
      )
  })
  
  
  ####Price comparison####
  output$price_sidebyside <- renderPlotly({
    price_average = DF %>% subset(title == input$product_1)  %>% group_by(MONTH) %>% summarise(Average = mean(Phpkg))
    price_average[nrow(price_average)+1,]= list(13,mean(price_average$Average))
    price_median = DF %>% subset(title == input$product_1)  %>% group_by(MONTH) %>% summarise(Median = median(Phpkg))
    price_median[nrow(price_median)+1,]= list(13,mean(price_median$Median))
    data= data.frame(c(1:13))
    colnames(data)="MONTH"
    a = merge(merge(data,price_average, all = TRUE ),price_median,all = TRUE)
    a[is.na(a)]=0
    plot_ly(a,
            x = ~MONTH,
            y = ~Average,
            type = "bar",
            name = "Average Price")%>%
      layout(
        title = "Median and Average Prices Comparison",
        xaxis = list(
          title = 'Months',
          ticktext = c(month.abb[1:12],'Annual'),
          tickvals = c(1:13),
          tickmode = "array"),
        yaxis = list(
          title = 'Price (PhP/kg)')) %>%
      add_trace(y = ~Median, name = 'Median Price')
  })
  
  
  ### mean price#####
  output$price_mean <- renderPlotly({
    price_average = DF %>% subset(title == input$product_1)  %>% group_by(MONTH) %>% summarise(Average = mean(Phpkg))
    price_average[nrow(price_average)+1,]= list(13,mean(price_average$Average))
    data= data.frame(c(1:13))
    colnames(data)="MONTH"
    a = merge(data,price_average, all = TRUE )
    a[is.na(a)]=0
    plot_ly(a,
            x = ~MONTH,
            y = ~Average,
            type = "bar",
            name = "Price")%>%
      layout(
        title = "Average Price by Months",
        xaxis = list(
          title = "Months",
          ticktext = c(month.abb[1:12],"Annual"),
          tickvals = c(1:13),
          tickmode = "array"),
        yaxis = list( title = 'Price (PhP/kg)'))  %>%
      add_lines(y =mean(price_average$Average),x = c(0,14), mode = "lines",name = "Average Price")
  })
  
  
  
  ####price_median#####
  output$price_median <- renderPlotly({
    price_median = DF %>% subset(title == input$product_1)  %>% group_by(MONTH) %>% summarise(Median = median(Phpkg))
    price_median[nrow(price_median)+1,]= list(13,mean(price_median$Median))
    data= data.frame(c(1:13))
    colnames(data)="MONTH"
    a = merge(data,price_median, all = TRUE )
    a[is.na(a)]=0
    plot_ly(a,
            x = ~MONTH,
            y = ~Median,
            type = "bar",
            name = "Price")%>% layout(
              title = "Median Price by Months",
              xaxis = list(
                title = 'Months',
                ticktext =c( month.abb[1:12],"Annual"),
                tickvals = c(1:13),
                tickmode = "array"),
              yaxis = list(title = "Price (PhP/kg)")
            )
    
  })
  
  
  
  ### heat map###
  output$top_exporter_map <- renderLeaflet({
    top_countries <- DF %>% subset(title == input$product_1) %>% subset(MONTH == input$month) %>% group_by(Countries) %>% summarise(Total = sum(as.numeric(QUANTITY.kg.))) %>% top_n(n = 10, wt = Total) 
    top_countries=top_countries[order(top_countries$Total,decreasing = TRUE),]
    if (length(top_countries$Total) == 0){ 
      return( map <- leaflet(data) %>% addTiles( ) )
    }
    if (nrow(top_countries)==1){
      bounds <- map("world", top_countries$Countries, fill = TRUE, plot = FALSE)
      i_popup <- paste0("<strong>Countries: </strong>", top_countries$Countries, "<br>", "<strong>Quantity: </strong>", formatC(top_countries$Total, format = "d", big.mark = ",", big.interval = 3), " kg")
      i2_popup <- paste(bounds$country," :", formatC(bounds$Value, format = "d", big.mark = ",", big.interval = 3)," kg")
      map <- leaflet(data) %>%
        addTiles() %>%
        addPolygons(data = bounds, group = "Countries", 
                    color = "blue", 
                    weight = 2,
                    popup  = i_popup,
                    fillOpacity = 0.8,
                    label = i2_popup,
                    highlightOptions = highlightOptions(color = "black", 
                                                        weight = 2,
                                                        bringToFront = TRUE))
      return(map)
    } 
    
    binpal <- colorBin("YlGnBu", top_countries$Total, 5, pretty = FALSE)
    countries <- top_countries$Countries
    bounds <- map("world", countries, fill = TRUE, plot = FALSE)
    bounds$country <- vapply(strsplit(bounds$name, ":"), function(x) x[1], FUN.VALUE="a")
    bounds$Value <- top_countries$Total[match(bounds$country, countries)]
    i_popup <- paste0("<strong>Countries: </strong>", bounds$country, "<br>", "<strong>Quantity: </strong>", formatC(bounds$Value, format = "d", big.mark = ",", big.interval = 3)," kg")
    i2_popup <- paste(bounds$country," :", formatC(bounds$Value, format = "d", big.mark = ",", big.interval = 3)," kg")
    map <- leaflet(data) %>%
      addTiles() %>%
      addPolygons(data = bounds, group = "Countries", 
                  #color = "blue", 
                  #color = ~binpal(top_countries$Total),
                  fillColor = ~binpal(bounds$Value),
                  weight = 2,
                  popup  = i_popup,
                  fillOpacity = 1,
                  label=i2_popup,
                  highlightOptions = highlightOptions(color = "black", 
                                                      weight = 2,
                                                      bringToFront = TRUE))%>%
      addLegend(pal = binpal, values = bounds$Value, opacity = 1,
                title = "Aggregated Quantity (kg)")
  })
  
}


shinyApp(ui = ui, server = server)
