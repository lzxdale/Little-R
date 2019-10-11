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

Jan <- read.csv("JANUARY 2018-des.csv")
product_list <- sort(unique(Jan$title))
exporter_list <- sort(unique(Jan$IMPORTER))
country_list <- sort(unique(Jan$ORIGIN))
importer_list <- sort(unique(Jan$EXPORTER))


ui <- fluidPage(
  headerPanel('Adhesives Product'),
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
           "Importer" = selectInput("importer", label = h4("Select Import Company"),
                                    choices = importer_list,
                                    selected = 1),
           "Exporter" = selectInput("exporter", label = h4("Select Export Country"),
                                    choices = country_list,
                                    selected = 1),
           "Totals" = selectInput("product_1", label = h4("Select Product"), 
                                  choices = product_list)
           )
  })
  output$ui2 <- renderUI({
    switch(input$action,
           "Exporter" = selectInput("co", label = h4("Select Export Company"),
                                      choices = append("ALL", as.character(importer_list)),
                                    selected = 1)
    )
  })
  
  output$ui4 <- renderUI({
    if (is.null(input$action))
      return()
    if (is.null(input$product_1))
      return()
    switch(input$action,
           "Importer" = selectInput("type_subim", label = h4("Select Category"),
                                    choices = append("ALL", as.character(importer_list)),
                                    selected = 1),
           "Exporter" = selectInput("type_subex", label = h4("Select Category"),
                                    choices = append("ALL", as.character(exporter_list)),
                                    selected = 1),
           "Top 10 Countries - Heatmap" = selectInput("type", label = h4("Select Category"), 
                                                      choices = sort(unique(Jan[Jan$title == input$product_1,]$Type))),
           "Top 10 Countries - Barplot" = selectInput("type", label = h4("Select Category"), 
                                                      choices = sort(unique(Jan[Jan$title == input$product_1,]$Type))),
           "Top 10 Importers" = selectInput("type", label = h4("Select Category"), 
                                            choices = sort(unique(Jan[Jan$title == input$product_1,]$Type))),
           "Median Price" = selectInput("type", label = h4("Select Category"), 
                                        choices = sort(unique(Jan[Jan$title == input$product_1,]$Type))),
           "Average Price" = selectInput("type", label = h4("Select Category"), 
                                         choices = sort(unique(Jan[Jan$title == input$product_1,]$Type))),
           "Median and Average Comparison" = selectInput("type", label = h4("Select Category"), 
                                                         choices = sort(unique(Jan[Jan$title == input$product_1,]$Type))),
           "Totals" =  selectInput("type", label = h4("Select Category"), 
                                   choices = sort(unique(Jan[Jan$title == input$product_1,]$Type)))
    
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
  #top_countries <- Jan %>% subset(title == input$type) %>% subset(MONTH == input$month) %>% group_by(Countries) %>% summarise(Total = sum(QUANTITY)) %>% top_n(n = 10, wt = Total) 
  top_countries <- Jan %>% subset(title == input$type) %>% group_by(ORIGIN) %>% summarise(Total = sum(as.numeric(QUANTITY.kg.))) %>% top_n(n = 10, wt = Total) 
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
            'Importer' = h3("Summary Table"),
            'Exporter' = h3("Summary Table"),
            'Totals' = h3("Category : ",input$type)
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
  
}
  

shinyApp(ui = ui, server = server)