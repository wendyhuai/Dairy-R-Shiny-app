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

merged = read_csv('dairy.csv')
short = cbind(merged$DATE1,merged$IMPORTER,merged$Countries,merged$DESCRIPTION,merged$Type,merged$Price, merged$QUANTITY,merged$PhPkg,merged$USDton, merged$NO.)
colnames(short) = c('Date','Company','Countries','Product','Category','Price (PhP)','Quantity (Kg)', 'Unit Price (PhP/kg)','Unit Price (USD/ton)','No')
short = as.data.frame(short)
short$No = as.numeric(short$No)
short = short[sort(short$No),]

export = cbind(merged$DATE1,merged$EXPORTER,merged$Countries, merged$IMPORTER,merged$DESCRIPTION,merged$Type,merged$Price, merged$QUANTITY,merged$PhPkg,merged$USDton, merged$NO.)
colnames(export) = c('Date','Company','Countries','Importer','Product','Category','Price (PhP)','Quantity (Kg)', 'Unit Price (PhP/kg)','Unit Price (USD/ton)','No')
export = as.data.frame(export)
export$No=as.numeric(export$No)
export = export[order(export$No),]

### Shiny App ####
ui <- fluidPage(
  # App title ----
  titlePanel("Ingredient Statistics"),
  
  ### select bars ####
  sidebarLayout(
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

    # end ####
    mainPanel(
      uiOutput('result'),
      uiOutput('result2'),
      uiOutput('result3'),
      uiOutput('result4')
    )
  )
)
  

server <- function(input, output){
  output$ui1 <- renderUI({
    if (is.null(input$action))
      return()
    switch(input$action,
          "Top 10 Countries - Heatmap" = selectInput("group1", label = h4("Select Category"), 
                                                      choices = c("MILK","CHEESE","OTHERS")),
           
          "Top 10 Countries - Barplot" = selectInput("group1", label = h4("Select Category"), 
                                                        choices = c("MILK","CHEESE","OTHERS")),
          "Top 10 Importers" = selectInput("group1", label = h4("Select Category"), 
                                           choices = c("MILK","CHEESE","OTHERS")),
          "Median Price" =  selectInput("group1", label = h4("Select Category"), 
                                        choices = c("MILK","CHEESE","OTHERS")),
          "Average Price"=  selectInput("group1", label = h4("Select Category"), 
                                         choices = c("MILK","CHEESE","OTHERS")),
          "Median and Average Comparison"=  selectInput("group1", label = h4("Select Category"), 
                                                        choices = c("MILK","CHEESE","OTHERS")),
          "Importer" = selectInput("importer", label = h4("Select Import Company"),
                                    choices = sort(c(unique(merged$IMPORTER))),
                                    selected = 1),
          "Exporter" = selectInput("exporter", label = h4("Select Export Country"),
                                    choices = sort(c(unique(merged$Countries))),
                                    selected = 1),
          "Totals" = selectInput("group1", label = h4("Select Category"), 
                                 choices = c("MILK","CHEESE","OTHERS"))

           
    )
  })
  
  
  output$ui2 <- renderUI({
    if (is.null(input$action))
      return()
    if (is.null(input$group1))
      return()
    if (input$group1 == "MILK" | input$group1 == "OTHERS")
      return()
    switch(input$action,
           "Top 10 Countries - Heatmap" = selectInput("group2", label = h4("Select Category"), 
                                                      if (input$group1 == "CHEESE") {
                                                        choices = sort(unique(merged[merged$Group == input$group1,]$Group2))
                                                      }),
           "Top 10 Countries - Barplot" = selectInput("group2", label = h4("Select Category"), 
                                                      if (input$group1 == "CHEESE") {
                                                        choices = sort(unique(merged[merged$Group == input$group1,]$Group2))
                                                      }),
           "Top 10 Importers"= selectInput("group2", label = h4("Select Category"), 
                                           if (input$group1 == "CHEESE") {
                                             choices = sort(unique(merged[merged$Group == input$group1,]$Group2))
                                           }),
           "Median Price"= selectInput("group2", label = h4("Select Category"), 
                                           if (input$group1 == "CHEESE") {
                                             choices = sort(unique(merged[merged$Group == input$group1,]$Group2))
                                           }),
           "Average Price"= selectInput("group2", label = h4("Select Category"), 
                                           if (input$group1 == "CHEESE") {
                                             choices = sort(unique(merged[merged$Group == input$group1,]$Group2))
                                           }),
           "Median and Average Comparison"= selectInput("group2", label = h4("Select Category"), 
                                           if (input$group1 == "CHEESE") {
                                             choices = sort(unique(merged[merged$Group == input$group1,]$Group2))
                                           }),
           
          
           "Totals" =  selectInput("group2", label = h4("Select Category"), 
                                   if (input$group1 == "CHEESE") {
                                     choices = sort(unique(merged[merged$Group == input$group1,]$Group2))
                                   })
    )
  })
  
  
  output$ui3 <- renderUI({
    if (length(input$type_subex) == 0)
      return()
    switch(input$action,
           "Exporter" = selectInput("co", label = h4("Select Export Company"),
                                    if( input$type_subex == "ALL"){
                                    choices = c("ALL", as.character(sort(unique(export[export$Countries == input$exporter,]$Company))))}
                                    else{
                                      choices =  as.character(sort(unique(export[export$Countries == input$exporter & export$Category == input$type_subex,]$Company)))
                                    },
                                    selected = 1)
           )
  })
  

  
  output$ui4 <- renderUI({
    if (is.null(input$action))
      return()
    if (is.null(input$group1))
      return()
    switch(input$action,
           "Importer" = selectInput("type_subim", label = h4("Select Category"),
                                    choices = c("ALL", as.character(sort(unique(short[short$Company == input$importer,]$Category)))),
                                    selected = 1),
           "Exporter" = selectInput("type_subex", label = h4("Select Category"),
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
  
  
  output$ui5 <- renderUI({
    if (is.null(input$action))
      return()
    if (is.null(input$group1))
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
  
 output$result <- renderUI({
    switch (input$action,
            'Top 10 Countries - Heatmap' = emptymap() ,
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
 
output$comp_info <- renderTable({
  short$`Unit Price (USD/ton)` = formatC(short$`Unit Price (USD/ton)`, format="d", big.mark=",")
  short$`Quantity (Kg)`=formatC(short$`Quantity (Kg)`, format="d", big.mark=",")
  short$`Price (PhP)`=formatC(short$`Price (PhP)`, format="d", big.mark=",")
  short$`Unit Price (USD/ton)`=formatC(short$`Unit Price (USD/ton)`, format="d", big.mark=",")
  short$`Unit Price (PhP/kg)`=formatC(short$`Unit Price (PhP/kg)`, format="d", big.mark=",")
  if(input$type_subim=="ALL") {
    return(short[short$Company == input$importer,c(1,4:ncol(short)-1)])
  }else{
    return(short[short$Company == input$importer& short$Category ==input$type_subim,c(1,4:ncol(short)-1)] )
  }
})
output$export_info <- renderTable({
  colnames(export) = c('Date','Export Company','Countries','Import Company','Product','Category','Price (PhP)','Quantity (Kg)', 'Unit Price (PhP/kg)','Unit Price (USD/ton)')
  export$`Unit Price (USD/ton)` = formatC(export$`Unit Price (USD/ton)`, format="d", big.mark=",")
  export$`Quantity (Kg)`=formatC(export$`Quantity (Kg)`, format="d", big.mark=",")
  export$`Price (PhP)`=formatC(export$`Price (PhP)`, format="d", big.mark=",")
  export$`Unit Price (USD/ton)`=formatC(export$`Unit Price (USD/ton)`, format="d", big.mark=",")
  export$`Unit Price (PhP/kg)`=formatC(export$`Unit Price (PhP/kg)`, format="d", big.mark=",")
  
  df = export[export$Countries == input$exporter, ]
  if (length(input$type_subex) == 0 | length(input$co) == 0)
    return()
  
  if(input$type_subex == "ALL" & input$co == "ALL"){
    return( df[,c(1:2,4:ncol(export)-1)])
  }
  if(input$type_subex != "ALL" & input$co == "ALL"){
    return(df[df$Category == input$type_subex,c(1:2,4:ncol(export)-1)]) 
      }
  if(input$type_subex == "ALL" & input$co != "ALL"){
    return( df[df$`Export Company` == input$co,c(1:2,4:ncol(export)-1)])
  }
  if(input$type_subex != "ALL" & input$co != "ALL"){
    return( df[df$`Export Company` == input$co &  df$Category == input$type_subex,c(1:2,4:ncol(export)-1)])
  }
})
output$totals <- renderTable({
  name = c('Category', 'Quantity Total', 'Average Price','Median Price','Unit Price (PhP/kg)','Unit Price (USD/ton)' )
  n = length(unique(merged$Type))
  df = data.frame(matrix(0, ncol = 6, nrow = n))
  colnames(df) = name
  merged1 = merged[order(merged$Type),]
  df[,1] = unique(merged1$Type)
  merged1$QUANTITY = as.numeric(merged1$QUANTITY)
  merged1$Price= as.numeric( merged1$Price)
  merged1$PhPkg= as.numeric(merged1$PhPkg)
  merged1$USDton= as.numeric(merged1$USDton)
  
  a = merged1 %>% group_by(Type) %>% summarise(Total = sum(QUANTITY))
  df[,2] = a[,2]
  b = merged1 %>% group_by(Type) %>% summarise( a = mean(Price))
  df[,3] = b[,2]
  c = merged1 %>% group_by(Type) %>% summarise( a = median(Price))
  df[,4] = c[,2]
  d = merged1 %>% group_by(Type) %>% summarise( a = mean(PhPkg))
  df[,5] = d[,2]
  e = merged1 %>% group_by(Type) %>% summarise( a = mean(USDton))
  df[,6] = e[,2]
  df[,2:4] = format(round(df[,2:4]), nsmall= 0,format="d", big.mark=",")
  df[,5:6] = format(round(df[,5:6]), nsmall= 1,format="d", big.mark=",")
  return(df)
  })
writetable <- function(input){
    name = c('Category', 'Quantity Total (kg)', 'Average Price','Median Price','Unit Price (PhP/kg)','Unit Price (USD/ton)' )
    n = length(unique(short[short$Company == input,]$Category))
    df = data.frame(matrix(0, ncol = 6, nrow = n))
    colnames(df) = name
    df[,1] = unique(short[short$Company == input,]$Category)
    short$`Quantity (Kg)`=as.numeric(short$`Quantity (Kg)`)
    a = short %>% subset(Company == input)%>% group_by(Category) %>% summarise(Total = sum(`Quantity (Kg)`)) 
    df[,2] = a[,2]
    short$`Price (PhP)`=as.numeric(short$`Price (PhP)`)
    b = short %>% subset(Company == input)%>% group_by(Category) %>% summarise(avg = mean(`Price (PhP)`)) 
    df[,3] = b[,2]
    c = short %>% subset(Company == input)%>% group_by(Category) %>% summarise(avg = median(`Price (PhP)`)) 
    df[,4] = c[,2]
    short$`Unit Price (PhP/kg)`=as.numeric(short$`Unit Price (PhP/kg)`)
    short$`Unit Price (USD/ton)`=as.numeric(short$`Unit Price (USD/ton)`)
    d = short %>% subset(Company == input)%>% group_by(Category) %>% summarise(avg = mean(`Unit Price (PhP/kg)`) )
    df[,5] = d[,2]
    e = short %>% subset(Company == input)%>% group_by(Category) %>% summarise(avg = mean(`Unit Price (USD/ton)`)) 
    df[,6] = e[,2]
    df[,2:4] = format(round(df[,2:4]), nsmall= 0,format="d", big.mark=",")
    df[,5:6] = format(round(df[,5:6]), nsmall= 1,format="d", big.mark=",")
    return(df)
}
writetable2 <- function(input){
    name = c('Category', 'Quantity Total (kg)', 'Average Price','Median Price','Unit Price (PhP/kg)','Unit Price (USD/ton)' )
    n = length(unique(export[export$Countries == input,]$Category))
    df = data.frame(matrix(0, ncol = 6, nrow = n))
    colnames(df) = name
    df[,1] = unique(export[export$Countries == input,]$Category)
    export$`Quantity (Kg)`=as.numeric(export$`Quantity (Kg)`)
    a = export %>% subset(Countries == input)%>% group_by(Category) %>% summarise(Total = sum(`Quantity (Kg)`)) 
    df[,2] = a[,2]
    export$`Price (PhP)`=as.numeric(export$`Price (PhP)`)
    b = export %>% subset(Countries == input)%>% group_by(Category) %>% summarise(avg = mean(`Price (PhP)`)) 
    df[,3] = b[,2]
    c = export %>% subset(Countries == input)%>% group_by(Category) %>% summarise(avg = median(`Price (PhP)`)) 
    df[,4] = c[,2]
    export$`Unit Price (PhP/kg)`=as.numeric(export$`Unit Price (PhP/kg)`)
    export$`Unit Price (USD/ton)`=as.numeric(export$`Unit Price (USD/ton)`)
    d = export %>% subset(Countries == input)%>% group_by(Category) %>% summarise(avg = mean(`Unit Price (PhP/kg)`) )
    df[,5] = d[,2]
    e = export %>% subset(Countries == input)%>% group_by(Category) %>% summarise(avg = mean(`Unit Price (USD/ton)`)) 
    df[,6] = e[,2]
    df[,2:4] = format(round(df[,2:4]), nsmall= 0,format="d", big.mark=",")
    df[,5:6] = format(round(df[,5:6]), nsmall= 1,format="d", big.mark=",")
    return(df)
}
 output$summary_table <- renderTable({
  df = writetable(input$importer)
  return(df)
})
 output$summary_table2 <- renderTable({
  df = writetable2(input$exporter)
  return(df)
})

 output$top_importer_pie <- renderPlotly ({
  
   top_importer <- merged %>% subset(Type == input$type) %>% subset(MONTH == input$month) %>% group_by(IMPORTER) %>% summarise(Total = sum(QUANTITY)) %>% top_n(n = 10, wt = Total)
   top_importer <- top_importer[order(top_importer$Total,decreasing = TRUE),]
   
  if( length(top_importer$IMPORTER) == 0) return( plot_ly( type = "pie" ) %>% layout( title = "There is no data available under this category."))
   
   plot_ly(
     top_importer,
     labels = ~IMPORTER, values = ~Total, type = 'pie',
     colors = 'Blues'
   ) %>% layout( title = "Top Importers")
 })

 output$top_exporter_bar <- renderPlotly ({
   top_countries <- merged %>% subset(Type == input$type) %>% subset(MONTH == input$month) %>% group_by(Countries) %>% summarise(Total = sum(QUANTITY)) %>% top_n(n = 10, wt = Total)
   top_countries$Countries <- factor(top_countries$Countries, levels = unique(top_countries$Countries)[order(top_countries$Total, decreasing = TRUE)])
   
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

emptymap <- function( )({
  top_countries <- merged %>% subset(Type == input$type) %>% subset(MONTH == input$month) %>% group_by(Countries) %>% summarise(Total = sum(QUANTITY)) %>% top_n(n = 10, wt = Total) 
  top_countries=top_countries[order(top_countries$Total,decreasing = TRUE),]
  if(length(top_countries$Total) == 0) return(h4('There is no data available under this category.'))
}) 

 output$top_exporter_map <- renderLeaflet({
   top_countries <- merged %>% subset(Type == input$type) %>% subset(MONTH == input$month) %>% group_by(Countries) %>% summarise(Total = sum(QUANTITY)) %>% top_n(n = 10, wt = Total) 
   top_countries=top_countries[order(top_countries$Total,decreasing = TRUE),]

   if (length(top_countries$Total) == 0){ 
     return( map <- leaflet(data) %>% addTiles( ) )
     }
 
   if (nrow( top_countries)==1){
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

 output$price_median <- renderPlotly({
   price_median = merged %>% subset(Type == input$type)  %>% group_by(MONTH) %>% summarise(Median = median(PhPkg))
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

 output$price_mean <- renderPlotly({
   price_average = merged %>% subset(Type == input$type)  %>% group_by(MONTH) %>% summarise(Average = mean(PhPkg))
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

 output$price_sidebyside <- renderPlotly({
   price_average = merged %>% subset(Type == input$type)  %>% group_by(MONTH) %>% summarise(Average = mean(PhPkg))
   price_average[nrow(price_average)+1,]= list(13,mean(price_average$Average))
   price_median = merged %>% subset(Type == input$type)  %>% group_by(MONTH) %>% summarise(Median = median(PhPkg))
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
  
}

shinyApp(ui = ui, server = server)




