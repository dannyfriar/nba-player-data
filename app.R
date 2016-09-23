##------- Shiny app to visualize NBA data
libs <- c('data.table', 'dplyr', 'DT', 'ggplot2', 'gridExtra', 'plyr', 'RColorBrewer', 'reshape2', 'RMySQL', 'scales', 'shiny', 'shinythemes', 'xts') 
lapply(libs, require, character.only = TRUE)
rm(libs)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  return(paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" "))
}

rmUnderScore <- function(x) {
  y <- paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep = '')
  uPos <- regexpr('_', x)[1]
  y <- paste(substr(y, 1, uPos), toupper(substr(y, uPos + 1, uPos + 1)), substr(y, uPos + 2, nchar(y)), sep = '')
  return(gsub('_', ' ', y))
}

nba_data <- data.table(read.csv('nbaPlayerStats.csv'))[order(team)]
nba_data$X <- NULL
nba_data$team <- as.character(nba_data$team)
nba_data$team <- sapply(nba_data$team, simpleCap)
nba_data$name <- sapply(as.character(nba_data$name), simpleCap)
nba_data <- nba_data[nchar(name)>0]
nba_data$name <- sapply(nba_data$name, rmUnderScore)
nba_data$pie <- as.numeric(gsub('%', '', as.character(nba_data$pie))) / 100

# --------UI
ui = {
  fluidPage(theme = shinytheme('journal'),
            
    headerPanel('NBA Player Stats'),
    
    sidebarPanel(
      selectInput('team', 'Select Team:', choices=unique(nba_data$team), selected='Warriors'),
      selectInput('plot_var', 'Choose Plot Variable:', choices=c('Points', 'Rebounds', 'Assists', 'PIE'), selected='PIE'),
      width = 3
    ),
            
    mainPanel(
      tabsetPanel(id = 'panel',
        tabPanel('Player Stats',
            h4(textOutput('subtitle_text')),
            plotOutput('plot'),
            DT::dataTableOutput('player_table'), br()
        )
      )  
    )
  )
}

#-------- SERVER
server = shinyServer(function(input, output){
  
  output$subtitle_text <- renderText({
    paste0('Showing per game player stats for the ', input$team, ':')
  })
  
  x <- reactive({
    # y <- nba_data[team == input$team][order(-eval(parse(text=input$order)))]
    y <- nba_data[team == input$team][order(-pie)]
    y$name <- factor(y$name, levels=unique(y$name))
    setnames(y, names(y), sapply(names(y), simpleCap))
    setnames(y, 'Pie', 'PIE')
    y
  })
  
  output$plot <- renderPlot({
    g <- ggplot(data=x(), aes_string(x='Name', y=input$plot_var))
    g <- g + geom_bar(stat='identity', fill='firebrick', color='black')
    g <- g + theme(axis.text.x=element_text(angle=90, size=12), axis.title.x=element_text(size=13))
    g <- g + theme(axis.text.y=element_text(size=12), axis.title.y=element_text(size=13))
    if (input$plot_var == 'PIE')
      g <- g + scale_y_continuous(labels=percent)
    g
  })
  
  output$player_table <- DT::renderDataTable({
    DT::datatable(x(), rownames = FALSE, selection = 'none',
                  options = list(
                    scrollY = 300,
                    scrollCollapse = TRUE,
                    bPaginate = FALSE,
                    bInfo = FALSE,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  )
    ) %>% DT::formatPercentage(ncol(x()), digits=0)
  })
  
})

#-------- APP
shinyApp(ui = ui, server = server)