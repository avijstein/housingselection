library(shiny)
setwd('~/Desktop/Real Life/Housing Analysis/For ResLife/')

# reading in the data, readying it for Shiny.
HousingCheatSheet = read.csv('housing.csv', stringsAsFactors = FALSE)
HousingCheatSheet =  HousingCheatSheet[2:length(HousingCheatSheet)]
HousingCheatSheet = HousingCheatSheet[HousingCheatSheet$Reserve == 'NO',]
HousingCheatSheet$Link = paste0("<a href= target='_blank'>",HousingCheatSheet$Link,"</a>")

names(HousingCheatSheet) = c('Address', 'Number', 'Street', 'Number of People', 'Bathrooms', 'Beds Average',
                             'Full Beds', 'Quiet', 'Fire Code Capacity', 'Minutes to Fountain',
                             'Minutes to Olin', 'Minutes to Gym', 'Reserve', 'ADA Compliance', 'New',
                             'Duplex', 'Link')


ui<-fluidPage(
  fluidRow(
    column(3, "Features",
           sliderInput(inputId="size",
                       label="Number of People",
                       value=c(min(HousingCheatSheet$`Number of People`),max(HousingCheatSheet$`Number of People`)),
                       min=min(HousingCheatSheet$`Number of People`),max=max(HousingCheatSheet$`Number of People`),
                       step=1),
           sliderInput(inputId="bath",
                       label="Number of Bathrooms",
                       value=c(min(HousingCheatSheet$Bathrooms),max(HousingCheatSheet$Bathrooms)),
                       min=min(HousingCheatSheet$Bathrooms),max=max(HousingCheatSheet$Bathrooms),
                       step=0.5),
           sliderInput(inputId="fullbeds",
                       label="Number of Full Beds",
                       value=c(min(HousingCheatSheet$`Full Beds`),max(HousingCheatSheet$`Full Beds`)),
                       min=min(HousingCheatSheet$`Full Beds`),max=max(HousingCheatSheet$`Full Beds`),
                       step=1),
           checkboxGroupInput(inputId="quiet",
                              label="Quiet",
                              choices=c("YES", "NO"),
                              selected=c("YES","NO")),
           sliderInput(inputId="distOlin",
                       label="Minutes from Olin",
                       value=c(min(HousingCheatSheet$`Minutes to Olin`),max(HousingCheatSheet$`Minutes to Olin`)),
                       min=min(HousingCheatSheet$`Minutes to Olin`),max=max(HousingCheatSheet$`Minutes to Olin`),
                       step=1),
           sliderInput(inputId="distFountain",
                       label="Minutes from Fountain",
                       value=c(min(HousingCheatSheet$`Minutes to Fountain`),max(HousingCheatSheet$`Minutes to Fountain`)),
                       min=min(HousingCheatSheet$`Minutes to Fountain`),max=max(HousingCheatSheet$`Minutes to Fountain`),
                       step=1),
           sliderInput(inputId="distGym",
                       label="Minutes from Gym",
                       value=c(min(HousingCheatSheet$`Minutes to Gym`),max(HousingCheatSheet$`Minutes to Gym`)),
                       min=min(HousingCheatSheet$`Minutes to Gym`),max=max(HousingCheatSheet$`Minutes to Gym`),
                       step=1),
           checkboxGroupInput(inputId="ada",
                              label="ADA Compliant",
                              choices=c("YES","NO"),
                              selected=c("YES","NO")),
           checkboxGroupInput(inputId="duplex",
                              label="Duplex",
                              choices=c("YES","NO"),
                              selected=c("YES","NO"))),
    column(9, "Available Properties",
           dataTableOutput(outputId="table")
    )))


server <- function(input, output){
  output$table<-renderDataTable({
    HousingCheatSheet[HousingCheatSheet$`Number of People`<=input$size[2]&HousingCheatSheet$`Number of People`>=input$size[1] &
                        HousingCheatSheet$Bathrooms<=input$bath[2]&HousingCheatSheet$Bathrooms>=input$bath[1] &
                        HousingCheatSheet$`Full Beds`<=input$fullbeds[2]&HousingCheatSheet$`Full Beds`>=input$fullbeds[1] &
                        HousingCheatSheet$`Minutes to Olin`<=input$distOlin[2]&HousingCheatSheet$`Minutes to Olin`>=input$distOlin[1] &
                        HousingCheatSheet$`Minutes to Fountain`<=input$distFountain[2]&HousingCheatSheet$`Minutes to Fountain`>=input$distFountain[1] &
                        HousingCheatSheet$`Minutes to Gym`<=input$distGym[2]&HousingCheatSheet$`Minutes to Gym`>=input$distGym[1] &
                        HousingCheatSheet$Duplex %in% input$duplex &
                        HousingCheatSheet$`ADA Compliance` %in% input$ada &
                        HousingCheatSheet$Quiet %in% input$quiet,
                      c('Address', 'Number of People', 'Bathrooms', 'Full Beds', 'Minutes to Olin', 
                        'Minutes to Fountain', 'Minutes to Gym', 'Fire Code Capacity', 'Quiet', 'Link')]
  }, escape = FALSE)
}



shinyApp(ui = ui, server = server)
