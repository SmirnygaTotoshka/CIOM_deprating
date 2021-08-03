#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#specs = c("Лечебное дело","Педиатрия","Стоматология","Медицинская биофизика",
#              "Медицинская биохимия","Медицинская кибернетика","Фармация","Биологи (бакалавриат)",
#              "Биологи (IMAT)","Лечебное дело (IMAT)","Биологи (магистратура)", "Клиническая психология",
#              "Социальная работа(бакалавриат)", "Социальная работа (магистратура)")
specs = c("Лечебное дело","Биология (бакалавриат)","Медицинская биофизика","Медицинская биохимия",
          "Медицинская кибернетика","Фармация","Лечебное дело (англофоны)","Биология (IMAT)",
          "Лечебное дело (IMAT)","Клиническая психология","Социальная работа","Педиатрия","Стоматология")
#topics = c("Программа обучения","Культура преподавания","Организация учебного процесса",
#           "Материально-техническое оснащение","Введение АОС")
# Define UI for application that draws a histogram
shinyUI(
    
    fluidPage(

    # Application title
    titlePanel("ЦИОМ РНИМУ, Рейтинг кафедр."),

    # Sidebar with a slider input for number of bins
     fluidPage( tabsetPanel(
        tabPanel("FAQ", tags$h1("Зачем всё это нужно?")), 
        tabPanel("Опрос по дисциплинам",
                 sidebarLayout(
                     sidebarPanel(selectizeInput("Speciality",label = "Направление:", choices = specs,selected = T),
                                  numericInput("Course",label = "Курс:", value = 1,min = 1,max = 6,step = 1),width = 3),
                     mainPanel(width = 9,
                         fluidPage( 
                            fluidRow(column(width = 12,dataTableOutput("CompProd"))
                                            )
                                    )
                                  )
                              )
                    ), 
        tabPanel("Рейтинг кафедр",dataTableOutput("CathedraRating"))))
))
