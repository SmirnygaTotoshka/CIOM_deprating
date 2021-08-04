#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(googlesheets4)
library(dplyr)
library(readxl)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    #linkMeta = "https://docs.google.com/spreadsheets/d/1gdbQJc36-6auD9IFuGAoBAf4T-cKIpEQM93q-TYCiQQ/edit?usp=sharing"
    #myData = read_sheet(linkMeta) %>% as.data.frame()
    myData = readxl::read_xlsx("Data/meta.xlsx")
    observeEvent(input$Speciality,{
        withProgress(message = 'Rendering, please wait!', {
            spec = isolate({input$Speciality})
            course = isolate({input$Course})
            link = pull(myData[myData$Speciality == spec & myData$Course == course,"Path"])
            print(link)
            if(is.na(link)){
                tbl.Comp.Prod = NA
            }
            else{
                #tbl.Comp.Prod = read_sheet(link) %>% as.data.frame()
                tbl.Comp.Prod = readxl::read_xlsx(link)
            }
            output$CompProd = renderDataTable({
                validate(need(!is.na(tbl.Comp.Prod), "Нет данных"))
                
                tbl.Comp.Prod
            })
            output$Num.Ans = renderText({
                validate(need(!is.na(tbl.Comp.Prod), ""))
                num.ans = pull(myData[myData$Speciality == spec & myData$Course == course,"Num.Ans"])
                paste0(spec,", ", course, " курс. Количество ответов = ",num.ans)
            })
        })
        
        #output$Productivity = renderDataTable({
        #    validate(need(is.list(tables), "Нет данных"))
        #    tables$Productivity
        #})
    })
    observeEvent(input$Course,{
        withProgress(message = 'Rendering, please wait!', {
            spec = isolate({input$Speciality})
            course = isolate({input$Course})
            link = pull(myData[myData$Speciality == spec & myData$Course == course,"Path"])
            print(link)
            if(is.na(link)){
                tbl.Comp.Prod = NA
            }
            else{
                #tbl.Comp.Prod = read_sheet(link) %>% as.data.frame()
                tbl.Comp.Prod = readxl::read_xlsx(link)
            }
            output$CompProd = renderDataTable({
                validate(need(!is.na(tbl.Comp.Prod), "Нет данных"))
                
                tbl.Comp.Prod
            })
            output$Num.Ans = renderText({
                validate(need(!is.na(tbl.Comp.Prod), ""))
                num.ans = pull(myData[myData$Speciality == spec & myData$Course == course,"Num.Ans"])
                paste0(spec,", ", course, " курс. Количество ответов = ",num.ans)
            })
        })
        #output$Productivity = renderDataTable({
        #    validate(need(is.list(tables), "Нет данных"))
        #    tables$Productivity
        #})
    })
    # output$CathedraRating = renderDataTable({
    #     withProgress(message = 'Rendering, please wait!', {
    #                     answers = isolate({myData()})
    #                     t = calculateRating(answers)
    #     })
    #                     validate(need(nrow(t) > 0, "Нет данных"))
    #                     t
    #     })
    #TODO - make error control and OnDiscipline
    ## prevent timeout
    autoInvalidate <- reactiveTimer(intervalMs = 50*1000)
    observe({
        autoInvalidate()
        cat(".")
    })
})

# calculateRating = function(inputTable){
#
#     require(binom)
#
#     #needTbl = inputTable
#     needTbl = myData
#     needTbl$Cathedra = as.factor(needTbl$Cathedra)
#     departments = levels(needTbl$Cathedra)
#
#     out <- data.frame(matrix(ncol = 3, nrow = 0))
#     colnames(out) = c("Место", "Название кафедры", "Рейтинг")
#     problemsColumns = c("PO", "KP", "OUP", "MTO", "AOS")
#     #weight coefficients
#     wOUP = 1
#     wKP = 0.4
#     wPO = 0.7
#     wAOS = 0.6
#     wMTO = 0
#
#     i = 1
#     for (d in departments) {
#         dep = needTbl[needTbl$Cathedra == d,]
#
#         dep[,problemsColumns] = ifelse(dep[,problemsColumns] < 4,0,1)
#         dep$WeightingRate = (wPO * dep$PO + wKP * dep$KP + wOUP * dep$OUP + wMTO * dep$MTO + wAOS * dep$AOS) / (wPO + wKP + wOUP + wMTO + wAOS)
#         dep$Success = ifelse(dep$WeightingRate < 0.6,0,1)
#         estimation = sum(dep$Success,na.rm = T)
#         low_rate = round(binom.confint(estimation, nrow(dep),conf.level = 0.95, methods = "wilson")$lower,4)*100
#         out[i,2] = d
#         out[i,3] = low_rate
#         i = i + 1
#     }
#
#     out = out[order(out[,3],decreasing = T),]
#     out[,1] = order(out[,3],decreasing = T)
#     return(out)
# }
#
# calculateDiscipline = function(inputTable,spec,course){
#     inputTable = as.data.frame(inputTable)
#     needTbl = inputTable[inputTable$Speciality == spec & inputTable$Course == course,]
#     if(nrow(needTbl) == 0)
#         return(-1)
#     #TODO - weight coefficients is matrix. Answer in the Preparing column is number of the element in weight vector-row
#     preparingWeights = data.frame(matrix(c(1,1,1,1,1),nrow = 1,byrow = T))
#
#
#     needTbl$Subject = as.factor(needTbl$Subject)
#     for (i in 1:nrow(needTbl)) {
#         needTbl[i,"WeightComplication"] = needTbl[i,"Complication"] * preparingWeights[1, needTbl[i,"Preparing"]]
#         needTbl[i,"WeightProductivity"] = needTbl[i,"Productivity"] * preparingWeights[1, needTbl[i,"Preparing"]]
#     }
#
#     comp = data.frame(matrix(ncol = 2, nrow = 0))
#     colnames(comp) <- c("Дисциплина", "Сложность")
#     prod = data.frame(matrix(ncol = 2, nrow = 0))
#     colnames(prod) <- c("Дисциплина", "Продуктивность")
#
#     i = 1
#     for (s in levels(needTbl$Subject)) {
#         t = subset.data.frame(needTbl,Subject == s)
#         comp[i,1] = s
#         prod[i,1] = s
#         comp[i,2] = mean(t$WeightComplication)
#         prod[i,2] = mean(t$WeightProductivity)
#         i = i + 1
#     }
#     return(list(Complication = comp[order(comp[,2],decreasing = T),], Productivity = prod[order(prod[,2],decreasing = T),]))
# }
