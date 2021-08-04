###############################################################################
##########    MAKING TABLES FOR TEMPLATES IN ILLUSTRATOR             ##########
##########                DISCIPLINE POLL                            ########## 
##########              author - agent 036                           ##########
###############################################################################
library(readxl)
library(xlsx)
library(stringr)
library(dplyr)

#load new data, made how data from ks, see tech task
# Faculcy Speciality Date Course Cathedra Subject Lector Complication Productivity Preparing Comments1 PO KP OUP MTO AOS Comments2
answers = read_excel("C:\\Users\\SmirnygaTotoshka\\Documents\\CommonWorkspace\\Univer\\ЦИОМ\\final.xlsx")
answers$Speciality = as.factor(answers$Speciality)


#load old data for compare
report.all = read.csv("C:\\Users\\SmirnygaTotoshka\\Downloads\\report_all.csv",fileEncoding = "UTF-8")
report.every = read.csv("C:\\Users\\SmirnygaTotoshka\\Downloads\\report_every_-_report_every.csv",fileEncoding = "UTF-8")
#----------------------------------------------------------------------------------------------
#fix speciality names differencies - optional, depend on current data
report.all[report.all$Специальность == "Лечебное дело МФ","Специальность"] = "Лечебное дело (IMAT)"
report.all[report.all$Специальность == "Лечебное дело МФ(иностр.)","Специальность"] = "Лечебное дело (англофоны)"
report.all[report.all$Специальность == "Биология","Специальность"] = "Биология (бакалавриат)"
report.every[report.every$Специальность == "Лечебное дело МФ","Специальность"] = "Лечебное дело (IMAT)"
report.every[report.every$Специальность == "Лечебное дело МФ(иностр.)","Специальность"] = "Лечебное дело (англофоны)"
report.every[report.every$Специальность == "Биология","Специальность"] = "Биология (бакалавриат)"
#----------------------------------------------------------------------------------------------
#Init summary tables
report.Specs.And.Course = data.frame(matrix(nrow = 0,ncol = ncol(report.all)))
colnames(report.Specs.And.Course) = colnames(report.all)

report.Subject = data.frame(matrix(nrow = 0,ncol = ncol(report.every)))
colnames(report.Subject) = colnames(report.every)

#labels for students preparing on two languages
preparing.rus = data.frame(matrix(c("Нет","Да, но редко","Да, иногда","Да, часто","Да, всегда"),ncol=1))
preparing.en = data.frame(matrix(c("No","Yes but rarely","Yes sometimes","Often","Yes always"),ncol=1))

#paths for saving histograms
comp.path = "C:\\Users\\SmirnygaTotoshka\\Desktop\\CIOM_DisciplineInfograf\\autumn2020\\Comp"
prod.path = "C:\\Users\\SmirnygaTotoshka\\Desktop\\CIOM_DisciplineInfograf\\autumn2020\\Prod"

#select speciality
for (s in levels(answers$Speciality)) {
  specs = answers[answers$Speciality == s,]
  specs$Course = as.factor(specs$Course)
  #select speciality & course
  for (c in levels(specs$Course)) {
    c = as.numeric(c)
    specs.and.course = specs[specs$Course == c,]
    #init infograf info
    specs.and.course$Subject = as.factor(specs.and.course$Subject)
    num.subjects = length(levels(specs.and.course$Subject))
    num.ans = ceiling(nrow(specs.and.course) / num.subjects)
    num.pages = ceiling(num.subjects / 7)
    ind.sub = 1#counter for report.Subject
    page = 1
    row.Spec = c(page,num.pages,s,c,num.ans)
    r = 1#counter for report.Specs.And.Course
    #select speciality & course & discipline
    for (sub in levels(specs.and.course$Subject)) {
      subject = specs.and.course[specs.and.course$Subject == sub,]
      comp = round(mean(subject$Complication),2)
      prod = round(mean(subject$Productivity),2)
      
      #count difference in the characteristics. 
      old.data = report.every[report.every$Специальность == s & report.every$Курс == c & report.every$Дисциплина == sub,]
      if(nrow(old.data) == 0){
        dComp = 0
        dProd = 0
      }
      else if(nrow(old.data) == 1){
        dComp = round(comp - old.data$Сложность_дисциплины,2)
        dProd = round(prod - old.data$Продуктивность,2)
      }
      else{
        print(paste("Bug ",s,c,sub))
      }
      
      #Count answers histograms depend on prepare to lessons
      subject$Preparing = as.factor(subject$Preparing)
      tbl.complication = data.frame(matrix(0L,ncol = 5,nrow = 5))
      tbl.productivity = data.frame(matrix(0L,ncol = 5,nrow = 5))
      for (i in 1:nrow(subject)) {
        if(grepl("(англофоны)",subject[i,"Speciality"],fixed = T))
          prep = preparing.en
        else
          prep = preparing.rus
        j = pull(subject[i,"Complication"])
        k = which(prep[,1] == pull(subject[i,"Preparing"]))
        tbl.complication[j,k] = tbl.complication[j,k] + 1
        
        j = pull(subject[i,"Productivity"])
        tbl.productivity[j,k] = tbl.productivity[j,k] + 1
        
      }
      #save histograms
      cp = paste0(comp.path,"\\Complication_",s,"_",c,"_",sub,".csv")
      pp = paste0(prod.path,"\\Productivity_",s,"_",c,"_",sub,".csv")
      write.table(tbl.complication,cp,row.names = F,col.names = F,sep = ",")
      write.table(tbl.productivity,pp,row.names = F,col.names = F,sep = ",")
      
      #calculate how many students meet with problems in procent
      oup = paste0(round(nrow(subject[subject$OUP == 2,]) / nrow(subject) * 100,digits = 2),"%")
      kp = paste0(round(nrow(subject[subject$KP == 2,]) / nrow(subject) * 100,digits = 2),"%")
      po = paste0(round(nrow(subject[subject$PO == 2,]) / nrow(subject) * 100,digits = 2),"%")
      mto = paste0(round(nrow(subject[subject$MTO == 2,]) / nrow(subject) * 100,digits = 2),"%")
      aos = paste0(round(nrow(subject[subject$AOS == 2,]) / nrow(subject) * 100,digits = 2),"%")
      
      #form summary tables
      row.Sub = c(ind.sub,num.subjects,s,c,paste(num.ans,"ответов"),sub,prod,dProd,pp,comp,dComp,cp,oup,po,kp,mto,aos)
      report.Subject[nrow(report.Subject) + 1,] = row.Sub
      row.Spec = c(row.Spec, sub, prod, comp)
      if(r == 7 || r == num.subjects){
        report.Specs.And.Course[nrow(report.Specs.And.Course)+1,] = row.Spec
        r = 0
        row.Spec = c(page,num.pages,s,c,num.ans)
        page = page + 1
      }
      if (r != 7 && ind.sub == num.subjects){
        row.Spec = c(row.Spec, rep("-", times = (7 - r) * 3))
        report.Specs.And.Course[nrow(report.Specs.And.Course)+1,] = row.Spec
      }
      r = r + 1
      ind.sub = ind.sub + 1
    }

  }
}
#Save summary
write.table(report.Subject,"C:\\Users\\SmirnygaTotoshka\\Desktop\\CIOM_DisciplineInfograf\\autumn2020\\report_every.csv",row.names = F,sep=",")
write.table(report.Specs.And.Course,"C:\\Users\\SmirnygaTotoshka\\Desktop\\CIOM_DisciplineInfograf\\autumn2020\\report_all.csv",row.names = F,sep=",")
