library(googlesheets4)
library(dplyr)
library(googledrive)
linkToActualTable = "https://docs.google.com/spreadsheets/d/1zaoiUihucFIRIdQdYzxTIfPGbzhWR4kCZrn9fANQ4ZQ/edit?usp=sharing"
inputTable = read_sheet(linkToActualTable) %>% as.data.frame()
all.specs = levels(as.factor(inputTable$Speciality))
all.courses = 1:6
preparingWeights = data.frame(matrix(c(1,1,1,1,1),nrow = 1,byrow = T))
preparing.rus = data.frame(matrix(c("Нет","Да, но редко","Да, иногда","Да, часто","Да, всегда"),ncol=1))
preparing.en = data.frame(matrix(c("No","Yes but rarely","Yes sometimes","Often","Yes always"),ncol=1))
en.spec.names = data.frame(matrix(nrow=length(all.specs),ncol=2))
colnames(en.spec.names) = c("Ru","En")
en.spec.names$Ru = all.specs
en.spec.names$En = c("Biology (IMAT)", "Biology(bachelor)","KlinPsych","Lech","Lech (IMAT)",
                     "Lech (EN)","MedBiophys","medBiochem","MedCyber","Ped","Soc","Stom","Pharm")
meta = data.frame(matrix(ncol=4))
colnames(meta) = c("Speciality","Course","Path","Num.Ans")
line = 1
for (s in all.specs) {
  for (c in all.courses) {
    needTbl = inputTable[inputTable$Speciality == s & inputTable$Course == c,]
    if(nrow(needTbl) == 0){
      meta[line,] = c(s,c,NA,NA)
    }
    else{
      needTbl$Subject = as.factor(needTbl$Subject)
      
      for (i in 1:nrow(needTbl)) {
        if(grepl("(англофоны)",needTbl[i,"Speciality"],fixed = T))
          prep = preparing.en
        else
          prep = preparing.rus
        k = which(prep[,1] == needTbl[i,"Preparing"])
        needTbl[i,"WeightComplication"] = needTbl[i,"Complication"] * preparingWeights[1, k]
        needTbl[i,"WeightProductivity"] = needTbl[i,"Productivity"] * preparingWeights[1, k]
      }
      d = data.frame(matrix(0L,ncol = 3, nrow = length(levels(needTbl$Subject))))
      i = 1
      for (sub in levels(needTbl$Subject)) {
        t = subset.data.frame(needTbl,Subject == sub)
        d[i,1] = sub
        d[i,2] = round(mean(t$WeightComplication),2)
        d[i,3] = round(mean(t$WeightProductivity),2)
        i = i + 1
      }
      colnames(d) = c("Дисциплина","Сложность","Продуктивность")
      name = paste0(en.spec.names[en.spec.names["Ru"]==s,"En"],"_",c)
      xlsx::write.xlsx(d,paste0("C:\\Users\\SmirnygaTotoshka\\Documents\\CommonWorkspace\\Univer\\ЦИОМ\\Ratings\\testDeploy\\Data\\",name,".xlsx"),row.names=F) 
      #g =  gs4_create(name,sheets = d)
      #drive_mv(g,"Disciplines",name)
      
      meta[line,"Speciality"] = s
      meta[line,"Course"] = c
      #meta[line,"Link"] = paste0("https://docs.google.com/spreadsheets/d/",g,"/edit?usp=sharing")
      meta[line,"Path"] = paste0("Data/",name,".xlsx")
      meta[line,"Num.Ans"] = ceiling(nrow(needTbl) / length(levels(needTbl$Subject)))
      #Sys.sleep(2)
    }
    line = line + 1
  }
}
xlsx::write.xlsx(meta,"C:\\Users\\SmirnygaTotoshka\\Documents\\CommonWorkspace\\Univer\\ЦИОМ\\Ratings\\testDeploy\\Data\\meta.xlsx",row.names=F) 
#g =  gs4_create("meta",sheets = meta)
#drive_mv(g,"Disciplines","all_meta")
