library(readr)
dataSet <- read_csv("C:/Users/User/Desktop/DSC 424/Project/dataSet.csv")
View(dataSet)


# Changing all the categorical values into factor
dataSet$`Elementary, Middle, or High School` <- as.factor(dataSet$`Elementary, Middle, or High School`)
dataSet$`CPS Performance Policy Status` <- as.factor(dataSet$`CPS Performance Policy Status`)
dataSet$`Healthy Schools Certified?` <- as.factor(dataSet$`Healthy Schools Certified?`)
dataSet$`Safety Icon` <- as.factor(dataSet$`Safety Icon`)
dataSet$`Environment Icon` <- as.factor(dataSet$`Environment Icon`)
dataSet$`Instruction Icon` <- as.factor(dataSet$`Instruction Icon`)
dataSet$`Leaders Icon` <- as.factor(dataSet$`Leaders Icon`)
dataSet$`Teachers Icon` <- as.factor(dataSet$`Teachers Icon`)
dataSet$`Parent Engagement Icon` <- as.factor(dataSet$`Parent Engagement Icon`)
dataSet$`Parent Environment Icon` <- as.factor(dataSet$`Parent Environment Icon`)

dataSet$`Safety Score`[which(is.na(dataSet$`Safety Score`))] = mean(dataSet$`Safety Score`,na.rm = TRUE)
dataSet$`Family Involvement Score`[which(is.na(dataSet$`Family Involvement Score`))] = mean(dataSet$`Family Involvement Score`,na.rm = TRUE)
dataSet$`Environment Score`[which(is.na(dataSet$`Environment Score`))] = mean(dataSet$`Environment Score`,na.rm = TRUE)
dataSet$`Instruction Score`[which(is.na(dataSet$`Instruction Score`))] = mean(dataSet$`Instruction Score`,na.rm = TRUE)
dataSet$`Leaders Score`[which(is.na(dataSet$`Leaders Score`))] = mean(dataSet$`Leaders Score`,na.rm = TRUE)
dataSet$`Teachers Score`[which(is.na(dataSet$`Teachers Score`))] = mean(dataSet$`Teachers Score`,na.rm = TRUE)
dataSet$`Parent Engagement Score`[which(is.na(dataSet$`Parent Engagement Score`))] = mean(dataSet$`Parent Engagement Score`,na.rm = TRUE)
dataSet$`Parent Environment Score`[which(is.na(dataSet$`Parent Environment Score`))] = mean(dataSet$`Parent Environment Score`,na.rm = TRUE)
dataSet$`Average Student Attendance`[which(is.na(dataSet$`Average Student Attendance`))] = mean(dataSet$`Average Student Attendance`,na.rm = TRUE)
dataSet$`Average Teacher Attendance`[which(is.na(dataSet$`Average Teacher Attendance`))] = mean(dataSet$`Average Teacher Attendance`,na.rm = TRUE)
dataSet$`Individualized Education Program Compliance Rate`[which(is.na(dataSet$`Individualized Education Program Compliance Rate`))] = mean(dataSet$`Individualized Education Program Compliance Rate`,na.rm = TRUE)
dataSet$`Pk-2 Literacy %`[which(is.na(dataSet$`Pk-2 Literacy %`))] = mean(dataSet$`Pk-2 Literacy %`,na.rm = TRUE)
dataSet$`Pk-2 Math %`[which(is.na(dataSet$`Pk-2 Math %`))] = mean(dataSet$`Pk-2 Math %`,na.rm = TRUE)
dataSet$`Gr3-5 Grade Level Math %`[which(is.na(dataSet$`Gr3-5 Grade Level Math %`))] = mean(dataSet$`Gr3-5 Grade Level Math %`,na.rm = TRUE)
dataSet$`Gr3-5 Grade Level Read %`[which(is.na(dataSet$`Gr3-5 Grade Level Read %`))] = mean(dataSet$`Gr3-5 Grade Level Read %`,na.rm = TRUE)
dataSet$`Gr3-5 Keep Pace Read %`[which(is.na(dataSet$`Gr3-5 Keep Pace Read %`))] = mean(dataSet$`Gr3-5 Keep Pace Read %`,na.rm = TRUE)
dataSet$`Gr3-5 Keep Pace Math %`[which(is.na(dataSet$`Gr3-5 Keep Pace Math %`))] = mean(dataSet$`Gr3-5 Keep Pace Math %`,na.rm = TRUE)
dataSet$`Gr6-8 Grade Level Math %`[which(is.na(dataSet$`Gr6-8 Grade Level Math %`))] = mean(dataSet$`Gr6-8 Grade Level Math %`,na.rm = TRUE)
dataSet$`Gr6-8 Grade Level Read %`[which(is.na(dataSet$`Gr6-8 Grade Level Read %`))] = mean(dataSet$`Gr6-8 Grade Level Read %`,na.rm = TRUE)
dataSet$`Gr6-8 Keep Pace Math%`[which(is.na(dataSet$`Gr6-8 Keep Pace Math%`))] = mean(dataSet$`Gr6-8 Keep Pace Math%`,na.rm = TRUE)
dataSet$`Gr6-8 Keep Pace Read %`[which(is.na(dataSet$`Gr6-8 Keep Pace Read %`))] = mean(dataSet$`Gr6-8 Keep Pace Read %`,na.rm = TRUE)
dataSet$`Gr-8 Explore Math %`[which(is.na(dataSet$`Gr-8 Explore Math %`))] = mean(dataSet$`Gr-8 Explore Math %`,na.rm = TRUE)
dataSet$`Gr-8 Explore Read %`[which(is.na(dataSet$`Gr-8 Explore Read %`))] = mean(dataSet$`Gr-8 Explore Read %`,na.rm = TRUE)
dataSet$`ISAT Exceeding Math %`[which(is.na(dataSet$`ISAT Exceeding Math %`))] = mean(dataSet$`ISAT Exceeding Math %`,na.rm = TRUE)
dataSet$`ISAT Exceeding Reading %`[which(is.na(dataSet$`ISAT Exceeding Reading %`))] = mean(dataSet$`ISAT Exceeding Reading %`,na.rm = TRUE)
dataSet$`ISAT Value Add Math`[which(is.na(dataSet$`ISAT Value Add Math`))] = mean(dataSet$`ISAT Value Add Math`,na.rm = TRUE)
dataSet$`ISAT Value Add Read`[which(is.na(dataSet$`ISAT Value Add Read`))] = mean(dataSet$`ISAT Value Add Read`,na.rm = TRUE)
dataSet$`Students Taking  Algebra %`[which(is.na(dataSet$`Students Taking  Algebra %`))] = mean(dataSet$`Students Taking  Algebra %`,na.rm = TRUE)
dataSet$`Students Passing  Algebra %`[which(is.na(dataSet$`Students Passing  Algebra %`))] = mean(dataSet$`Students Passing  Algebra %`,na.rm = TRUE)
dataSet$`9th Grade EXPLORE (2009)`[which(is.na(dataSet$`9th Grade EXPLORE (2009)`))] = mean(dataSet$`9th Grade EXPLORE (2009)`,na.rm = TRUE)
dataSet$`9th Grade EXPLORE (2010)`[which(is.na(dataSet$`9th Grade EXPLORE (2010)`))] = mean(dataSet$`9th Grade EXPLORE (2010)`,na.rm = TRUE)
dataSet$`10th Grade PLAN (2009)`[which(is.na(dataSet$`10th Grade PLAN (2009)`))] = mean(dataSet$`10th Grade PLAN (2009)`,na.rm = TRUE)
dataSet$`10th Grade PLAN (2010)`[which(is.na(dataSet$`10th Grade PLAN (2010)`))] = mean(dataSet$`10th Grade PLAN (2010)`,na.rm = TRUE)
dataSet$`Net Change EXPLORE and PLAN`[which(is.na(dataSet$`Net Change EXPLORE and PLAN`))] = mean(dataSet$`Net Change EXPLORE and PLAN`,na.rm = TRUE)
dataSet$`11th Grade Average ACT (2011)`[which(is.na(dataSet$`11th Grade Average ACT (2011)`))] = mean(dataSet$`11th Grade Average ACT (2011)`,na.rm = TRUE)
dataSet$`Net Change PLAN and ACT`[which(is.na(dataSet$`Net Change PLAN and ACT`))] = mean(dataSet$`Net Change PLAN and ACT`,na.rm = TRUE)
dataSet$`College Eligibility %`[which(is.na(dataSet$`College Eligibility %`))] = mean(dataSet$`College Eligibility %`,na.rm = TRUE)
dataSet$`Graduation Rate %`[which(is.na(dataSet$`Graduation Rate %`))] = mean(dataSet$`Graduation Rate %`,na.rm = TRUE)
dataSet$`College Enrollment Rate %`[which(is.na(dataSet$`College Enrollment Rate %`))] = mean(dataSet$`College Enrollment Rate %`,na.rm = TRUE)
dataSet$`College Enrollment (number of students)`[which(is.na(dataSet$`College Enrollment (number of students)`))] = mean(dataSet$`College Enrollment (number of students)`,na.rm = TRUE)

summary(dataSet)


#Model:
#First order terms
# predicting enrollment
#model <- lm (`College Enrollment (number of students)`~`CPS Performance Policy Status`+`Average Student Attendance`+`Rate of Misconducts (per 100 students)`+`Average Teacher Attendance` + `ISAT Exceeding Math %` +	`ISAT Exceeding Reading %` +`ISAT Value Add Math`+	`ISAT Value Add Read`+ `Students Taking  Algebra %` + `Students Passing  Algebra %` + `9th Grade EXPLORE (2009)` +	`9th Grade EXPLORE (2010)` +	`10th Grade PLAN (2009)`+ 	`10th Grade PLAN (2010)`+ 	`Net Change EXPLORE and PLAN`	+`11th Grade Average ACT (2011)` 	+`Net Change PLAN and ACT`+	`College Eligibility %`	+`Graduation Rate %`	+`College Enrollment Rate %`, data=dataSet)
# predicting enrollment rate
#model <- lm (`College Enrollment Rate %`~`CPS Performance Policy Status`+`Average Student Attendance`+`Rate of Misconducts (per 100 students)`+`Average Teacher Attendance` + `ISAT Exceeding Math %` +	`ISAT Exceeding Reading %` +`ISAT Value Add Math`+	`ISAT Value Add Read`+ `Students Taking  Algebra %` + `Students Passing  Algebra %` + `9th Grade EXPLORE (2009)` +	`9th Grade EXPLORE (2010)` +	`10th Grade PLAN (2009)`+ 	`10th Grade PLAN (2010)`+ 	`Net Change EXPLORE and PLAN`	+`11th Grade Average ACT (2011)` 	+`Net Change PLAN and ACT`+	`College Eligibility %`	+`Graduation Rate %`, data=dataSet)

##Full Model
#First order terms
model1 <- lm (`College Enrollment (number of students)`~`CPS Performance Policy Status`+`Average Student Attendance`+`Rate of Misconducts (per 100 students)`+`Average Teacher Attendance` + `ISAT Exceeding Math %` +    `ISAT Exceeding Reading %` +`ISAT Value Add Math`+    `ISAT Value Add Read`+ `Students Taking  Algebra %` + `Students Passing  Algebra %` +   `9th Grade EXPLORE (2010)` +    `10th Grade PLAN (2009)`+     `10th Grade PLAN (2010)`+     `Net Change EXPLORE and PLAN`    +`11th Grade Average ACT (2011)`     +`Net Change PLAN and ACT`+    `College Eligibility %`    +`Graduation Rate %`    +`College Enrollment Rate %`, data=dataSet)

#Second order terms
model1 <- lm (`College Enrollment (number of students)`~`CPS Performance Policy Status` + `Average Student Attendance`+`Rate of Misconducts (per 100 students)`+ `ISAT Exceeding Math %` +    `ISAT Exceeding Reading %` +`ISAT Value Add Math`+  `Net Change EXPLORE and PLAN`   +`College Enrollment Rate %`+  I(`Average Student Attendance`^2)+ I(`Rate of Misconducts (per 100 students)` ^2) + I(`ISAT Exceeding Math %` ^2)+ I(`ISAT Exceeding Reading %` ^2)+ I(`ISAT Value Add Math` ^2)+ I(`Net Change EXPLORE and PLAN` ^2) + I(`College Enrollment Rate %` ^2), data=dataSet)
#model1 <- lm (`College Enrollment (number of students)`~`CPS Performance Policy Status` + `Average Student Attendance`+`Rate of Misconducts (per 100 students)`+ `ISAT Exceeding Math %` +    `ISAT Exceeding Reading %` +`ISAT Value Add Math`+  `Net Change EXPLORE and PLAN`   +`College Enrollment Rate %`+ I(`Rate of Misconducts (per 100 students)` ^2) + I(`ISAT Exceeding Math %` ^2)+ I(`ISAT Exceeding Reading %` ^2)+ I(`ISAT Value Add Math` ^2)+ I(`Net Change EXPLORE and PLAN` ^2) + I(`College Enrollment Rate %` ^2), data=dataSet) 
summary(model1)

modelFinal <- lm (`College Enrollment (number of students)` ~ `CPS Performance Policy Status` + `Average Student Attendance` + `Rate of Misconducts (per 100 students)` + `ISAT Value Add Math` + `College Enrollment Rate %` + I(`Rate of Misconducts (per 100 students)`^2) + I(`ISAT Exceeding Math %`^2) + I(`ISAT Exceeding Reading %`^2) +  I(`Net Change EXPLORE and PLAN`^2) + I(`College Enrollment Rate %`^2) , data=dataSet)


#Backward Selection
library(MASS)
help(stepAIC)
#Hygiene
step <- stepAIC(model1, direction="backward")


#Forward Selection
dataSet_noNA <-na.omit(dataSet)
m0 <- lm (`College Enrollment (number of students)`~`CPS Performance Policy Status` + `Average Student Attendance`+`Rate of Misconducts (per 100 students)`+ `ISAT Exceeding Math %` +    `ISAT Exceeding Reading %` +`ISAT Value Add Math`+  `Net Change EXPLORE and PLAN`   +`College Enrollment Rate %`+ I(`Rate of Misconducts (per 100 students)` ^2) + I(`ISAT Exceeding Math %` ^2)+ I(`ISAT Exceeding Reading %` ^2)+ I(`ISAT Value Add Math` ^2)+ I(`Net Change EXPLORE and PLAN` ^2) + I(`College Enrollment Rate %` ^2), data=dataSet) 

m6 <- lm (`College Enrollment (number of students)`~`CPS Performance Policy Status` + `Average Student Attendance`+`Rate of Misconducts (per 100 students)`+ `ISAT Exceeding Math %` +    `ISAT Exceeding Reading %` +`ISAT Value Add Math`+  `Net Change EXPLORE and PLAN`   +`College Enrollment Rate %`+ I(`Rate of Misconducts (per 100 students)` ^2) + I(`ISAT Exceeding Math %` ^2)+ I(`ISAT Exceeding Reading %` ^2)+ I(`ISAT Value Add Math` ^2)+ I(`Net Change EXPLORE and PLAN` ^2) + I(`College Enrollment Rate %` ^2), data=dataSet_noNA) 
m7 <- lm(`College Enrollment (number of students)`~ 1, data=dataSet_noNA)
step <- stepAIC(m7,direction="forward", scope=list(upper=m0,lower=m7))

summary(m7)


#modelFinal <- lm (`College Enrollment (number of students)` ~ `CPS Performance Policy Status` + `Average Student Attendance` + `Rate of Misconducts (per 100 students)` + `ISAT Value Add Math` + `College Enrollment Rate %` + I(`Average Student Attendance`^2) +   I(`Rate of Misconducts (per 100 students)`^2) + I(`ISAT Exceeding Math %`^2) + I(`ISAT Exceeding Reading %`^2) + I(`Net Change EXPLORE and PLAN`^2) +   I(`College Enrollment Rate %`^2), data=dataSet)
summary(modelFinal)


#Multicolinearity
library(car)
vif(modelFinal)