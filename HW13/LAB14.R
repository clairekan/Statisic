# Lab 14: ANOVA



student = read.table("Student.csv", sep = ",", header = T)  # 690 observations
student = na.omit(student) # 682 observations


# 1. Examine necessary conditions --------------------------------------------

# data distribution
boxplot(GPA ~ Sex, data = student, 
        main = "GPA among male and female", cex.main = 1.6,
        col = c("Firebrick3", "Navy"))

# mean and sd for each group
stu.mean = tapply(student$GPA, student$Sex, mean); stu.mean
stu.sd = tapply(student$GPA, student$Sex, sd); stu.sd
stu.n = tapply(student$GPA, student$Sex, length); stu.n

stu = data.frame(n = stu.n, mean = stu.mean, sd = stu.sd)
stu

#install.packages("ggpubr")
library(ggpubr)

# visualize confidence interval for each group
ggline(student, x = "Sex", y = "GPA", add = c("mean_ci"), 
       color = "navy", main = "Confidence interval among groups",
       ggtheme = theme_gray())

# visualize std for each group
ggline(student, x = "Sex", y = "GPA", add = c("mean_sd"), 
       color = "firebrick3", main = "Std among groups",
       ggtheme = theme_gray())

?ggline
?desc_statby

# 2. One-Way ANOVA --------------------------------------------------------

oneway = aov(GPA ~ Sex, data = student)
summary(oneway)

results = lm(GPA ~ Sex, data = student)
summary(results)

anova(results)

# 3. Two-way ANOVA --------------------------------------------------------

twoway = aov(GPA ~ Sex + ReligImp + Sex:ReligImp, data = student)
summary(twoway)


#factor (base class for regression)
unique(student$ReligImp)
student$ReligImp = factor(student$ReligImp, levels = c("Not","Fairly", "Very"))
unique(student$ReligImp)

results2 = lm(GPA ~ Sex + ReligImp + Sex:ReligImp, data = student)
summary(results2)

anova(results2)

#?interaction.plot
#interaction.plot(data$var1, data$var2, data$response)
interaction.plot(student$ReligImp, student$Sex, student$GPA, 
                 #leg.bty = "o",
                 legend = F, lty = c(4,5), lwd = 2, col = c("red", "blue"),
                 xlab = "ReligImp", ylab = "GPA", 
                 main = "Interaction plot", cex.main = 1.6)

legend("right", inset = 0.02, 
       legend = c("Female", "Male"), col = c("red", "blue"), 
       lty = c(4,5), lwd = 2)



# key in data  ----------------------------------------------------------------

# Example 16.7 from Ch.16
weight = c(7,9,5,7,
           9,11,7,
           15,12,18)
program = c("pr1", "pr1", "pr1", "pr1", 
            "pr2", "pr2", "pr2", 
            "pr3", "pr3", "pr3")

class(program)
unique(program)
program = factor(program, levels = c("pr1", "pr2", "pr3"))

unique(program)

aa = aov(weight ~ program)
summary(aa)

bb = lm(weight ~ program)
summary(bb)