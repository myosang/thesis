##############################
#Loading Packages

#############################

library(lda)
library(slam)
library(stringr)
#install.packages("~/stmdev/rcode/stm_0.03.07.tar.gz", type="source", repos=NULL)
library(stm)

source("~/stmdev/rcode/stm/R/ploteffect.R")
source("~/stmdev/rcode/stm/R/plottopics.R")
source("~/stmdev/rcode/stm/R/plotPerspectives.R")
#############################
#Cleaning Data
#############################

setwd("../Data")

dataldac <- read.ldac("final_tdm.csv")
vocabcoarse <- read.csv("tdm1011_vocab.csv")$term
meta <- read.csv("tdm1011_metadata.csv")
meta$treatment <- as.numeric(meta$condition_number==10)

data <- prep.documents(dataldac, vocabcoarse, meta=meta)
documents <- data$documents
vocab <- data$vocab
meta <- data$meta

#####################################
#Model with treatment 1=Intuitive
#####################################

#Select Model
#set.seed(01238)
#models <- selectModel(documents, vocab, 5, prevalence=~meta$treatment, runs=50)
#plotModels(models)
#mod <- models$runout[[4]]

#Recover selected model
#seed: 4671639
mod <- stm(documents, vocab, 5, prevalence=~meta$treatment, seed=4671649)
labeltopics(mod)

#Treatment effect plot
prep <- prep.plot(mod, meta$treatment, theta.uncertainty="Global", documents=documents, ci.level=.9)
ploteffect(prep,choosetopics=c(1,4),model=mod, xlab="Difference in Topic Proportions (Treated-Control)", labeltype="numbers", xlim=c(-.2, .15))
plottopics(mod, topics=c(1,4),width=50)

#Normalized contributions plot
#brandonexample <- prep.plot(mod, meta$normalized.contribution, topics=c(1,4),theta.uncertainty="Local", span=2, ci.level=.9, documents=documents, dv="covariate")
prep2 <- prep.plot(mod, meta$normalized.contribution, topics=c(1,4),theta.uncertainty="Global", span=2, ci.level=.9, documents=documents, dv="covariate")
ploteffect(prep2, choosetopics=c(1,2),model=mod, xlab="Mean Topic Proportions", ylab="Mean Normalized Contributions", labeltype="numbers", xlim=c(.05,.6), ylim=c(.15,1), linecol=c("blue", "red"))

################################################
#Model with bottom covariate gender and treatment 
##############################################

#Select model
#set.seed(02138)
#models <- selectModel(documents, vocab, 5, prevalence=~meta$treatment, content=~meta$female, runs=50)
#plotModels(models)
#mod2 <- models$runout[[9]]
#mod$settings$seed

#Recover selected model
#Seed 4005847
meta$gender <- ifelse(meta$female==1, "female", "male")
mod2 <- stm(documents, vocab, 5, prevalence=~meta$treatment, content=~meta$gender, seed=4005847)
labeltopics(mod2)

prep <- prep.plot(mod2, meta$treatment, theta.uncertainty="None", documents=documents, ci.level=.95)
ploteffect(prep,model=mod2, xlab="Treated-Control", labeltype="numbers")

plotPerspectives(mod2, 3, M=50, labels=c("Female", "Male"))

plottopics(mod2, 3, width=20, covariatelabs=c("Female", "Male"), n=40)
