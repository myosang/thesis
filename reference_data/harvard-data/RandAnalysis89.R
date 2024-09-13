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

#############################
#Cleaning Data
#############################

setwd("../Data")

dataldac <- read.ldac("final_tdm.csv")
vocabcoarse <- read.csv("tdm89_vocab.csv")$term
meta <- read.csv("tdm89_metadata.csv")
meta$treatment <- as.numeric(meta$condition_number==9)

data <- prep.documents(dataldac, vocabcoarse, meta=meta)
documents <- data$documents
vocab <- data$vocab
meta <- data$meta

#####################################
#Model with treatment 1=pressure
#####################################

#Select Model
#set.seed(54321)
#models <- selectModel(documents, vocab, 5, prevalence=~meta$treatment, runs=50)
#plotModels(models)

mod <- models$runout[[3]]
mod$settings$seed

#Recover selected model
#seed:1601900
mod <- stm(documents, vocab, 5, prevalence=~meta$treatment, seed=1601900)
labeltopics(mod, n=24)

#Treatment effect plot
prep <- prep.plot(mod, meta$treatment, theta.uncertainty="Global", documents=documents, ci.level=.9)
ploteffect(prep, choosetopics=c(1,5), model=mod, xlab="Difference in Topic Proportions (Treated-Control)", labeltype="numbers", xlim=c(-.18,.15))
plottopics(mod, topics=c(1,5), width=50)

#Normalized contributions plot
prep2 <- prep.plot(mod, meta$normalized.contribution, topics=c(1,5),theta.uncertainty="Global", span=2, ci.level=.9, documents=documents, dv="covariate")
ploteffect(prep2, choosetopics=c(1,2),model=mod, xlab="Mean Topic Proportions", ylab="Mean Contributions", labeltype="prob", xlim=c(0,.6))
