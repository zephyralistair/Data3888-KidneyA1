#load library
library(GEOquery)
library(Biobase)
library(limma)
library(randomForest)
library(e1071)
library(R.utils)
library(reshape2)
library(ggplot2)
library(dplyr)


#function to find differentially expressed genes
top_gene<-function(data,label,num){
  design <- model.matrix(~ factor(label))
  fit <- lmFit(data, design)
  fit2 <- eBayes(fit)
  tT <- topTable(fit2, coef=2, number = num, sort.by ="t")
  retain_protein <- rownames(tT)
  return(retain_protein)
}



# GSE53605 ----------------------------------------------------------------
gse_GSE53605 <- getGEO("GSE53605")
gse_GSE53605 <- gse_GSE53605$GSE53605_series_matrix.txt.gz

head(exprs(gse_GSE53605))
head(pData(gse_GSE53605)[,1:10])
head(fData(gse_GSE53605)[,1:10])

#store different data into different dataframe
edata1=exprs(gse_GSE53605)
fdata1=fData(gse_GSE53605)
rejection_status  <- gse_GSE53605$characteristics_ch1.1
label1=unlist(lapply( strsplit(as.character(rejection_status), ": " ) , `[[` , 2)  )
label1= ifelse(grepl("AR", gse_GSE53605$title), "Rejection", "Stable")
label1<-as.factor(label1)
cdata1=pData(gse_GSE53605)

#checking missing values
table(is.na(edata1))

#transform the expression data to log form and remove those rows that have >10% missing values 
n=dim(edata1)[2]*0.1
edata1_log = log2(edata1[rowSums(is.na(edata1)) < n,])
dim(edata1_log)

#normalize the expression data
edata1_norm = normalizeBetweenArrays(edata1_log, method = "scale")

#return the top 50 differentially expressed genes

mv <- rowSums(is.na(edata1_norm)) == 0

gene1<-top_gene(as.matrix(edata1_norm[mv, ]),label1,50)

# GSE138043 ----------------------------------------------------------------
gse_GSE138043 <- getGEO("GSE138043")
gse_GSE138043 <- gse_GSE138043$GSE138043_series_matrix.txt.gz

gse_GSE138043$characteristics_ch1

# GSE14328 ----------------------------------------------------------------
gse_GSE14328 <- getGEO("GSE14328")
gse_GSE14328 <- gse_GSE14328$GSE14328_series_matrix.txt.gz

gse_GSE14328$characteristics_ch1
dim(exprs(gse_GSE14328))


# GSE145927 ----------------------------------------------------------------
gse_GSE145927 <- getGEO("GSE145927")
gse_GSE145927 <- gse_GSE145927$GSE145927_series_matrix.txt.gz

gse_GSE145927$characteristics_ch1
dim(exprs(gse_GSE145927))

# GSE1563 ----------------------------------------------------------------
gse_GSE1563 <- getGEO("GSE1563")
gse_GSE1563 <- gse_GSE1563$GSE1563_series_matrix.txt.gz

gse_GSE1563$description
dim(exprs(gse_GSE1563))

# GSE19130 ----------------------------------------------------------------
gse_GSE19130  <- getGEO("GSE19130")
gse_GSE19130 <- gse_GSE19130$GSE19130_series_matrix.txt.gz

gse_GSE19130$characteristics_ch1.2

dim(exprs(gse_GSE19130))

# GSE26578----------------------------------------------------------------
gse_GSE26578  <- getGEO("GSE26578")
gse_GSE26578 <- gse_GSE26578$GSE26578_series_matrix.txt.gz

gse_GSE26578$characteristics_ch1.2

dim(exprs(gse_GSE26578))



# GSE34437----------------------------------------------------------------
gse_GSE34437  <- getGEO("GSE34437")
gse_GSE34437 <- gse_GSE34437$GSE34437_series_matrix.txt.gz

gse_GSE34437$characteristics_ch1

dim(exprs(gse_GSE34437))
