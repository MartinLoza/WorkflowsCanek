##################################
#title: "Figure 3. Pseudo-batches"
#author: "Martin Loza"
##################################

#This is the main workflow to reproduce the pseudo-batches test from Figure 3.

library(here)
library(Canek)
library(Seurat)
library(RNAseqAnalysis)
options(future.globals.maxSize = 4e10)

dimPCA <- 10 # Number of PCA dimensions used in the analysis.
per <- c(0.05, 0.15, 0.3) # Percentages of mean size used in kBET.
resolution = 0.5 # Resolution used in clustering.
algorithm <- 1 # Algorithm used in clustering.
frac = 0.5 # Fraction of cells used to create the pseudo-batches.
seed <- 777 # Original seed.
rep = 2 # Number of test repetitions.
seeds <- seq(from = 1, to = rep, by = 1) # Seeds used in the repetitions.
batchKBET <- "batch" # Label used in kBET.
batchSilhouette <- "seurat_clusters" # Label used in Silhouette.

dataFile <- here("Data/Results/Spleen_TM/Raw.Rds") # Where the analized data is storage. 
resultsFile <- here("Data/Results/Figure3") # Where the analysis results are storage.

## Load data
x <- readRDS(dataFile)
x

## Sampling data. For tests
set.seed(seed)
x <- RNAseqAnalysis::SampleData(object = x, frac = 0.05, seed = seed)
x

## Data preprocessing
set.seed(seed)
x <- RNAseqAnalysis::SeuratPreprocessing(object = x)

## Clustering
set.seed(seed)
x <- FindNeighbors(x, dims = 1:dimPCA, reduction = "pca", verbose = FALSE)
x <- FindClusters(x, resolution =resolution, algorithm = algorithm, verbose = FALSE)

## Corrections and metrics
ks <- rep(length(unique(x$seurat_clusters)),2) # Number of celltypes used in scMerge.

for(i in seq_len(rep)){
  
  seed <- seeds[i] # Seed to use in the test.
  
  ## Create the pseudo-baches
  set.seed(seed)
  idxSample <- sample(x = ncol(x), size = floor(frac*ncol(x)), replace = FALSE)
  xl <- list("B1" = x[,idxSample], "B2" = x[,-idxSample])
  # Set up labels
  xl[["B1"]]$batch <- "Pseudo_batch_1"
  xl[["B2"]]$batch <- "Pseudo_batch_2"
  xl
  
  ## Corrections
  set.seed(seed)
  source(here("Results/CorrectData.R"), knitr::knit_global())
  
  # We create a list with the correction results to ease downstream analyses.
  datal <- list(Uncorrected = Uncorrected,
                Canek = Canek,
                MNN = MNN,
                Seurat = Seurat,
                scMerge = scMerge, 
                ComBat = ComBat, 
                Liger = Liger, 
                Harmony = Harmony, 
                Scanorama = Scanorama, 
                ComBatseq = ComBatseq)
  
  ## Save corrections
  saveRDS(object = datal, file = paste0(resultsFile, "/", i, "_datal.Rds"))
  
  ## Metrics
  set.seed(seed)
  source(here("Results/Metrics.R"), knitr::knit_global())
  
  ## Save scores
  saveRDS(object = list(scoresKbet = scoresKbet, scoresSilhouette = scoresSilhouette), file =  paste0(resultsFile, "/", i, "_scores.RDS"))
}