#################################
##1000 Genomes Fst Calculations##
#####for all subpopulations######
#################################

ALL.ids <- read.csv("/n/holystore01/LABS/xlin/Lab/dshyr/1KG/1KG_fst/1000Genomes_Indep_ids.csv", header=TRUE)
sub.pops <- unique(ALL.ids$Population)
###Load in 1000 Genomes Pruned Genotype##
library(KRIS)
geno = read.bed(bed = "/n/holystore01/LABS/xlin/Lab/dshyr/1KG/1KG_cleaned/1KG_pruned_forPCA.bed", bim = "/n/holystore01/LABS/xlin/Lab/dshyr/1KG/1KG_cleaned/1KG_pruned_forPCA.bim",
                fam = "/n/holystore01/LABS/xlin/Lab/dshyr/1KG/1KG_cleaned/1KG_pruned_forPCA.fam")
#Sample ids#
ind.info <- geno$ind.info

Fst.matrix <- diag(0, nrow = length(sub.pops), ncol = length(sub.pops))
row.names(Fst.matrix) <- colnames(Fst.matrix) <- sub.pops

for(i in 1:(length(sub.pops) - 1)){
  print(paste("Outer Loop =", i))
  for(j in (i + 1):length(sub.pops)){
    samps.1 <- ALL.ids[which(ALL.ids$Population == sub.pops[i]), "Sample"]
    samps.2 <- ALL.ids[which(ALL.ids$Population == sub.pops[j]), "Sample"]
    samp1.rows <- which(ind.info$FamID %in% samps.1)
    samp2.rows <- which(ind.info$FamID %in% samps.2)
    Fst.matrix[i, j] <- Fst.matrix[j, i] <- fst.hudson(geno$snp, samp1.rows, samp2.rows)
  }
}

write.csv(Fst.matrix, file = "/n/holystore01/LABS/xlin/Lab/dshyr/1KG/1KG_fst/1KG_Fstmatrix.csv")