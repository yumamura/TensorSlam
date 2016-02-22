#HOSVD for simple_sparse_array (slam)
library(irlba)
library(slam)
library(dplyr)

if(1==0){
	sample_tnsr <- array(runif(10*20*30),c(10,20,30))
	sample_tnsr <- as.simple_sparse_array(sample_tnsr)
}















