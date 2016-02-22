#' 2-dimensional array(from Slam) -> sparseMatrix(from Matrix)

#'@export
#'@param mat WIP
#'@return WIP

as.sparseMatrix.simple_sparse_array <- function(mat){

	mat <- sparseMatrix(i = mat$i[,1],j = mat$i[,2],x = mat$v,dims = mat$dim)
	return(mat)
}


