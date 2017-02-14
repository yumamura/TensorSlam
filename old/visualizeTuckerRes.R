library(rTensor)
library(slam)
library(igraph)

visualizeRes <- function(res,tnsr=NULL,EPS=2^-16){

	#=データ読み込みと変形

	if(0){
		res <- readRDS('./samplerTuckerRes.dat')
		res <- readRDS('./10mMovie-Genre-TagTuckerRes.dat')
		tnsr <- readRDS('./10mMovie-Genre-Tag.dat')
	}
	
	if(class(res[[1]])=='Tensor'){
		core <- res$Z@data
		Wlist <- res$U
	}else if(class(res[[1]])=='array'){
		core <- res$g
		Wlist <- res$As
	}

	core[which(core<=EPS)] <- 0

	#=plotのためのパラメータ定義

	tensorDim <- length(dim(core)) #テンソルの次元
	ncolGrobal <- tensorDim + tensorDim - 1 #図全体の縦に何等分するか


	#=描画スペースの定義
	split.screen(c(1,ncolGrobal))
	par(oma=c(0,0,2,0))
	par(mar=c(2,1,0.5,0.5))
	
	#=接続plot描画
	conn <- tensorDim -1 #描画すべき接続のスクリーン数
	for(con in 1:conn){
		weightMat <- apply(core,c(con,con+1),sum)
		weight <- cbind(which(weightMat>-Inf,arr.ind=T),weightMat[which(weightMat>-Inf)])
		colnames(weight) <- c('left','right','weight')
		weight <- data.frame(weight)
		weight[,'left'] <-letters[weight[,'left']]
		weight[,'right'] <- LETTERS[weight[,'right']]
		weight[,'weight'] <- abs(weight[,'weight'])
		
		#ここからigraph
		edgeList <- weight[,c('left','right')]
		g <- graph.empty()
		g <- add.vertices(g,nv=length(unique(edgeList[,'left'])),attr=list(name=unique(edgeList[,'left'])),type=T)
		g <- add.vertices(g,nv=length(unique(edgeList[,'right'])),attr=list(name=unique(edgeList[,'right'])),type=F)
		edgeListVec <- as.vector(t(as.matrix(edgeList)))
		g <- add.edges(g,edgeListVec)

		lay <- layout.bipartite(g)
		lay <- lay[,c(2,1)]
		screenHeight <- par('din')[2]
		lay[,2] <- lay[,2]*screenHeight

		MAX_EDGE_WIDTH <- 30

		screen(2*con)
		par(oma=c(0,0,0,0))
		par(mar=c(0.1,0.1,0.1,0.1))
		plot.igraph(g,
			    edge.curved=seq(-0.5,0.5,length=ecount(g)),
			    edge.width=weight[,'weight']/max(weight[,'weight'])*MAX_EDGE_WIDTH,
			    edge.arrow.size=0,
			    vertex.size=0,
			    vertex.label=NA,
			    #                             ylim=c(0,0.01),   
			    layout=lay)

	}
	#=個別特徴のプロットに入る
	for(d in 1:tensorDim){ #次元のindex
		numFeature <- ncol(Wlist[[d]])
		scridx <- split.screen(c(numFeature,1),screen=d+d-1)
		print(scridx)
		for(f in 1:numFeature){ #特徴のindex
			screen(scridx[f])
			par(oma=c(0,0,0,0))
			par(mar=c(0.2,0.5,0.5,0.2))
			plot(Wlist[[d]][,f],type='l',lwd=2,axes=F)
			abline(h = 0,col='red')

			#                         browser()
			overIndex <- which(Wlist[[d]][,f]>=0.5)
			label <- tnsr$dimnames[[d]][overIndex]

			for(i in 1:length(overIndex)){
				text(x=overIndex[i],y=Wlist[[d]][,f][overIndex[i]],label=label[i])
			}
			box()
		}

	}




}

	

	
