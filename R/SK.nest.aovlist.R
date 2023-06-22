##
## S3 method to 'aovlist' object
##
SK.nest.aovlist <- function(x,
			    which,
			    fl1,
			    fl2,
			    MSE,
			    dfr, 
			    sig.level,
			    round,
			    ...)
{

	my <- as.character(attr(x,'terms')[[2]])
	m1 <- gsub('\\:',
		   '\\+', 
		   which)

	forminter <- as.formula(paste(my, '~', m1))

	dat <- model.frame(x)

	aux_mt1 <- aggregate(forminter, 
			     data = dat,
			     function(x) c(means = mean(x),
					   r = length(x)))

	aux_mt2 <- aux_mt1[order(aux_mt1[[my]][,1], 
				 decreasing = TRUE),]

	aux_mt3 <- data.frame(aux_mt2[1:length(names(aux_mt2))-1],
			      means = aux_mt2[[my]][,1],
			      reps = aux_mt2[[my]][,2])

	nf1 <- unlist(strsplit(which,
			       split = ':'))[1] # nome do primeiro fator do which

	nf2 <- unlist(strsplit(which,
			       split = ':'))[2] # nome do segundo fator do which

	nf3 <- unlist(strsplit(which,
			       split = ':'))[3] # nome do terceiro fator do which

	if(is.null(fl2)){
		# Interesse apenas na interacao dupla
		f1 <- levels(model.frame(x)[,nf2]) # correspondem aos fatores que se quer comparar!


		f2 <- levels(model.frame(x)[,nf1])[fl1] # corresponde ao fator onde se estao fazendo o desdobramento!

		mt <- subset(aux_mt3, 
			     eval(parse(text = nf1)) == f2) # pegando as medias de interesse

		row.names(mt) <- paste(mt[,1],
				       mt[,2],
				       sep='/')  
	} # Interesse na interacao tripla 
	else {

		f1 <- levels(model.frame(x)[,nf3])

		f2 <- levels(model.frame(x)[,nf2])[fl2] 

		f3 <- levels(model.frame(x)[,nf1])[fl1]

		mt <- subset(aux_mt3, 
			     eval(parse(text = nf1)) == f3 & eval(parse(text=nf2)) == f2) # pegando as medias de interesse

		row.names(mt) <- paste(mt[,1],
				       mt[,2],
				       mt[,3],
				       sep='/')

	} 
	g    <- nrow(mt)

	out_groups <- MaxValue(g,
			       mt,#means and repetitions
			       mMSE = MSE,#Mean Square Error
			       dfr,
			       sig.level=sig.level,
			       1,
			       rep(0, g),
			       0,
			       rep(0, g))

	out_means_groups  <- cbind(format(round(mt[['means']],
						round),
					  nsmall = 2),
				   out_groups[[1]])

	rownames(out_means_groups) <- rownames(mt)
	colnames(out_means_groups) <- c('Means','G1')

	#Colancando letrinhas no lugar de numeros
	numericgroup <- as.numeric(out_means_groups[,2])
	group <- numericgroup
	ngroups <- as.numeric(group[length(group)])

	if(ngroups > 26){
		groupletter <- as.vector(t(outer(letters,
						 letters,
						 paste,
						 sep="")))
	}else{
		groupletter <- letters
	}

	xgroups <- seq(ngroups)

	for(i in 1:ngroups){
		group[group == xgroups[i]] <- groupletter[i]
	}

	newgroups <- matrix("",nrow=length(group),ncol=ngroups)
	aux <- cbind(1:nrow(newgroups),numericgroup)
	newgroups[aux] <- group

	out_means_groups <- data.frame(Means=out_means_groups[,1],
				       newgroups)
	colnames(out_means_groups) <- c('Means',paste('G',1:ngroups,sep=''))

	#Temina aqui!

	out <- list(Result     = out_means_groups,
		    Sig.level  = sig.level,
		    Replicates = mt[['reps']])

	m.inf <- m.infos.nest.aovlist(x         = x,
				      my        = my,
				      forminter = forminter,
				      which     = which,
				      fl1       = fl1,
				      fl2       = fl2,
				      sig.level = sig.level,
				      aux_mt    = aux_mt1,
				      MSE       = MSE)

	res <- list(out  = out,
	            info = m.inf,
	            stat = out_groups[[2]],
	            clus = out_groups[[3]])
}
