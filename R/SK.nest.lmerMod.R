SK.nest.lmerMod <- function(x,
			    which, 
			    fl1, 
			    fl2,
			    MSE,
			    dfr,
			    sig.level,
			    round,
			    ...)
{

	my <- as.character(formula(x)[[2]])
	m1 <- gsub('\\:','\\+', which)
	m2 <- unlist(strsplit(which,
			      '[[:punct:]]'))  

	forminter <- as.formula(paste(my, '~', m1))

	aux_r <- aggregate(forminter, 
			   data = x@frame,
			   function(x) r = length(x))
	reps <- aux_r[[my]]

	aux_mt <- suppressWarnings(doBy::LSmeans(x,
						 effect = m2))

	aux_mt1 <- aux_mt$coef[,1]

	aux_mt2 <- data.frame(aux_r[1:length(names(aux_r))-1],
			      means = aux_mt1,
			      reps = reps)

	aux_mt3 <- aux_mt2[order(aux_mt2[['means']],
				 decreasing = TRUE),]

	nf1 <- unlist(strsplit(which,
			       split = ':'))[1] # nome do primeiro fator do which

	nf2 <- unlist(strsplit(which,
			       split = ':'))[2] # nome do segundo fator do which

	nf3 <- unlist(strsplit(which,
			       split = ':'))[3] # nome do terceiro fator do which

	if(is.null(fl2)){
		# Interesse apenas na interacao dupla

		f1 <- levels(x@frame[[nf2]]) # correspondem aos fatores que se quer comparar!

		f2 <- levels(x@frame[[nf1]])[fl1] # corresponde ao fator onde se esta fazendo o desdobramento!

		mt <- subset(aux_mt3, 
			     eval(parse(text = nf1)) == f2) # pegando as medias de interesse

		row.names(mt) <- paste(mt[,1],
				       mt[,2],
				       sep='/')  
	} # Interesse na interacao tripla 
	else {

		f1 <- levels(x@frame[[nf3]])

		f2 <- levels(x@frame[[nf2]])[fl2] 

		f3 <- levels(x@frame[[nf1]])[fl1]

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

	#Colancando letrinhas no lugar de números
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

	m.inf <- m.infos.nest.lmerMod(x         = x,
				      my        = my,
				      forminter = forminter,
				      which     = which,
				      fl1       = fl1,
				      fl2       = fl2,
				      sig.level = sig.level,
				      aux_mt    = aux_mt)

	res <- list(out  = out,
		    info = m.inf,
	            stat = out_groups[[2]],
                    clus = out_groups[[3]])
}    
