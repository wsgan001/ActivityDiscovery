base = "C:\\Users\\zetan\\workspace\\MotionDL\\generated_data\\ubicomp\\DdA_Motion_50_50_30\\topic_model_raw_data\\docs\\day1"
setwd(base)
library(topicmodels)
library(tm)
ovid <- Corpus(DirSource(base),readerControl = list(language = "lat"))

dtm <- DocumentTermMatrix(ovid,control=list(wordLengths=c(1,Inf)))

K=10
train.lda <- LDA(dtm,K)
get_terms(train.lda, 10)

post <- posterior(train.lda, newdata = dtm)
pred = round(post$topics, digits = 2)


colors = c('gray','yellow', 'red', 'blue', 'pink', 'green', 'orange', 'cornflowerblue', 'green4', 'lightcoral', 'mediumslateblue', 'navy','saddlebrown', 'gray20', 'bisque', 'darkgoldenrod3', 'dodgerblue', 'gold4', 'deeppink4', 'navajowhite', 'hotpink4','indianred1', 'steelblue4', 'chocolate2', 'aquamarine1', 'cyan2', 'cyan4')




isActivity = function(x){
	sumVar = 0;
	for(i in 1:K){sumVar = sumVar + var(x[,i]);}
	return(ifelse(sumVar<0.08,1,0))
}


mergeNeighbourActivity = function(x){
	len = length(x[,1])
	actSize = 5
	findActivity = FALSE;
	startVec = vector()
	endVec = vector()
	for(i in 1:(len-actSize+1)){
		if(findActivity == FALSE){
			if(isActivity(x[i:(i+actSize-1),]) == TRUE){
				
				findActivity = TRUE;
				startVec = c(startVec, i +1);
			}
		}else{
			if(isActivity(x[i:(i+actSize-1),]) == FALSE){
				findActivity = FALSE;
				endVec = c(endVec, i+actSize-1 -1);
			}else{
				if(i == (len-actSize+1)){
					endVec = c(endVec, i+actSize-1 -1);
				}
			}
		}
	}
	return(rbind(startVec, endVec))
}
visualSegmentation = function(startEndList){
	pairCnt = length(startEndList[1,])
	for(i in 1:pairCnt){
		for(j in startEndList[1,i] : startEndList[2,i]){
			points(x = j, y = 0.7, col = colors[i], pch = 16)
		}
	}
}

labels = read.table(file="../labels_day1.txt",sep='\n')


offset = 300
plot(pred[(offset + 1):(offset+100),1], type="o", col=colors[1],ylim=c(0, 1), 
	xlab="activities", ylab="topic probability")
for(i in 2:K){
	lines(pred[((offset + 1):(offset+100)),i], type="o", pch=22, lty=2, col=colors[i])
}

segmentation = mergeNeighbourActivity(pred[(offset+1):(offset+100),])
visualSegmentation(segmentation)

for( i in 1:100){	
	 points(x = i, y = 0.9, col = colors[labels$V1[i+offset]], pch = 16)
}


for(i in 1:95){
	isAct = isActivity(pred[(i+offset):(i+5+offset), ])
	if(isAct == 1){
		rect(i,0,i+5,1)
	}
}





