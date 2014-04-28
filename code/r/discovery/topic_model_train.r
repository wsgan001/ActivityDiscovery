base = "C:\\Users\\zetan\\workspace\\MotionDL\\generated_data\\ubicomp\\DdA_Motion_50_50_30\\hlFeatureFixedDocSample"
setwd(base)
library(topicmodels)
library(tm)
ovid <- Corpus(DirSource(base),readerControl = list(language = "lat"))
#ovid <- tm_map(ovid, as.PlainTextDocument)   
#ovid <- tm_map(ovid, stripWhitespace)  
#ovid <- tm_map(ovid, tolower)                   
#ovid <- tm_map(ovid, removeWords, stopwords("english")) 

#inspect(ovid[1:2]) 

dtm <- DocumentTermMatrix(ovid,control=list(wordLengths=c(1,Inf)))

K=10
train.lda <- LDA(dtm,K)
get_terms(train.lda, 10)

post <- posterior(train.lda, newdata = dtm)
pred = round(post$topics, digits = 2)
#round(post$topics, digits = 2)

colors = c('gray','yellow', 'red', 'blue', 'pink', 'green', 'orange', 'cornflowerblue', 'green4', 'lightcoral', 'mediumslateblue', 'navy','saddlebrown', 'gray20', 'bisque', 'darkgoldenrod3', 'dodgerblue', 'gold4', 'deeppink4', 'navajowhite')

#labels = c('1_', '3_', '4_', '5_', '8_', '10_', '11_', '13_', '14_', '16_', '17_', '18_', '19_',
'20_', '21_', '22_', '23_', '24_', '25_', '27_', '30_');
#activityNames = c('driving bike', 'driving car', 'brushing teeth', 'personal hygiene', 'sitting  having a coffee', 'having dinner', 'having lunch', 'using the toilet', 'sitting  desk activities', 'standing  having a coffee', 'queuing in line', 'standing  talking on phone', 'standing  using the toilet', 'walking', 'walking while carrying something', 'walking freely', 'washing dishes', 'picking up mensa food', 'lying  using computer', 'discussing at whiteboard', 'watching movie');

labels = c('0_','1_', '10_', '17_', '18_', '20_', '32_', '3_', '25_');
activityNames = c('noise','driving bike', 'having dinner', 'queuing in line', 'standing  talking on phone','walking', 'watching movie', 'driving car', 'lying using computer')

docs = dtm$dimnames$Docs


for(activityIndex in 1:length(labels)){
#for(activityIndex in 1:2){
	indexes = which(substr(docs,1,nchar(labels[activityIndex]))==labels[activityIndex])
	png(filename=paste(c('..\\tm_fixed_',K,'_plots_noise\\',activityNames[activityIndex],'.png'),collapse=''))

	plot(pred[indexes,1], type="o", col=colors[1],ylim=c(0, 1), 
		xlab="activities", ylab="topic probability")
	title(main=activityNames[activityIndex])
	for(i in 2:K){
		lines(pred[indexes,i], type="o", pch=22, lty=2, col=colors[i])
	}
	dev.off()
}

for( j in seq(0:4)){
	png(filename=paste(c("act_" ,j,'.png'),collapse=''))
	plot(pred[(j*100 + 1):(j*100+100),1], type="o", col=colors[1],ylim=c(0, 1), 
		xlab="activities", ylab="topic probability")
	for(i in 2:K){
		lines(pred[((j*100 + 1):(j*100+100)),i], type="o", pch=22, lty=2, col=colors[i])
	}
	dev.off()
}
