base = "C:\\Users\\zetan\\workspace\\MotionDL\\generated_data\\ubicomp\\DdA_Motion_50_50_30\\testSample"
setwd(base)

ovid_test <- Corpus(DirSource(base),readerControl = list(language = "lat"))

dtm_test <- DocumentTermMatrix(ovid_test,control=list(wordLengths=c(1,Inf)))



post_test <- posterior(train.lda, newdata = dtm_test)
pred_test = round(post_test$topics, digits = 2)
#round(post$topics, digits = 2)

colors = c('gray','yellow', 'red', 'blue', 'pink', 'green', 'orange', 'cornflowerblue', 'green4', 'lightcoral', 'mediumslateblue', 'navy','saddlebrown', 'gray20', 'bisque', 'darkgoldenrod3', 'dodgerblue', 'gold4', 'deeppink4', 'navajowhite')


labels = c('0_','3_', '14_', '22_', '25_', '27_');
activityNames = c('noise', 'driving car', 'sitting  desk activities','walking freely', 'lying  using computer', 'discussing at whiteboard');

docs = dtm_test$dimnames$Docs


for(activityIndex in 1:length(labels)){
#for(activityIndex in 1:2){
	indexes = which(substr(docs,1,nchar(labels[activityIndex]))==labels[activityIndex])
	png(filename=paste(c('..\\tm_fixed_',K,'_plots_noise_test_8_ground\\',activityNames[activityIndex],'.png'),collapse=''))

	plot(pred_test[indexes,1], type="o", col=colors[1],ylim=c(0, 1), 
		xlab="activities", ylab="topic probability")
	title(main=activityNames[activityIndex])
	for(i in 2:K){
		lines(pred_test[indexes,i], type="o", pch=22, lty=2, col=colors[i])
	}
	dev.off()
}

