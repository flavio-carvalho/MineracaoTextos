wd<- paste((getwd()),"/R",sep="")
setwd(wd)
unzip("curto.zip",exdir = "./curto",junkpaths = FALSE)
wd<- paste((getwd()),"/curto",sep="")
setwd(wd)

doclist<- list.files(path = ".", pattern = ".txt", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)


obtem_vetor <- function(f,lower=FALSE,min_len=2) {
  vetor <- scan(f, sep="", what=character(),quote="")
  
  if(lower) {
    vetor <- tolower(vetor)
  }
  return(vetor[nchar(vetor)>=min_len])
}

x <- data.frame( Termos=character())
for (i in 1:(length(doclist))){
  arquivo <- doclist[i]
  termos <- obtem_vetor(arquivo)
  xyz<-unlist(strsplit(termos, "\\W+"))
  palavras <- paste(i,names(table(xyz)), table(xyz), sep=",")
  palavras2 <- as.data.frame((cbind(names(table(termos)),table(termos))), row.names = FALSE)
  vetor <- paste(palavras, sep="\n", "\n") 
  write.csv(cat(vetor, file ="curto.csv",append=TRUE))
#parte que monta a tabela documentos x TF
  y<-palavras2
  colnames(y)<-c("Termos",paste("Doc",i,sep=""))
  x<-merge(x,y, by=intersect(names(x),names(y)),all=T)

}

termosdocumentos = read.csv("curto.csv", stringsAsFactors = F)
colnames(termosdocumentos)=c("doc","termo","freq")

s<-summary((as.factor(termosdocumentos$termo)),maxsum = 999999)
termo_df_idf<- as.data.frame(cbind(Termos=names(s),df = s, idf = log10((length(doclist))/s)), stringAsFactors=F,row.names=1:length(s))

logdf<-as.data.frame(cbind(termosdocumentos$doc,termosdocumentos$termo,1+log10(termosdocumentos$freq)))
colnames(logdf)=c("Doc_number","Termo","TF")