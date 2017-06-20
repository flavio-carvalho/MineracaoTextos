# 0.1 - Descompacta e acessa pasta do arquivo
wd<- paste((getwd()),"/R",sep="")
setwd(wd)
unzip("curto.zip",exdir = "./curto",junkpaths = FALSE)
wd<- paste((getwd()),"/curto",sep="")
setwd(wd)

# 0.2 - Lista dos arquivos descompactados na pasta
doclist<- list.files(path = ".", pattern = ".txt", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# 0.3 -Função para leitura do arquivo 
# Obs: troca os caracteres maiúsculos por minúsculos, descarta termos com 1 caracter.
obtem_vetor <- function(f,lower=FALSE,min_len=2) {
  vetor <- scan(f, sep="", what=character(),quote="")
  
  if(lower) {
    vetor <- tolower(vetor)
  }
  return(vetor[nchar(vetor)>=min_len])
}

# 0.4 -Loop para Calculo de TF (Sem penalizar) e gera arquivo csv
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

# 0.5 - Leitura do arquivo csv com o resultado da matriz Term frequency (TF) (sem penalizar)
termosdocumentos = read.csv("curto.csv", stringsAsFactors = F)
colnames(termosdocumentos)=c("doc","termo","freq")

# 1.0 - Solicitado "código para produzir a matriz de document frequency (DF) conforme a fórmula com log"
# Obs.: Acho que ele quer iDF x Termo, não?
s<-summary((as.factor(termosdocumentos$termo)),maxsum = 999999)
termo_df_idf<- as.data.frame(cbind(Termos=names(s),df = s, idf = log10((length(doclist))/s)), stringAsFactors=F,row.names=1:length(s))

# 2.0 Gera um arquivo pdf com o resultado da matriz DF x Termo
# Obs.: Acho que ele quer iDF x Termo, não?

# WIP

# 3.0 - Código para produzir a matriz de Term frequency (TF) conforme a fórmula com log
# Obs.: #Calcula o terceiro item solicitado no trabalho -> Matriz de Term Frequency (TF) (penalizando)

logdf<-as.data.frame(cbind(termosdocumentos$doc,termosdocumentos$termo,1+log10(termosdocumentos$freq)))
colnames(logdf)=c("Doc_number","Termo","TF")

# 4.0 insira no arquivo pdf com o resultado da matriz TF x Documento.
# WIP

# 5.0 Código para produzir a matriz de TF*IDF
# WIP
