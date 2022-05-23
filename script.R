# Nova Etapas
# 1- Bconstruir novas vari[aveis de transformação] -amortecimento
# testar o modelo multivariado a partir ketelaere 2003


library(dplyr)
library(tidyr)
library(ggplot2)
library(wrassp)
library(tuneR)
library(seewave)

####################################################################
au = read.AsspDataObj("C:/Users/Usuario/Downloads/Semana 09 Ovos_Calcio_01Abril/1A.wav")

caminho <- "C:\\Users\\Usuario\\Downloads\\Semana 09 Ovos_Calcio_01Abril\\1A_data\\e08\\d08"
arq<-list.files(caminho,full.names = TRUE)
extract_audio_features()

seewave::read.audacity("C:/Users/Usuario/Downloads/Semana 09 Ovos_Calcio_01Abril/1A.aup",
                       format="dir")
readLines("C:/Users/Usuario/Downloads/Semana 09 Ovos_Calcio_01Abril/1A.aup")
read.table(arq[1], header = FALSE, sep = "\t",
           dec = ".")
#####################################################################

# Definindo o caminho dos arquivos
getwd()
files<-list.files("data-raw/",
                full.names=TRUE)
files
au = read.AsspDataObj(files[1])


y<-au$audio[,1]
length(y)

# Script Nelson

Fs<-rate.AsspDataObj(au) # (sample rate - Hz): taxa de amostragem por seg.
t_incr<-1/Fs#incremento no tempo
To<-startTime.AsspDataObj(au)#tempo inicial da 1a amostra
Tf<-dur.AsspDataObj(au) # duração da gravação em ms
N<-numRecs.AsspDataObj(au) # T: total de ptos que são capturados.
t<-seq(To,Tf,t_incr);# discretização no tempo
t<-t[-length(t)]
length(t)
length(y)
nfft<-nextn(length(y),2);

y<-as.vector(y)

TDF<-fft(y[1:(length(y)-1)],inverse=FALSE);#transfomada discreta de Fourier
length(TDF)

plot(t,y, type="l", xlab="tempo(s)", ylab="Amplitude")

fff<-TDF[1:(nfft/2)];#Freq máxima
xfft<-seq(0,(nfft/2)-1/nfft)*Fs/N;# eixo das frequencias
A<-Mod(fff);
par(mfrow = c(2,1))

tibble(A,xfft)  |>
  filter(xfft >= 800, xfft <= 9000)  |>
  ggplot(aes(x=xfft,y=A)) +
  geom_line(color="red")

tibble(A,xfft)  |>
  filter(xfft >= 800, xfft <= 9000) |>
  arrange(desc(A))  |>
  slice(1:4)  |>
  summarise(media = mean(xfft))


dB = 10*log10(A/1e-12)

tibble(A,xfft)  |>
  mutate(dB = 20*log10(A/max(A)/2))  |>
  filter(xfft >= 800, xfft <= 9000)  |>
  ggplot(aes(x=xfft,y=dB)) +
  geom_line(color="red")+
  coord_cartesian(ylim=c(-50,-10))


