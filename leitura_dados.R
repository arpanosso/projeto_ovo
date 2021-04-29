library(wrassp)
library(tuneR)
# https://medium.com/@taposhdr/basics-of-audio-file-processing-in-r-81c31a387e8e
# https://cran.r-project.org/web/packages/wrassp/vignettes/wrassp_intro.html

# get the path to the data that comes with the package
wavPath = system.file('extdata', package='wrassp')
# now list the .wav files so we have some audio files to play with
wavFiles = list.files(wavPath, pattern=glob2rx('*.wav'), full.names=TRUE)

files<-list.files("1B_data/1B_data/e08/d08",full.names=TRUE)
au = read.AsspDataObj(files[1])

#class(au)
#print(au)
#######################################################################################
# Obtendo os termos no domínio do tempo usados no Processamento de Sinal Digital (DSP)#
#######################################################################################

#relação entre nro de ptos (T) ,taxa de amostragem (N) e o passo (delta.T):T=N*delta.T
dur.AsspDataObj(au) # deta.T ou passo: The fundamental period.
rate.AsspDataObj(au) # N (sample rate - Hz): is the number of samples taken over a time period T.
numRecs.AsspDataObj(au) # T: nro de ptos que são capturados para realizar a transformação de Fourier

# attributes(au)
#tracks.AsspDataObj(au)
#head(au$audio)

# criando o vetor discreto no tempo(s) cujos N pontos serão amostrados: x=(0,1/N, 2/N,..., N-1/N)
x<-seq(0,numRecs.AsspDataObj(au) - 1, 1/ rate.AsspDataObj(au)) #confirmar esse -1, mas parece ok?
#length(x)
y<-au$audio[c(TRUE, rep(FALSE,9))]
#length(y)

plot(x,y,
     type='l',
     xlab='time (s)',
     ylab='Audio samples')


############################################################################################
# Obtendo os termos no domínio da frequência usados no Processamento de Sinal Digital (DSP)#
############################################################################################

# Alan ele não transformou para o  domínio da frequência ne fez a transformada de fourier:

# calculate formants and corresponding bandwidth values
fmBwVals = forest(files[1], toFile=F)
# due to toFile=F this returns an object of the type AsspDataObj and
# prevents the result being saved to disc as an SSFF file

class(fmBwVals)
# extract track names
# this time the object contains muliple tracks (formants + their bandwidths)

tracks.AsspDataObj(fmBwVals)
dim(fmBwVals$fm)
matplot(seq(0,numRecs.AsspDataObj(fmBwVals) - 1) / rate.AsspDataObj(fmBwVals) +
          attr(fmBwVals, 'startTime'),
        fmBwVals$fm,
        type='l',
        xlab='time (s)',
        ylab='Formant frequency (Hz)')


# calculate the fundamental frequency contour
f0vals = ksvF0(files[1], toFile=F)
# plot the fundamental frequency contour
plot(seq(0,numRecs.AsspDataObj(f0vals) - 1) / rate.AsspDataObj(f0vals) +
       attr(f0vals, 'startTime'),
     f0vals$F0,
     type='l',
     xlab='time (s)',
     ylab='F0 frequency (Hz)')


rmsana(wavFiles, outputDirectory = tempdir())

rmsFilePaths = list.files(tempdir(),
                          pattern = paste0('*.',wrasspOutputInfos$rmsana$ext),
                          full.names = T)

rmsvals = read.AsspDataObj(rmsFilePaths[1])

plot(seq(0,numRecs.AsspDataObj(rmsvals) - 1) / rate.AsspDataObj(rmsvals) +
       attr(rmsvals, 'startTime'),
     rmsvals$rms,
     type='l',
     xlab='time (s)',
     ylab='RMS energy (dB)')
