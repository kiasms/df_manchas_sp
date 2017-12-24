#############################################
#       Funcao de Lucas Menezes Siva        #
#                                           #
# https://github.com/kiasms/df_manchas_sp   #    
#############################################

#Esta funcao fara com que o R leia arquivos .bmp (imagem bitmap) que está no diretorio especifico do computador. A funcao exige que este diretorio seja informado como argumento da mesma. As imagens analisadas serao apenas as manchas dorsais de um anfibio. As imagens sao no formato .bmp, de tamanho padronizado (5x5cm), em escala de cinza, com resolução de 50 pixels por polegada.
#Apos as imagens serem carregadas o R ira quantificar no espaco de pixels o quanto e como as  imagens estao preenchidas. Para isso a funcao carregada utiliza a metodologia box-couting. O metodo analisa as formas dos objetos que podem estar contidos numa imagem sobre forma que grade de quadrados com lados de tamanho s. A analise tambem pode considerar matrizes de valores, onde numeros indicam as cores armazenadas nos pixels (quadrados). A analise conta o numero de quadrados (N(s)) e repete a contagem para quadrados reticulados de tamanhos s menores do primeiro que foi analisado para obter valores de N de cada. Por fim, a analise monta um grafico com valores logaritmizados de N(s) x log 1s. Uma reta de regressao e obtida para os pontos contados obtidos e o valor de dimensao fractal e obtido pela inclinacao desta reta.
#Apos contagem ser realizada, o R fara uma analise de dimensao factral para cada imagem e armazenara estes dados em uma tabela com os valores respectivos de cada imagem.
#Por fim, o R ira reunir os dados de dimensao fractal das imagens dos especimes analisados e fara uma media dos mesmos armazendo o valor num objeto.

df_mancha_sp = function(path_folder){ #Cria uma funcao com o argumento que e o diretorio onde as imagens .bmp estao armazenadas 
  library(bmp) #carrega o pacote bmp o qual le arquivos .bmp e os transforma em matrizes de valores.
  library(fractaldim) #carrega o pacote fractaldim o qual contem a funcao de analise de dimensao fractal de imagens transformadas em matrizes.
  
  lista = list.files(#lista os arquivos de imagem .bmp que estao no diretorio.
  path_folder#dentro do dentro diretorio das imagens.
  ,pattern ="*.bmp")#leitura com padrao de extensao .bmp

matrizes = sapply(lista, read.bmp) #Aplica a funcao read.bmp em todos os elementos de lista, ou seja, em todas as imagens .bmp.

#Transformando as matrizes de imagem em matrizes binarias. Metodologia escolhida para padronizar as matrizes e deixa-las mais compactas para a analise de dimensao fractal. 

matrizes.b =   ifelse(matrizes < 20,0,matrizes) #aplica nas matrizes geradas a partir das imagens o primeiro tratamento: considera pixels cinza mais proximos de preto como preto. Na matriz binaria, pixel preto = 0; o tratamento considera como padronizacao um intervalo de 0 a 20 como preto.
matrizes.bin = ifelse(matrizes.b >= 20,1,matrizes.b) #aplica nas matrizes geradas a partir das imagens o segundo tratamento: considera pixels cinza mais proximos de branco como branco. Na matriz binaria, pixel branco = 1; o segundo tratamento considera como padronizacao valores acima de 20 como branco.

#Aplicacao da funcao fd.estim.boxcount (padrao de pixels) para cada matriz binaria armazenada dentro de matrizes.bin

fractal = apply( #armazena numa lista fractal o resultado da funcao fd.estim.boxcount
  matrizes.bin #matrizes binarias as quais sera executada a funcao  fd.estim.boxcount
  , FUN = fd.estim.boxcount #indica que a funcao a ser executada (fd.estim.boxcount - pacote fractaldim). A analise fd.estim.boxcount
  , MARGIN = 2 #indica que sera aplicada nas colunas (da lista que carrega as matrizes binarias).
  , nlags = "all") #argumento necessario para funcao fd.estim.boxcount. nlags = all significa que todos os lags serao utilizados na estimacao. Todas as opcoes possiveis puxa o melhor numero de lags do metodo.

#Em fractal (acima), os valores de fd estao dentro de uma sublista. Para acessar esses valores de fd de cada imagem na pasta, sera realizado o procedimento abaixo, com o objetivo de gerar uma dataframe com os nomes de cada imagem em ordem crescente e os seus  respectivos valores de fd.

fdparcial = sapply(fractal, function(x) {x[2]}) #Os valores de fd sao o segundo elemento de cada sublista (cada imagem). Neste caso, o objeto fdparcial foi criado o qual contem somente este segundo elemento de cada imagem. (Comentario - Lucas - Mas mesmo assim nao consegui fazer o dataframe desejado, pode ser que exista comandos com menores numeros de linhas, porem, ate o momento decidi passar para procedimento abaixo.

fdresults = unlist(fdparcial,  use.names = FALSE) #cria um objeto fdresults no qual foi transformado fdparcial numa lista simples, apenas com os valores de fd de cada imagem (use.names = FALSE para nao incluir os nomes da lista anterior na lista atual).


img.fds = data.frame(lista, fdresults) #Cria um dataframe com os valores de fd de cada imagem e os nomes de cada imagem.

colnames(img.fds) = c("Imagem", "FD") #Dar nome de coluna do dataframe criado acima como FD.

fdmean = mean(img.fds$FD) #Cria um objeto que armazena o valor da media das dimensoes fractais dos especimes analisados

results = list(img.fds, fdmean) #Cria um objeto que lista o dataframe com os valores de FD de cada imagem e a media dos valores de FD das imagens.

return(results) #Retorna os valores do objeto results

}
