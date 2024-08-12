## Teste de Estatística D 

##Pacotes necessários
library(phytools)
library(caper)
library(geiger)
library(vegan)
##Pacotes adicionais dependendo da sua versão do R 
library(ape)  # é tipo uma versão velha do phytools misturada com ggplot
library(readxl)  # para abrir bancos de dados em formato .xlsx

#Primeiro passo: Carregar e organizar os dados
tree<-read.tree("amphibian.tre")
traits<-read_excel("bufonidae_traits.xlsx") # trocar para a que estiver usando

##Os dados precisam ser organizados para que listem apenas as espécies presentes nos dois arquivos, e na mesma ordem do que o arquivo da árvore

##Cria dois vetores que listam nome das espécies presentes na árvore e nos dados
species_in_tree <- tree$tip.label
species_in_data <- traits$species

##Cria um vetor com as espécies comuns entre árvore e traits
common_species <- intersect(species_in_tree, species_in_data)

##Cortar tips desnecessárias da árvore (remove espécies adicionais que não estão presentes nos traits)
pruned_tree <- drop.tip(tree, setdiff(species_in_tree, common_species))

##Faz a mesma coisa para os traits
pruned_traits <- traits[traits$species %in% common_species, ]

##Reordenar as sps em traits para ser igual a ordem da árvore
pruned_traits <- pruned_traits[match(pruned_tree$tip.label, pruned_traits$species), ]
row.names(pruned_traits) <- pruned_traits$species #coloca coluna adicional para nome das espécies 

##Verificar se estão iguais 
print(length(pruned_tree$tip.label)) 
print(length(rownames(traits))) #o número deve ser igual ao resultado anterior
summary(rownames(traits)==pruned_tree$tip.label) #verifica se estão na mesma ordem, deve resultar Mode TRUE logical
pruned_tree$node.label<-NULL #Passo adicional para funcionamento da estatística D

#Se tudo deu certo, dá pra selecionar as variáveis para fazer o teste D
##pra fazer o teste precisa 
traitD <- data.frame(species = rownames(pruned_traits), arboreal = pruned_traits$arboreal) #cria tabela formato data frame com uma coluna para espécie e uma coluna para o trait observado, por exemplo habitat fossorial
traitD <- comparative.data(pruned_tree, traitD, names.col = "species") #combina dados de traits com dados da árvore, alinhando pelas espécies presentes
## Para fazer o teste D crie um objeto para facilitar a visualização dps
print(D <- phylo.d(traitD, binvar = arboreal))
##Plotagem gráfica 
plot(D)

##Dá pra criar uma árvore com uma bolinha aberta ou fechada indicando a ausência ou presença do trait binário sendo analisado:
plot(pruned_tree,cex=0.5) ##cex determina tamanho da fonte em proporção ao tamanho standard, se vc colocar cex=0.5 será 0.5x menor que a fonte original etc
tip_labels <- as.numeric(pruned_traits$aquatic) #deve ser referente a tabela criada no início, não a criada quando montando o teste D
tip_colors <- ifelse(tip_labels == 1, "black", "white")
tiplabels(pch = 21, col = "black", bg = tip_colors, cex = 1, adj = 0.5)

##Troubleshooting:
#Para erros rodando função phylo.d 
## "Labels duplicated between tips and nodes in phylogeny:
pruned_tree$node.label<-NULL
## Erros na criação de data.frame e comparative.data :
## "arguments imply different number of rows" 
## provavelmente vc selecionou uma linha ao inves de uma coluna para leitura dos dados, é importante que a parte "species=" sempre seja seguido do argumento "rownames(base de onde vc quer tirar o trait a ser analisado)"
## o jeito mais fácil de conferir é rodando isso:
length(rownames(pruned_traits))
length(pruned_traits$arthropod_diet)
## Os números devem ser iguais. Se um deles estiver retornando 7 volte na seleção e escolha leitura por rows


#Regressão de Mantel 
##Criar objeto com as distancias fenotipicas entre pares de tips usando as distâncias de branch 
dist.phy<-cophenetic.phylo(pruned_tree)  #dist.phy = distancia filogenetica
##criar uma matriz com os dados binários e contínuos que queremos testar 
matriz_mantel<-as.matrix(data.frame(pruned_traits$arboreal,pruned_traits$body_size_mm,row.names = rownames(pruned_traits)))
##organizar os dados de maneira padronizada 
matriz_mantel<-decostand(matriz_mantel, method='standardize')
dist.traits<-as.matrix(vegdist(matriz_mantel, "euclidean"))
print(resultado_mantel <- multi.mantel(dist.traits, dist.phy)) #é normal demorar um pouquinho pra calcular esse
##representação gráfica 
plot(dist.phy,dist.traits) #distribuição, um em cima do outro 
abline(resultado_mantel) #usa os valores da regressão de mantel pra plotar uma linha em cima do scatterplot



#Outros testes:

#Teste de Lambda de Pagel (para 1 trait numérico contínuo)
(Pagel <- phylosig(pruned_tree, pruned_traits$body_size_mm, method = "lambda", test = TRUE))

#Teste de K de Blomberg (para 1 trait numérico contínuo)
(K <- phylosig(pruned_tree, pruned_traits$body_size_mm, method = "K", test = TRUE))



