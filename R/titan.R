# Threshold indicator taxa analisys
# anaílse de indicadores ao longo de um gradiente
# paper original:  https://doi.org/10.1111/j.2041-210X.2009.00007.x
# Kele Rocha Firmaino
# 02/07/20

# pacote ####
library(TITAN2)

# carregando e preparando dados #####
dados <- read.table("./data/dados_titan.txt", sep = ".", h = T)








#dir()#checking directory

# loading libraries
library(vegan)
library(ggplot2)
library(corrplot)

# Loading metrics
dat<-read.table("posdoc.txt",h=T)

# Splitting data
dat[,20:28]->land #land use %
land[,1:8]->land1 #land use %
land[,5:9]->land2 #land use %
dat[,29:39]->wq # water quality
wq[,6]->temp # temperature: temperature_water
dat[,88]->fine # homogeneity: PCT_SFGF_S
dat[,103]->diver # diversity: DIV_SUBS
dat[,131]->rz # canopy cover: XCDENMID
dat[,96]->lit # litter: PCT_BF

dat[,297:381]->fam # family
dat[,c(34,88,96,103,131)]->met # only local metrics
dat[,c(88,96,103,131)]->met1 # only local metrics (-temperature)
dat[,c(88,103)]->sub #fine and diver

### Histograms ####
hist(rz) # canopy cover
hist(temp) # temperature
hist(fine) # homogeneity
hist(diver) # diversity
hist(lit) # litter

#### Pearson correlations ####
corrplot(cor(land1, method="pearson"), method="circle") # land use metrics
cor(land1, method = "pearson")

corrplot(cor(met, method="pearson"), method="circle") # local metrics
cor(met, method = "pearson")

corrplot(cor(met1, method="pearson"), method="circle") #only local metrics (-temperature)
cor(met1, method = "pearson")

corrplot(cor(sub, method="pearson"), method="circle") #substrate
cor(sub, method = "pearson")

corrplot(cor(land, method="pearson"), method="circle") #land use
cor(land, method = "pearson")

corrplot(cor(land2, method="pearson"), method="circle") #land use2
cor(land2, method = "pearson")

#### TITAN ####

# loading libraries
library(TITAN2)

#### Titan riparian zone ####
ti<- titan(rz, fam)
#ti$sumz.cp #dados da tabela geral
#titan(ti) #teoricamente era pra salvar o objeto
write.table(ti$sppmax, "rz_taxa.txt", sep="\t") #salvar o resultado para todos os taxons
write.table(ti$sumz.cp, "rz_sumz.txt", sep="\t")

## Plot titan
plotSumz(ti, filter = FALSE)
plotSumz(ti, filter = TRUE)
plotTaxa(ti, xlabel = "Mean Channel Canopy Cover (%)") #plot obs
plotTaxa(ti, xlabel = "Mean Channel Canopy Cover (%)", z.med = T) #plot media e mediana
plotTaxa(ti, xlabel = "Mean Channel Canopy Cover (%)", z.med = F, prob95 = T) #95% prob
plotCPs(ti, taxa.dist = FALSE, xlambel = "Mean Channel Canopy Cover (%)", stacked = TRUE) #plot CP dos taxa pur e conf >95%

plotCPs(ti) #histograma por taxon (pur e conf) ao longo do gradiente ambiental
#plotCPs(ti, taxaID = "Libellulidae", cp.trace = TRUE, xlabel = "Mean Channel Canopy Cover (%)")
#plotCPs(ti, taxaID = "Baetidae", cp.trace = TRUE, xlabel = "Mean Channel Canopy Cover (%)")
#plotCPs(ti, taxaID = "Calamoceratidae", cp.trace = TRUE, xlabel = "Mean Channel Canopy Cover (%)")

#### Titan litter ####
ti1<- titan(lit, fam)
#ti1$sumz.cp #dados da tabela geral
#titan(ti1) #teoricamente era pra salvar o objeto
write.table(ti1$sppmax, "lit_taxa.txt", sep="\t") #salvar o resultado para todos os taxons
write.table(ti1$sumz.cp, "lit_sumz.txt", sep="\t")

## Plot titan
plotSumz(ti1, filter = FALSE)
plotSumz(ti1, filter = TRUE)
plotTaxa(ti1, xlabel = "Coarse Litter (%)") #plot obs
plotTaxa(ti1, xlabel = "Coarse Litter (%)", z.med = T) #plot media e mediana
plotTaxa(ti1, xlabel = "Coarse Litter (%)", z.med = F, prob95 = T) #95% prob
plotCPs(ti1, taxa.dist = FALSE, xlambel = "Coarse Litter (%)", stacked = TRUE) #plot CP dos taxa pur e conf >95%

plotCPs(ti1) #histograma por taxon (pur e conf) ao longo do gradiente ambiental, ? plota!

## Titan Temperature ####
ti2<- titan(temp, fam)
ti2$sumz.cp #dados da tabela geral
titan(ti2) #teoricamente era pra salvar o objeto
write.table(ti2$sppmax, "temp_taxa.txt", sep="\t") #salvar o resultado para todos os taxons
write.table(ti2$sumz.cp, "temp_sumz.txt", sep="\t")

plotSumz(ti2, filter = FALSE)
plotSumz(ti2, filter = TRUE)
plotTaxa(ti2, xlabel = "Water Temperature (?C)") #plot obs
plotTaxa(ti2, xlabel = "Water Temperature (?C)", z.med = T) #plot media e mediana
plotTaxa(ti2, xlabel = "Water Temperature (?C)", z.med = F, prob95 = T) #95% prob
plotCPs(ti2, taxa.dist = FALSE, xlambel = "Water Temperature (?C)", stacked = TRUE) #plot CP dos taxa pur e conf >95%

plotCPs(ti2) #histograma por taxon (pur e conf) ao longo do gradiente ambiental

## Titan Substrate Diversity ####
ti3<- titan(diver, fam)
#ti3$sumz.cp #dados da tabela geral
#titan(ti3) #teoricamente era pra salvar o objeto
write.table(ti3$sppmax, "diver_taxa.txt", sep="\t") #salvar o resultado para todos os taxons
write.table(ti3$sumz.cp, "diver_sumz.txt", sep="\t")

plotSumz(ti3, filter = FALSE)
plotSumz(ti3, filter = TRUE)
plotTaxa(ti3, xlabel = "Substrate Diversity") #plot obs
plotTaxa(ti3, xlabel = "Substrate Diversity", z.med = T) #plot media e mediana
plotTaxa(ti3, xlabel = "Substrate Diversity", z.med = F, prob95 = T) #95% prob
plotCPs(ti3, taxa.dist = FALSE, xlambel = "Substrate Diversity", stacked = TRUE) #plot CP dos taxa pur e conf >95%

plotCPs(ti3) #histograma por taxon (pur e conf) ao longo do gradiente ambiental

## Titan Substrate Homogeneity ####
ti4<- titan(fine, fam)
#ti4$sumz.cp #dados da tabela geral
#titan(ti4) #teoricamente era pra salvar o objeto
write.table(ti4$sppmax, "fine_taxa.txt", sep="\t") #salvar o resultado para todos os taxons
write.table(ti4$sumz.cp, "fine_sumz.txt", sep="\t")

plotSumz(ti4, filter = FALSE)
plotSumz(ti4, filter = TRUE)
plotTaxa(ti4, xlabel = "Fine sediment < 16 mm (%)") #plot obs
plotTaxa(ti4, xlabel = "Fine sediment < 16 mm (%)", z.med = T) #plot media e mediana
plotTaxa(ti4, xlabel = "Fine sediment < 16 mm (%)", z.med = F, prob95 = T) #95% prob
plotCPs(ti4, taxa.dist = FALSE, xlambel = "Fine sediment < 16 mm (%)", stacked = TRUE) #plot CP dos taxa pur e conf >95%

plotCPs(ti4) #histograma por taxon (pur e conf) ao longo do gradiente ambiental


### PROCRUSTES FAM ETP X GEN ETP ###
# Objetivo: ver a concord?ncia entre fam e gen e assim evitar
# potenciais viezes no uso de fam?lias como bioindicadores
# Aten??o 1: na plan de gen, foram excluidas as linhas (sites) sem generos (crit?rio pra calcular
# distancia de bray-curtis)
# Aten??o 2: as matrizes fam1 e gen1 tem o mesmo tamanho (pela retirada de linhas = 0)
# sites excluidos: Tm6757, Vg21, Vg1257

# loading libraries
library(vegan)

# Only EPT fam x EPT gen
fam1<-read.table("fam1.txt",h=T)
gen1<-read.table("gen1.txt",h=T)

# crie as matrizes de dissimilaridade
d.fam1<-vegdist(fam1, "bray")
d.gen1<-vegdist(gen1, "bray")

# Ordenacoes (PCOA)
pcoa.fam1<-cmdscale(d.fam1, k=77)#k: usei numero de eixos, >0
pcoa.gen1<-cmdscale(d.gen1, k=77)#k: usei numero de eixos, >0

# procrustes
famgen<-protest(pcoa.gen1, pcoa.fam1, permutations = 9999)
famgen

# all Taxa x EPT gen
fam2<-read.table("fam2.txt",h=T)
gen1<-read.table("gen1.txt",h=T)

# crie as matrizes de dissimilaridade
d.fam2<-vegdist(fam2, "bray")
d.gen1<-vegdist(gen1, "bray")

# Ordenacoes (PCOA)
pcoa.fam2<-cmdscale(d.fam2, k=82) #k: usei numero de eixos, >0
pcoa.gen1<-cmdscale(d.gen1, k=82)#k: usei numero de eixos, >0

# procrustes
famgen<-protest(pcoa.gen1, pcoa.fam2, permutations = 9999)
famgen

### Novas correla??es tendo o RCOND com impacto na zona riparia ###
testcor<-read.table("teste_cor.txt",h=T)
testcor[,c(2,4:11)]->met2 # rcond + land use with all natural land use
testcor[,c(2,8:12)]->met3 # rcond + land use with natural sum
testcor[,c(2,13:17)]->met4 # rcond + local metrics

corrplot(cor(met2, method="pearson"), method="circle") # w1_hall + land use with all natural land use
cor(met2, method = "pearson")

corrplot(cor(met3, method="pearson"), method="circle") # w1_hall + land use with natural sum
cor(met3, method = "pearson")

corrplot(cor(met4, method="pearson"), method="circle") # w1_hall + local metrics
cor(met4, method = "pearson")

### Novas correla??es tendo o X_HAG com impacto na zona riparia ###

# Loading metrics
dat<-read.table("posdoc.txt",h=T)

# Splitting data
dat[,c(24:28,34,88,96,103,131,283)]->allcor
corrplot(cor(allcor, method="pearson"), method="circle") # X_HAG + local metrics + % land use
cor(allcor, method = "pearson")

dat[,c(34,88,96,103,131,283)]->allcor1
corrplot(cor(allcor1, method="pearson"), method="circle") # X_HAG + local metrics + % land use
cor(allcor1, method = "pearson")

