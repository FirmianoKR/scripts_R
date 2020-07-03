# Threshold indicator taxa analisys
# anaílse de indicadores ao longo de um gradiente
# paper original:  https://doi.org/10.1111/j.2041-210X.2009.00007.x
# Kele Rocha Firmaino
# 02/07/20

# pacote ####
library(TITAN2)

# carregando e preparando dados #####
dados <- read.table("./data/dados_titan.txt", h = T)

# IMPORTANTE: cada taxon deve ter no mínimo, abundância total >= 5, e frequência >= 3. O dataframe "dados" foi previamente preparado para cumprir este quesito.

agri <- dados[ ,2] # preditor: % de agricultura na bacia de drenagem

tax <- dados[ ,3:61] # resposta: taxons amostrados em cada trecho de riacho


# TITAN ####
res_titan <- titan(agri, tax, boot = FALSE) # apenas para testar.
res_titan <- titan(agri, tax, boot = TRUE) # titan para cada taxon.
write.table(res_titan$sppmax, "./outputs/res_titan_tax.txt", sep = "\t") # salvando

res_titan$sumz.cp # titan considerando toda a comunidade. Precisa do boot = TRUE
write.table(res_titan$sumz.cp, "./outputs/res_titan_sumz.txt", sep = "\t") # salvando

# Alguns Plots titan ####
plotSumz(res_titan, filter = FALSE)
plotSumz(res_titan, filter = TRUE)
plotTaxa(res_titan, xlabel = "Agriculture (%)") #plot obs
plotTaxa(res_titan, xlabel = "Agriculture (%)", z.med = T) #plot media e mediana
plotTaxa(res_titan, xlabel = "Agriculture (%)", z.med = F, prob95 = T) #95% prob

