data <- read_excel("~/Santo Tomás/FODEIN/confidence-online/data.xlsx")
library(plspm)
library(ggplot2)
library(dplyr)
#Modelo parte 1 
                    # 1  2  3  4  5  6  7  8  9  10 11
Riesgo       =     c (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Seguridad    =     c (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Privacidad   =     c (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Reputacion   =     c (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Utilidad     =     c (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Calidad      =     c (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Confianza    =     c (1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
Satisfaccion =     c (0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
Actitud      =     c (0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
Intencion    =     c (0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0)
Lealtad      =     c (0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)

names(data)

# path matrix created by row binding
confidence_path = rbind(Riesgo, Seguridad, Privacidad, Reputacion,
                        Utilidad, Calidad, Confianza, Satisfaccion,
                        Actitud, Intencion, Lealtad)
names(data)

# add column names (optional)
colnames(confidence_path) = rownames(confidence_path)
# let's see it
confidence_path
# plot the path matrix
innerplot(confidence_path)        
# define list of indicators: what variables are associated with
# what latent variables
confidence_blocks = list(1:6, 7:11, 12:16, 17:20, 21:25, 26:40, 51:53, 41:45, 46:47, 54:60, 48:50)  
# all latent variables are measured in a reflective way
confidence_modes = rep("A", 11) 
#modificar los simbolos 
# run plspm analysis
confidence_pls = plspm(data, confidence_path, confidence_blocks, modes = confidence_modes)
# summarized results
summary(confidence_pls)
#plot
plot(confidence_pls)
#effects
path_effs <- confidence_pls$effects
path_effs
#Cargas y comunalidades (esto solo se mide en el modelo exterior)
cargas<- confidence_pls$outer_model
cargas
#Revisar por bloques
confidence_pls$outer_model

summary(subset(confidence_pls$outer_model, block == "Riesgo"))
summary(subset(confidence_pls$outer_model, block == "Seguridad"))
summary(subset(confidence_pls$outer_model, block == "Privacidad"))
summary(subset(confidence_pls$outer_model, block == "Reputacion"))
summary(subset(confidence_pls$outer_model, block == "Utilidad"))
summary(subset(confidence_pls$outer_model, block == "Calidad"))
summary(subset(confidence_pls$outer_model, block == "Satisfaccion"))
summary(subset(confidence_pls$outer_model, block == "Actitud"))
summary(subset(confidence_pls$outer_model, block == "Lealtad"))
summary(subset(confidence_pls$outer_model, block == "Confianza"))
summary(subset(confidence_pls$outer_model, block == "Intencion"))
#Eliminar ítems problemáticos y recalcular
data <- select(data, -SAT5) # eliminamos item problemático SAT5 
names(data)
# what latent variables
confidence_blocks2 = list(1:6, 7:11, 12:16, 17:20, 21:25, 26:40, 51:53, 41:44, 45:46, 53:59, 47:49)  
# run plspm analysis
confidence_pls2 = plspm(data, confidence_path, confidence_blocks2, modes = confidence_modes)
# summarized results
summary(confidence_pls2)
#plot
plot(confidence_pls2)
#effects
path_effs2 <- confidence_pls2$effects
path_effs2
#Cargas y comunalidades (esto solo se mide en el modelo exterior)
cargas<- confidence_pls2$outer_model
cargas
#Revisar la carga del factor donde se elimino el item
summary(subset(confidence_pls2$outer_model, block == "Satisfaccion"))
# cross-loadings
confidence_pls2$crossloadings
# load ggplot2 and reshape 
library(ggplot2) 
library(reshape)
# reshape crossloadings data.frame for ggplot 
xloads = melt(confidence_pls2$crossloadings, id.vars = c("name", "block"), variable_name = "LV")
# bar-charts of crossloadings by block 
ggplot(data = xloads, aes(x = name, y = value, fill = block)) + 
# add horizontal reference lines 
  geom_hline(yintercept = 0, color = "gray75") + geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) + 
# indicate the use of car-charts 
  geom_bar(stat = 'identity', position = 'dodge') + 
# panel display (i.e. faceting) 
  facet_wrap(block ~ LV) + # tweaking some grahical elements 
  theme(axis.text.x = element_text(angle = 90), line = element_blank(), plot.title = element_text(size = 12)) + 
  # add title 
  ggtitle("Crossloadings")
#graficar el modelo estructural
plot(confidence_pls2, arr.pos = 0.35, arr.lwd = arrow_lwd)
confidence_pls2$scores
# matrix of path coefficients 
Paths = confidence_pls2$path_coefs
# matrix with values based on path coeffs
arrow_lwd = 10 * round(Paths, 2)
# arrows of different sizes reflecting the values of the path coeffs 
plot(confidence_pls2, arr.pos = 0.35, arr.lwd = arrow_lwd, box.size = 0,2)
# inner model 
confidence_pls2$inner_model
confidence_pls2$inner_summary
# gof index 
confidence_pls2$gof
# running bootstrap validation 
confidence_pls_val = plspm(data, confidence_path, confidence_blocks2, modes = confidence_modes, boot.val = TRUE, br = 200)
# bootstrap results
confidence_pls_val$boot
#Descriptivos
Descriptivos <- read_excel("Descriptivos.xlsx")
sd(Descriptivos$Edad)
Genero_Factor <- factor(Descriptivos$`Indique su genero`, levels=c("Mujer", "Hombre"))
(Genero_Factor)
Estrato_Factor <- Factor <- factor(Descriptivos$Estratosoeconómico) 
(Estrato_Factor)

