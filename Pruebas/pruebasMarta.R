byindus <- table(incidents$industry)
indus <- as.data.frame(byindus)
ggplot(indus, aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity", fill="coral") +
  coord_flip()

incidents <- incidents %>% unnest(mitre_attack)

#Tactics
bytactics <- table(incidents$tactic)
tact <- as.data.frame(bytactics)
tmp <- tact %>%
  filter(!is.na(Freq)) %>%
  mutate(Var1=factor(Var1, Var1))) %>%
  arrange(desc(Freq))


# Set a number of 'empty bar'
empty_bar=10
# Add lines to the initial tmpset
to_add = matrix(NA, empty_bar, ncol(tmp))
colnames(to_add) = colnames(tmp)
tmp=rbind(tmp, to_add)
tmp$id=seq(1, nrow(tmp))

# Get the name and the y position of each label
label_tmp=tmp
number_of_bar=nrow(label_tmp)
angle= 90 - 360 * (label_tmp$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_tmp$hjust<-ifelse( angle < -90, 1, 0)
label_tmp$angle<-ifelse(angle < -90, angle+180, angle)
label_tmp$Var1 <- paste(label_tmp$Var1, " (", label_tmp$Freq,")", sep="")

# Make the plot
ggplot(tmp, aes(x=as.factor(id), y=Freq)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill=alpha("#69b3a2", 0.8)) +
  ylim(-7000,13000) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar(start = 0) +
  geom_text(data=label_tmp, aes(x=id, y=Freq+200, label=Var1 ), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_tmp$angle, hjust=label_tmp$hjust, inherit.aes = FALSE )

#Techniques
incidents2 <- incidents %>%
  mutate(
    paste = paste(id,"|",technique)
  )
bytechniques <- table(incidents2$paste)
techniques <- as.data.frame(bytechniques)
names(techniques)[names(techniques) == "Var1"] <- "technique"
techniques<- within(techniques, technique<-data.frame(do.call('rbind', strsplit(as.character(technique), '|', fixed=TRUE))))

techniques$text <- paste("name: ",techniques$technique, "\n", "value:", data$Freq, "\n", "")
# Generate the layout
packing <- circleProgressiveLayout(techniques$Freq, sizetype='area')
data <- cbind(techniques, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot with a few differences compared to the static version:
p <- ggplot() +
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = data$text[id], data_id = id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, label = gsub("Technique", "", technique)), size=2, color="black") +
  theme_void() +
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) +
  coord_equal()

# Turn it interactive
widg <- ggiraph(ggobj = p, width_svg = 7, height_svg = 7)


####ORg
incidents3 <- incidents %>%
  mutate(
    paste = paste(industry,"|",org)
  )
byorg <- table(incidents3$paste)
org <- as.data.frame(byorg)
org<-within(org, Var1<-data.frame(do.call('rbind', strsplit(as.character(Var1), '|', fixed=TRUE))))
org <- cbind(org$Freq, org$Var1$X1, org$Var1$X2)
org <- as.data.frame(org)
org$V1 <- as.numeric(org$V1)
library(plotly)


fig <- plot_ly(
  type='treemap',
  labels=org$v3,
  parents=org$v2)
fig

library(lubridate)
fechas1 <- incidents[c(1)]
fechas2 <- incidents[c(2)]
fechas1$first_event_ts <- date(ymd_hms(fechas1$first_event_ts))
fechas2$first_alert_ts <- date(ymd_hms(fechas1$first_alert_ts))
fechas1$dia <- day(fechas1$first_event_ts)
fechas1$mes <- month(fechas1$first_event_ts)
fechas1 <- as.data.frame(table(fechas1[c(2,3)]))
fechas1$dia<-as.numeric(fechas1$dia)
fechas1$mes<-as.numeric(fechas1$mes)
p1 <- ggplot(fechas1, aes(x=mes, y=dia, size = Freq, fill=dia)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.01, 5.4), name="Ataques por día") +
  scale_fill_viridis() +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Día") +
  xlab("Mes") +
  theme(legend.position = "none") +
  scale_x_continuous(labels=as.character(x),breaks=x)
p1
pp <- ggplotly(p, tooltip="text")
pp


