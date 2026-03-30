library(ggplot2)
library(data.table)
library(ggforce)
library(ggsci)
library(effectsize)
library(patchwork)

# =============================================================================
# Getting SD estimates from 100 sims per sample size
# =============================================================================

ss <- c(2,3,5,10,24,30,50,100) # sample sizes

pilot.sd = rbindlist(lapply(ss, function(n) data.frame(Samples = n, sapply(1:100,function(x) sd(rnorm(n,0,1))))))

colnames(pilot.sd) <- c("Samples","sd")
pilot.sd$Samples = factor(as.character(pilot.sd$Samples),levels=ss)

ggplot(pilot.sd, aes(x = Samples, y = sd,group = Samples)) +  geom_sina() +
  theme_classic(18) + theme(strip.background = element_blank()) + 
  geom_hline( aes(yintercept=1,color ="True SD"),linetype = "dotted") +
  scale_color_manual(values="red3") +
  theme(legend.title = element_blank(),legend.position = c(0.85,0.75)) +
  xlab("Pilot sample size") + 
  ylab("SD estimated for each pilot study") +
  labs(caption = "*Each dot is one possible pilot study")

# =============================================================================
# Getting Cohen's D from estimates for various sample sizes
# Cohen's D adjusted fro small samples
# =============================================================================

ss <- 2*c(2,3,5,10,24,30,50,100)
pilot.d = rbindlist(lapply(ss, function(n) data.frame(Samples = n/2, sapply(1:100,function(x) {t = rnorm(n,0,1);  group = factor(rep(c("A","B"),each=n/2)); cohens_d(t~group,adjust=F)$Cohens_d  }))))
colnames(pilot.d) <- c("Samples","d")
pilot.d$Samples = factor(as.character(pilot.d$Samples),levels=ss/2)

ggplot(pilot.d, aes(x = Samples, y = d,group = Samples)) +  geom_sina() +
  theme_classic(18) + theme(strip.background = element_blank()) + 
  geom_hline( aes(yintercept=0,color ="True Cohen's d"),linetype = "dotted") +
  geom_hline( aes(yintercept=0.5,color ="Medium effect"),linetype = "dashed") +
  geom_hline( aes(yintercept=0.8,color ="Large effect"),linetype = "solid") +
  geom_hline( aes(yintercept=-0.5,color ="Medium effect"),linetype = "dashed") +
  geom_hline( aes(yintercept=-0.8,color ="Large effect"),linetype = "solid") +
  scale_color_nejm() +
  theme(legend.title = element_blank(),legend.position = c(0.85,0.8)) +
  xlab("Pilot sample size per group") + 
  ylab("Cohen's d of each pilot study") +
  labs(caption = "*Each dot is one possible pilot study")


# =============================================================================
# Getting Hedge's D from estimates for various sample sizes
# Cohen's D adjusted fro small samples
# =============================================================================

ss <- 2*c(2,3,5,10,24,30,50,100)
pilot.d = rbindlist(lapply(ss, function(n) data.frame(Samples = n/2, sapply(1:100,function(x) {t = rnorm(n,0,1);  group = factor(rep(c("A","B"),each=n/2)); cohens_d(t~group,adjust=T)$Hedges_g  }))))
colnames(pilot.d) <- c("Samples","d")
pilot.d$Samples = factor(as.character(pilot.d$Samples),levels=ss/2)

ggplot(pilot.d, aes(x = Samples, y = d,group = Samples)) +  geom_sina() +
  theme_classic(18) + theme(strip.background = element_blank()) + 
  geom_hline( aes(yintercept=0,color ="True Cohen's d"),linetype = "dotted") +
  geom_hline( aes(yintercept=0.5,color ="Medium effect"),linetype = "dashed") +
  geom_hline( aes(yintercept=0.8,color ="Large effect"),linetype = "solid") +
  geom_hline( aes(yintercept=-0.5,color ="Medium effect"),linetype = "dashed") +
  geom_hline( aes(yintercept=-0.8,color ="Large effect"),linetype = "solid") +
  scale_color_nejm() +
  theme(legend.title = element_blank(),legend.position = c(0.85,0.8)) +
  xlab("Pilot sample size per group") + 
  ylab("Hedge's g of each pilot study") +
  labs(caption = "*Each dot is one possible pilot study")


