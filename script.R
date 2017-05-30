library("readxl");
library("ggplot2");

# Load MT data
data.MT <- read_excel("vlook_up_MT.xlsx", sheet = "Exp3 vs Exp4");
data.MT <- cbind.data.frame(data.MT, "MT");
colnames(data.MT) <- c("Protein", "LFQ_Exp4", "LFQ_Exp3", "Src");

# Load GFP data
data.GFP <- read_excel("vlook_up_GFP.xlsx", sheet = "Exp3 vs Exp4");
data.GFP <- cbind.data.frame(data.GFP, "GFP");
colnames(data.GFP) <- c("Protein", "LFQ_Exp4", "LFQ_Exp3", "Src");

# Combine data
data.all <- rbind.data.frame(data.MT, data.GFP);

# Fits
fit.MT <- lm(LFQ_Exp4 ~ LFQ_Exp3, data = data.all[data.all$Src == "MT", ]);
fit.GFP <- lm(LFQ_Exp4 ~ LFQ_Exp3, data = data.all[data.all$Src == "GFP", ]);

# Individual plots
gg <- ggplot(data.all, aes(x = LFQ_Exp3, y = LFQ_Exp4));
gg <- gg + geom_point();
gg <- gg + facet_wrap(~ Src);
gg <- gg + stat_smooth(method = "lm", col = "red");
gg <- gg + theme_bw();
gg <- gg + labs(
    title = "LFQ(Exp3) vs. LFQ(Exp4)",
    subtitle = (sprintf(
        "\nMT:  R^2 = %4.3f, beta0 = %4.3f +- %4.3f, beta1 = %4.3f +- %4.3f
		 \nGFP: R^2 = %4.3f, beta0 = %4.3f +- %4.3f, beta1 = %4.3f +- %4.3f",
        summary(fit.MT)$r.squared,
        summary(fit.MT)$coef[1, 1],
        summary(fit.MT)$coef[1, 2],
        summary(fit.MT)$coef[2, 1],
        summary(fit.MT)$coef[2, 2],
        summary(fit.GFP)$r.squared,
        summary(fit.GFP)$coef[1, 1],
        summary(fit.GFP)$coef[1, 2],
        summary(fit.GFP)$coef[2, 1],
        summary(fit.GFP)$coef[2, 2])),
    x = "LFQ(Exp3)",
    y = "LFQ(Exp4)");
gg;

# Match and combine data
commonProt <- intersect(data.MT$Protein, data.GFP$Protein);
data.matched <- data.MT[data.MT$Protein %in% commonProt, ];
data.matched <- cbind.data.frame(
	data.matched, 
	data.GFP[match(data.MT$Protein, data.GFP$Protein), ]);
colnames(data.matched) <- c(
	paste(colnames(data.MT), ".MT", sep = ""),
	paste(colnames(data.GFP), ".GFP", sep = ""));
# Sanity check 
all(data.matched[, 1] == data.matched[, 5]);


data.diff <- cbind.data.frame(
	diff.Exp3 = data.matched[, "LFQ_Exp3.MT"] - data.matched[, "LFQ_Exp3.GFP"],
	diff.Exp4 = data.matched[, "LFQ_Exp4.MT"] - data.matched[, "LFQ_Exp4.GFP"]);


gg <- ggplot(data.diff, aes(x = diff.Exp3, y = diff.Exp4));
gg <- gg + geom_point();
gg <- gg + stat_smooth(method = "lm", col = "red");
gg <- gg + theme_bw();
gg <- gg + labs(
	title = "LFQ(MT)-LFQ(GFP) in Exp3 vs. LFQ(MT)-LFQ(GFP) in Exp4",
	x = "LFQ(MT)-LFQ(GFP) in Exp3",
	y = "LFQ(MT)-LFQ(GFP) in Exp4");
gg;

x1 <- seq(15, 25, length.out = 300);
y1 <- x1 + runif(length(x1), min = 1, max = 5);

df1 <- cbind.data.frame(x1, y1);
ggplot(df1, aes(x1, y1)) + geom_point() + theme_bw();

x2 <- seq(18, 33, length.out = 300);
y2 <- x2 + runif(length(x2), min = 2, max = 3);

df2 <- cbind.data.frame(x2, y2);
ggplot(df2, aes(x2, y2)) + geom_point() + theme_bw();

df_diff <- cbind.data.frame(
	xdiff = x1 - x2,
	ydiff = y1 - y2);
ggplot(df_diff, aes(xdiff, ydiff)) + geom_point() + theme_bw();