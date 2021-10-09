#### Packages
library(dismo)

#### Extracting training and testing data
# values for current observation
presvals = combined %>%
  filter(pa == 1)
presvals$pa = NULL # remove presence/background factor variable

# current observations coordinates
pts_subset_df = as.data.frame(pts_subset@coords)

# background observation coordinates
backgr <- as.data.frame(pts_background@coords)


nr <- nrow(presvals) # number of current observations
set.seed(9)
s <- sample(nr, 0.25 * nr) # sample a quarter of indices from data
pres_train <- pts_subset_df[-s, ] # create training dataset with sample removed 
pres_test <- pts_subset_df[s, ] # create testing dataset with sample


nr <- nrow(backgr) # number of background observations
set.seed(9)
s <- sample(nr, 0.25 * nr) # sample a quarter of indices from data
back_train <- backgr[-s, ] # create training dataset with sample removed
back_test <- backgr[s, ] # create testing dataset with sample

# spatial sorting bias calculations
i <- pwdSample(pres_test, back_test, pres_train, n=1, tr=0.1) # pairwise distance sampling algorithm
pres_test_pwd <- pres_test[!is.na(i[,1]), ]
back_test_pwd <- back_test[na.omit(as.vector(i)), ]
sb <- ssb(pres_test_pwd, back_test_pwd, pres_train)
sb[1]/ sb[2]

# MODEL
# run model on training data
bc <- bioclim(variable_stack, pres_train)

# EVALUATION
# evaluate your model on testing data
e = evaluate(bc, p=pres_test_pwd, a=back_test_pwd, x=variable_stack)
plot(e,"ROC")






