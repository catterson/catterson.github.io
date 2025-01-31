## CHECK-IN

## LOADING DATA.
d <- read.csv("~/Dropbox/!GRADSTATS/Datasets/Grad Onboard 2025/grad_onboard_SP25.csv",
              stringsAsFactors = T)
d
head(d)
nrow(d)
ncol(d)
length(d)
names(d)

## thinking about epistemology.
## positivist = truth exists and we will someday get there (it is possible)
## post-positivism = truth exists but humans are imperfect.
## social construction = trust is created by people; subjective
## anti-positivist = there is no truth; seeking truth is dangerous and used by people in power to control [foucault]

#d$epistemology <- as.factor(d$epistemology)
levels(d$epistemology) <- c("soccon", "postpos", "pos", "antipos")
d$epistemology
plot(d$epistemology)
summary(d$epistemology)
table(d$epistemology)

plot(d$consent)


## GOAL : remove non-consenting person from the dataset.
## identify the dataset
## identify the variable
d$consent
## find the individual in the variable who did not consent
plot(d$consent)
levels(d$consent)
d$consent[33] # try to write code that will work no matter the data!
d[33,]
d[d$consent == "I am the professor and you should remove me.",]

## remove that individual.
d[d$consent == "I am the professor and you should remove me.",] <- NA

plot(d$consent) # testing
d$consent

##### working with numeric data
hist()

# Graph the variables `self.skills` and `class.skills` side by side using the par() function. Change the formatting of the graph to make it look ready for presentation. Add vertical lines to each graph to illustrate the mean (solid line) and standard deviation (dashed lines).

hist(d$class.skills) 
# things we see / learn : 
## most people think classmates are better than them???
## most people think classmates are better than midpoint of the scale.
## a few people gave very low ratings for skills.

# questions we have
## how do these ratings compare to self-ratings?
## what's going on with the super low ratings?
  ## no confidence in their classmates?
  ## general uncertainty and no preconceived notions (I AM BIAS FREE)
  ## they are super smart and no one can live up to their expectations.
  ## they feel anxious and this is motivated reasoning (everyone is also gonna struggle, right?!?!?!)
## 
par(mfrow = c(1,2))
hist(d$self.skills, main = "", xlab = "Self-Perceptions of Skills",
     ylim = c(0,20))
abline(v = mean(d$self.skills, na.rm = T), lwd = 5)

hist(d$class.skills, main = "", xlab = "Perceptions of Classmates' Skills",
     ylim = c(0,20)) 
abline(v = mean(d$class.skills, na.rm = T), lwd = 5)

mean(d$self.skills, na.rm = T)
mean(d$class.skills, na.rm = T)

## BREAK TIME : MEET BACK AT 10:55

# Below each graph, report the mean and standard deviation of both variables, and interpret what these statistics tell you about the individuals in our class. (Who cares? What do these statistics tell us?)

par(mfrow = c(1,1))
plot(d$self.skills)
abline(h = mean(d$self.skills, na.rm = T), lwd = 5)

diffy <- d$self.skills - mean(d$self.skills, na.rm = T) # how far each person is from the mean

sum.square.diffy <- sum(diffy^2, na.rm = T)

sum.square.diffy/length(diffy)

sqrt(sum.square.diffy/((length(diffy)-1)))
sd(d$self.skills, na.rm = T)

