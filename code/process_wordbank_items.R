#packages
library(ggplot2)
library(plyr)
library(dplyr)
library(drc)
library(robustHD)
library(car)

#Parameters
IGNORE_INFLECTIONS = T
MIN_FEATURE_FREQ = 1000
WINSORIZATION_LEVEL = 10
NUM_PCS = 10
RE_RUNNING = T

#Load data downloaded from Wordbank
ws_items = read.csv('ws_items.csv')
wg_items = read.csv('wg_items.csv')
comp_items = read.csv('wg_items_comprehension.csv')

if (!RE_RUNNING){
  #Select the columns representing production and comprehension of each word,
  #merging the production from the WS and WG forms
  ws_items = ws_items[ws_items$type == "word",]
  wg_items = wg_items[wg_items$type == "word",]
  comp_items = comp_items[comp_items$type == "word",]
  all_items = merge(ws_items, wg_items, by = "definition", all = T)
  all_items = all_items[, c("definition", "X8", "X9", "X10", "X11",
                            "X12", "X13", "X14", "X15", "X16.x", "X16.y",
                            "X17.x", "X17.y", "X18.x", "X18.y",
                            "X19", "X20", "X21", "X22", "X23", 
                            "X24", "X25", "X26", "X27", "X28", "X29",
                            "X30")]
  comp_items = comp_items[, c("definition", "X8", "X9", "X10", "X11", 
                              "X12", "X13", "X14", "X15", "X16", "X17", "X18")]
  
  #fit logistic curve to observed proportions of comprehension and production for each word
  #reusable function to fit aoa for each word of a df
  fit_aoa <- function(items_df, columns_range, column_ages){
      nitems = dim(items_df)[1]
      aoa = numeric(nitems)
      for (i in 1:nitems){
          model = drm(unlist(items_df[i, columns_range]) ~ column_ages,
                      fct = L.3(),
                      type = "continuous",
                      upperl = c(-.001, 1.000, 50), #max proportion is 1
                      lowerl = c(-Inf, .99, 0)) # all children assumed to learn the word eventually
          bde = coef(model)
          aoa[i] = bde[3] + (log(2*bde[2] -1)/bde[1])
      }
      aoa
  }
  #apply for production
  prod_ages = c(8,9,10,11,12,13,14,15,16,16,17,17,18,18,19,20,21,22,23,24,25,26,27,28,29,30)
  aop = fit_aoa(all_items, 2:27, prod_ages)
  #apply for comprehension
  comp_ages = c(8,9,10,11,12,13,14,15,16,17,18)
  aoc = fit_aoa(comp_items, 2:12, comp_ages)
  #add fitted AOC and AOP values to comprehension and production data frames
  comp_data = data.frame(comp_items$definition, aoc)
  names(comp_data) = c("word", "aoc")
  prod_data = data.frame(all_items$definition, aop)
  names(prod_data) = c("word", "aop")
  
  #remove mommy, daddy, brother, sister because 50% comprehension age could not be estimated
  comp_data = comp_data[!(comp_data$word %in% c("mommy*", "daddy*", "brother", "sister")),]
  
  #merge comp and prod data
  aoa_data = merge(prod_data, comp_data, by = 'word', all.x = T, all.y = T)
  
  #drop unused words
  aoa_data = aoa_data[!(aoa_data$word %in% 
                            c("inside/in",
                              "babysitter's name",
                              "child's own name",
                              "pet's name",
                              "give me five!",
                              "go potty",
                              "gonna get you!",
                              "lemme/let me",
                              "on top of",
                              "so big!",
                              "this little piggy",
                              "turn around",
                              "penis*",
                              "vagina*")),]
  
  #drop any parenthetical comments from words
  aoa_data$word = gsub(" \\(.+\\)", "", aoa_data$word)
  
  #combine polysemous words to minimum AOA and single metric valeus
  aoa_data = aoa_data %>% group_by(word) %>%
  summarize(aop = min(aop, na.rm = T),
  aoc = min(aoc, na.rm = T)) %>% data.frame()
  aoa_data[is.infinite(aoa_data$aoc), "aoc"] <- NA
  
  #relabel words
  aoa_data[aoa_data$word=="a lot", "word"] = "lot"
  aoa_data[aoa_data$word=="all gone", "word"] = "gone"
  aoa_data[aoa_data$word=="belly button", "word"] = "belly_button"
  aoa_data[aoa_data$word=="french fries", "word"] = "french_fry"
  aoa_data[aoa_data$word=="gas station", "word"] = "gas_station"
  aoa_data[aoa_data$word=="have to", "word"] = "have_to"
  aoa_data[aoa_data$word=="gotta/got to", "word"] = "gotta"
  aoa_data[aoa_data$word=="baa baa", "word"] = "baa"
  aoa_data[aoa_data$word=="buttocks/bottom*", "word"] = "buttocks"
  aoa_data[aoa_data$word=="bubbles", "word"] = "bubble"
  aoa_data[aoa_data$word=="boots", "word"] = "boot"
  aoa_data[aoa_data$word=="beads", "word"] = "bead"
  aoa_data[aoa_data$word=="beans", "word"] = "bean"
  aoa_data[aoa_data$word=="camping", "word"] = "camp"
  aoa_data[aoa_data$word=="carrots", "word"] = "carrot"
  aoa_data[aoa_data$word=="cheerios", "word"] = "cheerio"
  aoa_data[aoa_data$word=="choo choo", "word"] = "choo"
  aoa_data[aoa_data$word=="church*", "word"] = "church"
  aoa_data[aoa_data$word=="daddy*", "word"] = "daddy"
  aoa_data[aoa_data$word=="did/did ya", "word"] = "did"
  aoa_data[aoa_data$word=="gloves", "word"] = "glove"
  aoa_data[aoa_data$word=="gonna/going to", "word"] = "gonna"
  aoa_data[aoa_data$word=="grandma*", "word"] = "grandma"
  aoa_data[aoa_data$word=="grandpa*", "word"] = "grandpa"
  aoa_data[aoa_data$word=="grapes", "word"] = "grape"
  aoa_data[aoa_data$word=="green beans", "word"] = "green_bean"
  aoa_data[aoa_data$word=="grrr", "word"] = "grr"
  aoa_data[aoa_data$word=="hafta/have to", "word"] = "have_to"
  aoa_data[aoa_data$word=="high chair", "word"] ="high_chair"
  aoa_data[aoa_data$word=="I", "word"] = "i"
  aoa_data[aoa_data$word=="ice cream", "word"] = "ice_cream"
  aoa_data[aoa_data$word=="keys", "word"] = "key"
  aoa_data[aoa_data$word=="uh-oh", "word"] = "uhoh"
  aoa_data[aoa_data$word=="lawn mower", "word"] = "lawnmower"
  aoa_data[aoa_data$word=="lips", "word"] = "lip"
  aoa_data[aoa_data$word=="living room", "word"] = "living_room"
  aoa_data[aoa_data$word=="mittens", "word"] = "mitten"
  aoa_data[aoa_data$word=="mommy*", "word"] = "mommy"
  aoa_data[aoa_data$word=="need/need to", "word"] = "need"
  aoa_data[aoa_data$word=="next to", "word"] = "next"
  aoa_data[aoa_data$word=="night night", "word"] = "night_night"
  aoa_data[aoa_data$word=="noodles", "word"] = "noodle"
  aoa_data[aoa_data$word=="nuts", "word"] = "nut"
  aoa_data[aoa_data$word=="owie/boo boo", "word"] = "booboo"
  aoa_data[aoa_data$word=="pajamas", "word"] = "pajama"
  aoa_data[aoa_data$word=="peanut butter", "word"] = "peanut_butter"
  aoa_data[aoa_data$word=="peas", "word"] = "pea"
  aoa_data[aoa_data$word=="play dough", "word"] = "playdoh"
  aoa_data[aoa_data$word=="play pen", "word"] = "playpen"
  aoa_data[aoa_data$word=="potato chip", "word"] = "chip"
  aoa_data[aoa_data$word=="quack quack", "word"] = "quack"
  aoa_data[aoa_data$word=="rocking chair", "word"] = "rocking_chair"
  aoa_data[aoa_data$word=="scared", "word"] = "scare"
  aoa_data[aoa_data$word=="shh/shush/hush", "word"] = "shh"
  aoa_data[aoa_data$word=="shopping", "word"] = "shop"
  aoa_data[aoa_data$word=="soda/pop", "word"] = "soda"
  aoa_data[aoa_data$word=="stairs", "word"] = "stair"
  aoa_data[aoa_data$word=="teddybear", "word"] = "teddy"
  aoa_data[aoa_data$word=="thank you", "word"] = "thank_you"
  aoa_data[aoa_data$word=="tired", "word"] = "tire"
  aoa_data[aoa_data$word=="tissue/kleenex", "word"] = "tissue"
  aoa_data[aoa_data$word=="try/try to", "word"] = "try"
  aoa_data[aoa_data$word=="TV", "word"] =  "tv"
  aoa_data[aoa_data$word=="uh oh", "word"] = "uhoh"
  aoa_data[aoa_data$word=="vitamins", "word"] = "vitamin"
  aoa_data[aoa_data$word=="wanna/want to", "word"] = "wanna"
  aoa_data[aoa_data$word=="washing machine", "word"] = "washing_machine"
  aoa_data[aoa_data$word=="woof woof", "word"] = "woof"
  aoa_data[aoa_data$word=="yum yum", "word"] =  "yum"
  
  write.table(aoa_data, file = "aoa_data.csv", sep = ",",  row.names = F, quote = F)
}
####### run from here when re-running

aoa_data = read.csv("aoa_data.csv", na.strings = c("NA","Inf"))
word_stats = read.csv("word_stats.csv")

data = merge(aoa_data, word_stats, by = 'word')
data = data[,-4] # remove an unneeded index column

#process solo and final frequency
data$solo_freqs = log(data$solo_freqs)
data$final_freqs = log(data$final_freqs)
data$solo_freqs[is.infinite(data$solo_freqs)] = 0
data$final_freqs[is.infinite(data$final_freqs)] = 0
solo_lm = with(data, lm(solo_freqs ~ logfreq))
final_lm = with(data, lm(final_freqs ~ logfreq))
data$solo_freqs = solo_lm$residuals
data$final_freqs = final_lm$residuals

#scale variables
data$solo_freqs = scale(data$solo_freqs)
data$final_freqs = scale(data$final_freqs)
data$MLU = scale(data$MLU)
data$logfreq = scale(data$logfreq)

#scale outcome measures
scaled_data = data
scaled_data$aop = scale(scaled_data$aop)
scaled_data$aoc = scale(scaled_data$aoc)

if(IGNORE_INFLECTIONS) { 
    infl_str = "noinfl"
} else {
    infl_str = "inflections"
}

#Load the frame data and co-occurrence data
frame_data = read.csv(paste("word_frame_freqs_", infl_str, "_", MIN_FEATURE_FREQ, ".csv", sep = ""))
coals_features = read.csv(paste("coals_features_", infl_str, "_", MIN_FEATURE_FREQ, ".csv", sep = ""))

#Clean the frame data
frame_data$X = c()
#normalize
num_frames = dim(frame_data)[2] - 1
word_totals = rowSums(frame_data[, 1:num_frames])
frame_data[, 1:num_frames] = frame_data[, 1:num_frames]/word_totals
#pca on frame data
pca_frame = prcomp(frame_data[,1:num_frames], scale. = T)
#pca on coals data
pca_coals = prcomp(coals_features[,2:(dim(coals_features)[2]-1)], scale. = T)
# merge the top 100 PCs
pc_data = data.frame(cbind(frame_data$word, scale(pca_frame$x[,1:100]),scale(pca_coals$x[,1:100]) ))
names(pc_data)[1]= "word_id"
pc_data$word_id = frame_data$word
#merge pcs with regression data
pc_aoa_data = merge(pc_data, scaled_data, by.x = "word_id", by.y = "word")

#convert to original scale (months)
pc_aoa_data$aop = sd(aoa_data$aop)* pc_aoa_data$aop + mean(aoa_data$aop)
pc_aoa_data$aoc = sd(aoa_data$aoc, na.rm = T)* pc_aoa_data$aoc + mean(aoa_data$aoc, na.rm = T)

###describe the PCs
describe_pc <- function(n, nwords = 10, type = "frame"){
  if (type == "frame"){
    print("lowest frames")
    print(sort(pca_frame$rotation[,n])[1:nwords])
    print("highest frames")
    print(sort(pca_frame$rotation[,n], decreasing = TRUE)[1:nwords])
    print("lowest words")
    print(frame_data$word[sort(pca_frame$x[,n], index.return = TRUE)[2]$ix[1:nwords]])
    print("highest words")
    print(frame_data$word[sort(pca_frame$x[,n], index.return = TRUE, decreasing= TRUE)[2]$ix[1:nwords]])
  } else if (type == "coals"){
    print("lowest context words")
    print(sort(pca_coals$rotation[,n])[1:nwords])
    print("highest context words")
    print(sort(pca_coals$rotation[,n], decreasing = TRUE)[1:nwords])
    print("lowest words")
    print(coals_features$word.1[sort(pca_coals$x[,n], index.return = TRUE)[2]$ix[1:nwords]])
    print("highest words")
    print(coals_features$word.1[sort(pca_coals$x[,n], index.return = TRUE, decreasing= TRUE)[2]$ix[1:nwords]])
  }
    else {
      print("INVALID INPUT")
  }
}

##Scree plots
plot(seq(30), 100*(pca_frame["sdev"][[1]][1:30]^2)/sum(pca_frame["sdev"][[1]]^2),
     type = 'o', pch=19, ylab = '% Variance Explained', xlab = 'Number of components', main = 'Syntactic', cex.main=1.6, cex.lab=1.3, cex.axis=1.1)
plot(seq(30), 100*(pca_coals["sdev"][[1]][1:30]^2)/sum(pca_coals["sdev"][[1]]^2),
     type = 'o', pch=19, ylab = '% Variance Explained', xlab = 'Number of components', main = 'Thematic', cex.main=1.6, cex.lab=1.3, cex.axis=1.1)

##frame PCs
#FPC1: -PRONOUN +VERB
describe_pc(1, 10, "frame")
#FPC2: -NOUN +VERB
describe_pc(2, 10, "frame")
#FPC3: - HELPING VERB + PRONOUN
describe_pc(3, 10, "frame")
#FPC4: - (HELPING VERB, YOU) + (WH WORD) 
describe_pc(4, 10, "frame")
#FPC5: - (YOU) + OTHER PRONOUNS 
describe_pc(5, 10, "frame")
#FPC6: - (MISC, QUESTION) + (ADJECTIVE, 3 P PRON)
describe_pc(6, 10, "frame")
#FPC7: - (MISC) + (MISC, QUESTION)
describe_pc(7, 10, "frame")
#FPC8: - (MISC) + (LOCATION)
describe_pc(8, 10, "frame")
#FPC9: - (TRANSITION, MISC) + (HELPING VERBS, MISC)
describe_pc(9, 10, "frame")
#FPC10: - OTHER VERBS + (DON'T)
describe_pc(10, 10, "frame")

##context PCs
#CPC1: - NARRATIVE + FOOD
describe_pc(1, 10, "coals")
#CPC2: - FOOD + ANIMAL SOUNDS
describe_pc(2, 10, "coals")
#CPC3: - (FOOD, TIME) + MOVING OBJECTS
describe_pc(3, 10, "coals")
#CPC4: - (FOOD, ACKNOWLEDGING CHILD TALK) + DAILY ACTIVITIES
describe_pc(4, 10, "coals")
#CPC5: - PLAYING WITH TOYS + PLAYING WITH PEOPLE
describe_pc(5, 10, "coals")
#CPC6: - ACTIONS + BODY
describe_pc(6, 10, "coals")
#CPC7: - COLORS + FOOD
describe_pc(7, 10, "coals")
#CPC8: - CLOTHING + VEHICLES
describe_pc(8, 10, "coals")
#CPC9: - DAILY ACTIVITIES + ANIMAl SOUNDS
describe_pc(9, 10, "coals")
#CPC10: - (MISC) + (COLORS, SKY)
describe_pc(10, 10, "coals")

#Winsorize
pc_aoa_data[, 2:201] = apply(pc_aoa_data[, 2:201], 2, FUN = function(x){winsorize(x, const = WINSORIZATION_LEVEL)})
pc_aoa_data[, 2:201] = scale(pc_aoa_data[, 2:201])

###Show that PCs represent syntactic vs. semantic categories
#get categories
pc_category_data = merge(pc_aoa_data, ws_items[, c("definition", "category")],
                         all.x = T, by.x = "word_id", 
                         by.y ="definition")
pc_category_data$category = as.character((pc_category_data$category))
pc_category_data$category[
  pc_category_data$word %in% c("baa", "bead", "bean", "belly_button", "booboo", "boot",
                               "bubble", "buttocks", "call", "camp", "can", "carrot",
                               "cheerio", "chicken", "chip", "choo", "church",
                               "clean", "daddy", "did", "dress", "drink", "dry",
                               "fish", "french_fry", "gas_station", "glove", "gone",
                               "gonna", "gotta", "grandma", "grape","green_bean",
                               "grr", "have_to", "high_chair", "i", "ice_cream",
                               "in", "inside", "key", "lawnmower", "lip", "little",
                               "living_room", "lot", "mitten", "mommy", "need",
                               "next", "night_night", "noodle", "nut", "orange",
                               "pajama", "pea", "peanut_butter","playdoh", "playpen",
                               "quack", "rocking_chair", "scare", "shh", "shop",
                               "slide", "soda", "stair", "swing", "teddy", "thank_you",
                               "tire", "tissue", "toy", "try", "tv", "uhoh", "vitamin",
                               "wanna", "washing_machine", "watch", "water", 
                               "woof", "work", "yum")] =
  c("sounds", "clothing", "food_drink", "body_parts", "body_parts", "clothing",
    "toys", "body_parts", "games_routines", "places", NA, "food_drink",
    "food_drink", NA, "food_drink", "sounds", "places",
    NA, "people", "helping_verbs", "clothing", "action_words", NA,
    NA, "food_drink", "places", "clothing", "descriptive_words", 
    "helping_verbs", "helping_verbs", "people", "food_drink", "food_drink",
    "sounds", "helping_verbs", "furniture_rooms", "pronouns", "food_drink",
    "locations", "locations", "household", "outside", "body_parts","descriptive_words", 
    "furniture_rooms", "quantifiers", "clothing","people", "helping_verbs",
    "locations", "games_routines", "food_drink","food_drink", NA,
    "clothing", "food_drink", "food_drink", "toys", "furniture_rooms",
    "sounds", "furniture_rooms", "descriptive_words", "games_routines", "games_routines",
    NA, "food_drink", "furniture_rooms", NA, "toys", "games_routines",
    "descriptive_words", "household", "toys", "helping_verbs", "furniture_rooms", "sounds", "food_drink",
    "helping_verbs", "furniture_rooms", NA,NA, 
    "sounds", NA, "sounds")


pc_category_data$syn_category = mapvalues(
  pc_category_data$category,
  to = c(
    "quantifiers", "locations", NA, "things", "things", "helping_verbs",
    "connecting_words", "things", "things", "descriptive_words", "things",
    NA, "things", "things", "things", "things", NA, "things", "things",
    "action_words", NA, "pronouns", "question_words"
  ),
  from = unique(pc_category_data$category)
)
pc_category_data$sem_category = mapvalues(
  pc_category_data$category,
  to = c(
    NA, NA, NA, "vehicles", "animals", NA,
    NA, "body_parts", "food_drink", NA, "people",
    NA, "outside", "toys", "furniture_rooms", "household", NA, "places", "clothing",
    NA, NA, NA, NA
  ),
  from = unique(pc_category_data$category)
)

#get f-statistics for categories as a function of each PC
fstat = c(summary(lm(PC1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC1.1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC2 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC2.1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC3 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC3.1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC4 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC4.1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC5 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC5.1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC6 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC6.1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC7 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC7.1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC8 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC8.1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC9 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC9.1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC10 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC10.1 ~ sem_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC1 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC1.1 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC2 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC2.1 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC3 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC3.1 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC4 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC4.1 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC5 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC5.1 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC6 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC6.1 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC7 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC7.1 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC8 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC8.1 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC9 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC9.1 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC10 ~ syn_category, pc_category_data))$fstatistic["value"],
          summary(lm(PC10.1 ~ syn_category, pc_category_data))$fstatistic["value"])

category_anova_data = tibble(
  feature_type = rep(c("Adjacent (Syntactic)", "Non-adjacent (Thematic)"), 20),
  category_type = rep(c("Thematic", "Syntactic"), each = 20),
  fstat = fstat,
  feature = rep(c('S1', 'T1', 'S2', 'T2', 'S3', 'T3', 'S4', 'T4', 'S5', 'T5', 'S6', 'T6', 'S7', 'T7', 'S8', 'T8', 'S9', 'T9', 'S10', 'T10'),2),
  feature_x = c(rep(c(1.45,2.35),10), rep(c(0.65,1.55),10))
)

#parametric check for interaction & post hoc
summary(aov(fstat ~ feature_type*category_type, category_anova_data)) # .0001
summary(lm(fstat ~ feature_type, category_anova_data[category_anova_data$category_type == "Thematic",])) # .0008
summary(lm(fstat ~ feature_type, category_anova_data[category_anova_data$category_type == "Syntactic",])) # .005
#nonparametric pairwise
fsem = category_anova_data$fstat[category_anova_data$feature_type == "Adjacent (Syntactic)" & category_anova_data$category_type == "Thematic"]
csem = category_anova_data$fstat[category_anova_data$feature_type == "Non-adjacent (Thematic)" & category_anova_data$category_type == "Thematic"]
fsyn = category_anova_data$fstat[category_anova_data$feature_type == "Adjacent (Syntactic)" & category_anova_data$category_type == "Syntactic"]
csyn = category_anova_data$fstat[category_anova_data$feature_type == "Non-adjacent (Thematic)" & category_anova_data$category_type == "Syntactic"]
wilcox.test(fsyn, fsem, paired=T) #.002
wilcox.test(csyn, csem, paired=T) #.04
rank_biserial <- function(v1, v2){
  sum(rank(abs(v1-v2)) * (((v1-v2) < 1)*2-1))/sum(seq(length(v2)))
}
median(fsyn)
median(fsem)
median(csyn)
median(csem)
rank_biserial(fsyn, fsem)
rank_biserial(csyn, csem)

#Plot feature F-scores for syntactic and thematic categories
barplot_data = data_frame(
  feature_type = c("Adjacent (Syntactic)", "Non-adjacent (Thematic)", "Adjacent (Syntactic)", "Non-adjacent (Thematic)"),
  category_type = c("Syntactic", "Syntactic", "Thematic", "Thematic"),
  means = c(mean(category_anova_data[category_anova_data$feature_type == "Adjacent (Syntactic)" & category_anova_data$category_type == "Syntactic",]$fstat),
            mean(category_anova_data[category_anova_data$feature_type == "Non-adjacent (Thematic)" & category_anova_data$category_type == "Syntactic",]$fstat),
            mean(category_anova_data[category_anova_data$feature_type == "Adjacent (Syntactic)" & category_anova_data$category_type == "Thematic",]$fstat),
            mean(category_anova_data[category_anova_data$feature_type == "Non-adjacent (Thematic)" & category_anova_data$category_type == "Thematic",]$fstat)),
  sem = c(sd(category_anova_data[category_anova_data$feature_type == "Adjacent (Syntactic)" & category_anova_data$category_type == "Syntactic",]$fstat)/sqrt(10),
          sd(category_anova_data[category_anova_data$feature_type == "Non-adjacent (Thematic)" & category_anova_data$category_type == "Syntactic",]$fstat)/sqrt(10),
          sd(category_anova_data[category_anova_data$feature_type == "Adjacent (Syntactic)" & category_anova_data$category_type == "Thematic",]$fstat)/sqrt(10),
          sd(category_anova_data[category_anova_data$feature_type == "Non-adjacent (Thematic)" & category_anova_data$category_type == "Thematic",]$fstat)/sqrt(10))
)

ggplot(barplot_data, aes(x=category_type, y = means, fill = feature_type)) +
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin = means - sem, ymax = means+sem), position = position_dodge()) +
  scale_fill_grey(start = 0.4, end = 0.8, na.value = "red") +
  theme_bw() +
  xlab("Category set")+
  ylab("F-statistic")+
  labs(fill = "Feature type")

ggplot(category_anova_data, aes(x=feature_type, y = fstat, fill=category_type)) +
  geom_line(aes(group=feature, x=feature_x)) +
  geom_point(aes(x=feature_x), size=4, shape=21) +
  scale_fill_grey(start=0.3,end=0.9)+
  scale_x_continuous(
                   limits=c(0.6,2.4),
                   breaks=c(1, 2),
                   labels=c("Frame (Adjacent)", "Co-occurrence (Non-adjacent)")) +
  theme_bw() +
  xlab("Feature type")+
  ylab("F-statistic")+
  labs(fill = "Category set") + theme(text = element_text(size=24))



##Train regression models
base_model_prod = lm(aop~., pc_aoa_data[,c(204,206,205,207, 202)])
no_base_model_prod = lm(aop~., pc_aoa_data[,c(2:(1+NUM_PCS),102:(101+NUM_PCS), 202)])
no_sem_model_prod = lm(aop~., pc_aoa_data[,c(204,206,205,207,2:(1+NUM_PCS), 202)])
no_syn_model_prod = lm(aop~., pc_aoa_data[,c(204,206,205,207,102:(101+NUM_PCS), 202)])
full_model_prod = lm(aop~., pc_aoa_data[,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 202)])
base_model_comp = lm(aoc~., pc_aoa_data[,c(204,206,205,207, 203)])
no_base_model_comp = lm(aoc~., pc_aoa_data[,c(2:(1+NUM_PCS),102:(101+NUM_PCS), 203)])
no_sem_model_comp = lm(aoc~., pc_aoa_data[,c(204,206,205,207,2:(1+NUM_PCS), 203)])
no_syn_model_comp = lm(aoc~., pc_aoa_data[,c(204,206,205,207,102:(101+NUM_PCS), 203)])
full_model_comp = lm(aoc~., pc_aoa_data[,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 203)])

#inspect and evaluate regression models
summary(full_model_prod)
summary(base_model_prod)
summary(no_base_model_prod)
summary(no_syn_model_prod)
summary(no_sem_model_prod)
summary(full_model_comp)
summary(base_model_comp)
summary(no_base_model_comp)
summary(no_syn_model_comp)
summary(no_sem_model_comp)

#likelihood ratio tests
anova(full_model_prod, no_base_model_prod)
anova(full_model_prod, no_sem_model_prod)
anova(full_model_prod, no_syn_model_prod) #ps < .001

anova(full_model_comp, no_base_model_comp)
anova(full_model_comp, no_sem_model_comp)
anova(full_model_comp, no_syn_model_comp) #ps < .001

#VIF
vif(full_model_prod)
vif(full_model_comp)

#Correlation matrix
cor(pc_aoa_data[,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 202)])

#Show that fit coefficients are shared between models
cor(coef(full_model_comp)[2:(1+2*NUM_PCS)], coef(full_model_prod)[2:(1+2*NUM_PCS)])
cor(coef(no_base_model_comp)[2:(1+2*NUM_PCS)], coef(no_base_model_prod)[2:(1+2*NUM_PCS)])

#Plot predictions vs targets 
prediction_plot_data = data.frame(
                               predicted_aop = NA,
                               predicted_aoc = NA,
                               aop = pc_aoa_data$aop, 
                               aoc = pc_aoa_data$aoc,
                               word = pc_aoa_data$word,
                               filtered_word = pc_aoa_data$word,
                               filtered_word_comp = pc_aoa_data$word,
                               stringsAsFactors = F)
set.seed(464482)
#folds
folds = sample(rep(1:10, each = 66))[1:656]
for (i in 1:10){
  fold_model_prod_full = lm(aop~., pc_aoa_data[folds!=i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 202)])
  fold_model_prod_base = lm(aop~., pc_aoa_data[folds!=i,c(204,206,205,207, 202)])
  fold_model_prod_nobase = lm(aop~., pc_aoa_data[folds!=i,c(2:(1+NUM_PCS),102:(101+NUM_PCS), 202)])
  fold_model_prod_nosyn = lm(aop~., pc_aoa_data[folds!=i,c(102:(101+NUM_PCS),204,206,205,207, 202)])
  fold_model_prod_nosem = lm(aop~., pc_aoa_data[folds!=i,c(2:(1+NUM_PCS),204,206,205,207, 202)])
  fold_model_comp_full = lm(aoc~., pc_aoa_data[folds!=i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 203)])
  fold_model_comp_base = lm(aoc~., pc_aoa_data[folds!=i,c(204,206,205,207, 203)])
  fold_model_comp_nobase = lm(aoc~., pc_aoa_data[folds!=i,c(2:(1+NUM_PCS),102:(101+NUM_PCS), 203)])
  fold_model_comp_nosyn = lm(aoc~., pc_aoa_data[folds!=i,c(102:(101+NUM_PCS),204,206,205,207, 203)])
  fold_model_comp_nosem = lm(aoc~., pc_aoa_data[folds!=i,c(2:(1+NUM_PCS),204,206,205,207, 203)])
  fold_predictions_prod_full = predict(fold_model_prod_full,
                                        newdata = pc_aoa_data[folds==i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 202)])
  fold_predictions_prod_base = predict(fold_model_prod_base,
                                        newdata = pc_aoa_data[folds==i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 202)])
  fold_predictions_prod_nobase = predict(fold_model_prod_nobase,
                                       newdata = pc_aoa_data[folds==i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 202)])
  fold_predictions_prod_nosyn = predict(fold_model_prod_nosyn,
                                       newdata = pc_aoa_data[folds==i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 202)])
  fold_predictions_prod_nosem = predict(fold_model_prod_nosem,
                                       newdata = pc_aoa_data[folds==i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 202)])
  fold_predictions_comp_full = predict(fold_model_comp_full,
                                       newdata = pc_aoa_data[folds==i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 203)])
  fold_predictions_comp_base = predict(fold_model_comp_base,
                                       newdata = pc_aoa_data[folds==i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 203)])
  fold_predictions_comp_nobase = predict(fold_model_comp_nobase,
                                         newdata = pc_aoa_data[folds==i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 203)])
  fold_predictions_comp_nosyn = predict(fold_model_comp_nosyn,
                                        newdata = pc_aoa_data[folds==i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 203)])
  fold_predictions_comp_nosem = predict(fold_model_comp_nosem,
                                        newdata = pc_aoa_data[folds==i,c(2:(1+NUM_PCS),102:(101+NUM_PCS),204,206,205,207, 203)])
  prediction_plot_data[folds==i, "predicted_aop"] = fold_predictions_prod_full
  prediction_plot_data[folds==i, "predicted_aoc"] = fold_predictions_comp_full
  prediction_plot_data[folds==i, "predicted_aop_base"] = fold_predictions_prod_base
  prediction_plot_data[folds==i, "predicted_aoc_base"] = fold_predictions_comp_base
  prediction_plot_data[folds==i, "predicted_aop_nobase"] = fold_predictions_prod_nobase
  prediction_plot_data[folds==i, "predicted_aoc_nobase"] = fold_predictions_comp_nobase
  prediction_plot_data[folds==i, "predicted_aop_nosyn"] = fold_predictions_prod_nosyn
  prediction_plot_data[folds==i, "predicted_aoc_nosyn"] = fold_predictions_comp_nosyn
  prediction_plot_data[folds==i, "predicted_aop_nosem"] = fold_predictions_prod_nosem
  prediction_plot_data[folds==i, "predicted_aoc_nosem"] = fold_predictions_comp_nosem
}
prediction_plot_data$filtered_word[abs(prediction_plot_data$predicted_aop - prediction_plot_data$aop) < 99] = NA
prediction_plot_data$filtered_word_comp[abs(prediction_plot_data$predicted_aoc - prediction_plot_data$aoc) < 99] = NA

#RMSE (months)
sqrt(mean((prediction_plot_data$predicted_aop - prediction_plot_data$aop)^2))
sqrt(mean((prediction_plot_data$predicted_aop_base - prediction_plot_data$aop)^2))
sqrt(mean((prediction_plot_data$predicted_aop_nobase - prediction_plot_data$aop)^2))
sqrt(mean((prediction_plot_data$predicted_aop_nosyn - prediction_plot_data$aop)^2))
sqrt(mean((prediction_plot_data$predicted_aop_nosem - prediction_plot_data$aop)^2))
sqrt(mean((prediction_plot_data$predicted_aoc - prediction_plot_data$aoc)^2, na.rm = T))
sqrt(mean((prediction_plot_data$predicted_aoc_base - prediction_plot_data$aoc)^2, na.rm = T))
sqrt(mean((prediction_plot_data$predicted_aoc_nobase - prediction_plot_data$aoc)^2, na.rm = T))
sqrt(mean((prediction_plot_data$predicted_aoc_nosyn - prediction_plot_data$aoc)^2, na.rm = T))
sqrt(mean((prediction_plot_data$predicted_aoc_nosem - prediction_plot_data$aoc)^2, na.rm = T))

#change in order of syntactic and thematic not due to different word set
sqrt(mean((prediction_plot_data$predicted_aop_nosyn - prediction_plot_data$aop)[!is.na(prediction_plot_data$aoc)]^2))
sqrt(mean((prediction_plot_data$predicted_aop_nosem - prediction_plot_data$aop)[!is.na(prediction_plot_data$aoc)]^2))

#overall error on comprehension word set
sqrt(mean((prediction_plot_data$predicted_aop - prediction_plot_data$aop)[!is.na(prediction_plot_data$aoc)]^2))
sqrt(mean((prediction_plot_data$predicted_aoc - prediction_plot_data$aoc)[!is.na(prediction_plot_data$aoc)]^2))

#don't need to rescale to months in revised version
#prediction_plot_data$aop = sd(aoa_data$aop)* prediction_plot_data$aop + mean(aoa_data$aop)
#prediction_plot_data$predicted_aop = sd(aoa_data$aop)* prediction_plot_data$predicted_aop + mean(aoa_data$aop)
#prediction_plot_data$aoc = sd(aoa_data$aoc, na.rm = T)* prediction_plot_data$aoc + mean(aoa_data$aoc, na.rm = T)
#prediction_plot_data$predicted_aoc = sd(aoa_data$aoc, na.rm = T)* prediction_plot_data$predicted_aoc + mean(aoa_data$aoc, na.rm = T)
ggplot(prediction_plot_data, aes(x =predicted_aop, y=aop)) +
    geom_point() +
    geom_text(aes(label = filtered_word, hjust = "left"), nudge_x = .35) +
    xlab("Predicted age of production (months)") +
    ylab("Age of production (months)")+
    labs(title = "Predicted vs. Actual Age of Production") +
    coord_cartesian(xlim = c(11, 37), ylim = c(11,37)) +
    #scale_y_continuous(breaks = c(10,15,20,25,30,35,40)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))

ggplot(prediction_plot_data[which(!is.na(pc_aoa_data$aoc)), ], aes(x =predicted_aoc, y=aoc)) +
  geom_point() +
  geom_text(aes(label = filtered_word_comp, hjust = "left"), nudge_x = .35) +
  xlab("Predicted age of comprehension (months)") +
  ylab("Age of comprehenson (months)")+
  labs(title = "Predicted vs. Actual Age of Comprehension") +
  coord_cartesian(xlim = c(7,33), ylim = c(7,33)) +
  scale_y_continuous(breaks = c(10,20,30)) +
  scale_x_continuous(breaks = c(10,20,30)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))


#Plot final model coefficients
if (NUM_PCS == 10) { #otherwise, figure will not be properly organized
    coefficients_plot_data = data_frame(coef = -abs(full_model_prod$coefficients[-1]))
    coefficients_plot_data$se = data.frame(summary(full_model_prod)[[4]])[-1, "Std..Error"]
    coefficients_plot_data$comp_coef = full_model_comp$coefficients[-1] *
      ((2*(full_model_prod$coefficients[-1] < 0)) - 1) #this term flips coefficients that were positive for production
    for (fnum in 21:24){ 
      coefficients_plot_data$comp_coef[fnum] = full_model_comp$coefficients[fnum+1] #do not flip baseline features
      coefficients_plot_data$coef[fnum] = full_model_prod$coefficients[fnum+1] #do not flip baseline features
    }
    coefficients_plot_data$comp_se = data.frame(summary(full_model_comp)[[4]])[-1, "Std..Error"]
      
    coefficients_plot_data$names = c("SPC1", "SPC2", "SPC3", "SPC4", "SPC5",
                                     "SPC6", "SPC7", "SPC8", "SPC9", "SPC10",
                                     "TPC1", "TPC2", "TPC3", "TPC4", "TPC5", 
                                     "TPC6", "TPC7", "TPC8", "TPC9", "TPC10",
                                     "Frequency", "Solo frequency", "MLU", "Final frequency")
    coefficients_plot_data$names = factor(coefficients_plot_data$names)
    coefficients_plot_data$names = factor(coefficients_plot_data$names,
        levels(coefficients_plot_data$names)[c(5,7:14,6,15,17:24,16,2,1,3,4)])
    coefficients_plot_data$group = c(rep(2,10), rep(3,10), rep(1,4))
    
    #plot production coefficients
    ggplot(coefficients_plot_data,
           aes(x = names, y = coef, fill = group)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = coef-se, ymax = coef+se), position = position_dodge()) +
        theme_bw()+ 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
              text = element_text(size=14)) +
        scale_fill_gradient(low = "#555555", high = "#BBBBBB") +
        ylab("Feature weight (production)") +
        xlab("Feature") +
        guides(fill=FALSE)+
        coord_cartesian(ylim = c(-2, .7)) +
        scale_y_reverse()
    
    #plot comprehension coefficients
    ggplot(coefficients_plot_data,
           aes(x = names, y = comp_coef, fill = group)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_errorbar(aes(ymin = comp_coef-comp_se, ymax = comp_coef+comp_se), position = position_dodge()) +
      theme_bw()+ 
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
            text = element_text(size=14)) +
      scale_fill_gradient(low = "#555555", high = "#BBBBBB") +
      ylab("Feature weight (comprehension)") +
      xlab("Feature") +
      guides(fill=FALSE) +
      coord_cartesian(ylim = c(-2, .85))+
      scale_y_reverse(
        #breaks = c(.5, 0, -.5, -1, -1.5, -2),
        #labels = c('0.5','0.0','-0.5', '-1.0', '-1.5', '-2.0')
      )
}

###Look at residuals
res = full_model_prod$residuals
pc_aoa_data[res > 1, "word_id"]
pc_aoa_data[res < -1, "word_id"]
