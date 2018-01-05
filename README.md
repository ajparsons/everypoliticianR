everypoliticianR
================




About the package
-----------------

This package provides a basic interface to access [everypolitician.org](http://everypolitician.org/) datasets through R.
As well as various functions to assist with data reconcilliation. 

More complete bindings exist for [Python](https://github.com/everypolitician/everypolitician-popolo-python) and [Ruby](https://github.com/everypolitician/everypolitician-popolo). 

How to install
--------------

The package is currently only available on github.

``` eval
library(devtools)
install_github("ajparsons/everypoliticianR")
library(twfyR)
```

How to use
--------------

Given a country name (and chamber if there are multiple chambers covered for that country in EveryPolitician) 
this package will download the most recent json file for that chamber and initiallise it using jsonlite. 

The structure of the file is explained [here](http://docs.everypolitician.org/data_structure.html),
but it is useful to download and view one of the json files in a text editor to get a sense of
the data avalaible for that specific country.

### Downloading a country's information

``` r
library(devtools)
install_github("ajparsons/everypoliticianR")
library(everypoliticianR)

house_of_commons = everypolitician("United Kingdom")
us_senate = everypolitician("United States of America","Senate")
```

### Using everypolitician to connect two different id formats

EveryPolitician can be used as a rosetta stone for different id schemas. 

The 'ep_id_lookup' command can be used to create a dataframe to map between EP IDs and the other schema.

For instance, ep_id_lookup(pop,'wikidata') will produce a frame with an 'ep_id' and 'wikidata' ID.

For this example, EveryPolitician data will be joined with the TheyWorkForYou data
to produce seperate wordcloud of words used in terrorism debates by male and female MPs.
(based on example of twfy R bindings [here](https://github.com/jblumenau/twfyR)).

``` r
library(devtools)
install_github("jblumenau/twfyR")
library(twfyR)

install_github("ajparsons/everypoliticianR")
library(everypoliticianR)

library(quanteda)

#get uk politician info
pop = everypolitician("United Kingdom")

#extract parlparse ids
parlparse = ep_id_lookup(pop,"parlparse")

#convert to twfy format
#e.g. uk.org.publicwhip/person/25622 to - 25622
tidy_id <- function (x){ strsplit(x["parlparse"],"/")[[1]][[3]]}
parlparse$twfy <- apply(parlparse,1,tidy_id)

#get recent commons speeches on terrorism
my_key = "YOUR_KEY_HERE"
set_twfy_key(my_key)
debates = getDebates(type="commons", search="terrorism")

#merge debates with the lookup table, then with the ep persons table
debates = merge(debates,parlparse,by.x="person_id",by.y="twfy")
debates = merge(debates,pop$persons,by.x="ep_id",by.y="id")

male_speeches = subset(debates,gender == "male")
female_speeches = subset(debates,gender == "female")


wordcloud <- function(x){
  speech_corpus <- corpus(x$body)
  
  speech_dfm <- dfm(speech_corpus, remove = c("will", stopwords("english")),
                    remove_punct  = TRUE)
  
  speech_dfm <- dfm_trim(speech_dfm, max_count = 400, min_count = 10)
  
  speech_idf <- tfidf(speech_dfm)
  
  textplot_wordcloud(speech_idf, min.req = 10, random.order = FALSE, rot.per = .25, 
                     colors = RColorBrewer::brewer.pal(8,"Dark2"))
}

#make workcloud of respectve corpus
#not particlarly enlightening - but it'll do for an example
wordcloud(male_speeches)
wordcloud(female_speeches)
```

### Get alternate names

Some datasets only contain names for politicians. EP data will often contain alternate versions of these names (which can be downloaded as a csv from the country page). 

This command will produce the equivliant data frame - ep_alt_name_list.

``` R
house_of_commons = everypolitician("United Kingdom")
name_lookup_frame = ep_alt_name_list(house_of_commons)
```

### Comparing gender ratio of two countries with a t-test

``` r

library(devtools)
install_github("ajparsons/everypoliticianR")
library(everypoliticianR)
library(car)

#function to compare the proportion of female representatives in two chambers
compare_gender_ratio <- function(a_name,b_name,a_chamber_name="",b_chamber_name="") {
  
  get_current_people <- function(name,chamber_name="") {
    #load popolo for this chamber
    pop = everypolitician(name,chamber_name)
    
    #get the latest legislative period
    current_period = tail(pop$periods$legislative_period_id,n=1)
    
    #get all memberships from this legislative period
    memberships = subset(pop$memberships, legislative_period_id == current_period)
    
    #merge to restrict to just these memberships
    people = merge(pop$persons,memberships,by.x ="id", by.y="person_id")
    
    #recode gender as an integer so the average is meaningful. 
    people$gender <- recode(people$gender, "'male'=0; 'female'=1")
    
    #introduce the name of the country for comparison when merged 
    people$name = name
    
    #different countries have slightly different structures, retain only those that we're interested in
    f = subset(people, select=c(id,name,gender))
  }
  
  #get a data frame for each country
  country_a_people = get_current_people(a_name,a_chamber_name)
  country_b_people = get_current_people(b_name,b_chamber_name)
  
  #merge
  combo = rbind(country_a_people, country_b_people)
  
  #check for a mean difference
  t.test(combo$gender~combo$name)
}

a_name = "Germany"
a_chamber_name =""
b_name = "United States of America"
b_chamber_name = "Senate"
compare_gender_ratio(a_name,b_name,a_chamber_name,b_chamber_name)
```

### Analysing differences in age when elected

EveryPolitician data contains info on when someone was elected.
By joining the membership information for a specific term with the people table, you can get a value for age when they were elected.

The approx_date function corrects for when the date is incomplete, and has only the year (1971) or month (1971-05). In these cases those dates would be expanded to '1971-01-01' and '1971-05-01'.

This example looks to see if there is a significant difference in age between Labour and Conservative MPs in the 2017 Parliament:

```R

library(devtools)
install_github("ajparsons/everypoliticianR")
library(everypoliticianR)

pop = everypolitician("United Kingdom")

age_at_election <- function(x) {
  #return time in years between being born and being elected in this session
  start = approx_date(x$start_date)
  birth = approx_date(x$birth_date)
  if (is.na(start) || is.na(birth)){
    return(NA)
  }
  r =as.numeric(difftime(start,birth, unit="weeks"))/52.25
}

#get memberships for labour and conservative MPs in term/57 (2017-)
landc_memberships = subset(pop$memberships, on_behalf_of_id %in% c("labour","conservative") & legislative_period_id == "term/57")

#merge with the persons table to limit to just these members and get birth_date and start_date in same table
people = merge(pop$persons,landc_memberships,by.x ="id", by.y="person_id")

#create age variable by finding difference between birthdate and election
people$age = apply(people,1,age_at_election)

# drop any without a valid age (missing birthdate)
people = people[!is.na(people$age),] 

#t-test to see if signififcant difference in age
t.test(people$age~people$on_behalf_of_id)
```
