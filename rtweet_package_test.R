## install rtweet from CRAN
install.packages("rtweet")

## load rtweet package
library(rtweet)

## install devtools package if it's not already
if (!requireNamespace("devtools", quietly = TRUE)) {
        install.packages("devtools")
}

## install dev version of rtweet from github
devtools::install_github("mkearney/rtweet")

## load rtweet package
library(rtweet)
library(ROAuth)
library(httr)

api_key <- "xOnMCl2XZQf96M3S0E8ebGiEV"
api_secret <- "uSKcaXSklvu69wwLdymo0nZhgaKk6cHpiNTSGzas9Yg6hEq9NZ"
access_token <- "67433123-AZezkqDJswd011NJHjNTTDOwvCWyVfqmgFxd6MqsW"
access_token_secret <- "cZihVqQ2eKW5lnyS6IMaxh0gaQsLI54u1Euly82GUEGtw"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

unlink(“.httr-oauth”)

## search for 5000 tweets using the rstats hashtag
rt <- search_tweets(
        "#rstats", n = 18000, include_rts = FALSE
)
