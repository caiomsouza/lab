


# https://github.com/abjur/prodTJSP



if (!require(devtools)) install.packages('devtools')
devtools::install_github('abjur/prodTJSP')

library("prodTJSP")

d_list <- prod_download_courts()

d_list <- prod_list()

head(d_list)

prod_download(d_list, path = 'data-raw/pdf')

# suppose your pdf files are in data-raw/pdf folder
arqs <- dir('data-raw/pdf', full.names = TRUE)
d_prod <- prod_parse_uni(arqs)
d_prod

