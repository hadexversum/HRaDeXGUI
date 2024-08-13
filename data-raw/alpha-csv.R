## code to prepare `alpha.csv` dataset goes here

alpha_dat <- HaDeX::read_hdx("C:/Users/User/Desktop/article_hradex/ALPHA G i BG.csv")

alpha_dat <- HRaDeX::move_dataset(alpha_dat, -25)

usethis::use_data(alpha_dat, overwrite = TRUE)
