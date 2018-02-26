all_clusts_old_xpn <- readr::read_rds("./data/intermediate/outputs/ov.afc1_xpn_v1/all_clusts_ov.afc1_xpn.rds")
dat_old_xpn<- readr::read_rds("./data/intermediate/outputs/ov.afc1_xpn_v1/cdat_ov.afc1_xpn.rds")

xpn_v1_c1 <- data.frame(sampleID=rownames(dat_old_xpn),clusts1= all_clusts_old_xpn$kmodes)

all_clusts_old_eb <- readr::read_rds("./data/intermediate/outputs/ov.afc1_eb_v1/all_clusts_ov.afc1_eb.rds")
dat_old_eb<- readr::read_rds("./data/intermediate/outputs/ov.afc1_eb_v1/cdat_ov.afc1_eb.rds")

eb_v1_c1 <- data.frame(sampleID=rownames(dat_old_eb),clusts1= all_clusts_old_eb$kmodes)


all_clusts_new_xpn <- readr::read_rds("./data/intermediate/outputs/ov.afc1_xpn/all_clusts_ov.afc1_xpn.rds")
dat_new_xpn<- readr::read_rds("./data/intermediate/outputs/ov.afc1_xpn/cdat_ov.afc1_xpn.rds")

all_clusts_new_cbt <- readr::read_rds("./data/intermediate/outputs/ov.afc1_cbt/all_clusts_ov.afc1_cbt.rds")
dat_new_cbt<- readr::read_rds("./data/intermediate/outputs/ov.afc1_cbt/cdat_ov.afc1_cbt.rds")

dropChar <- function(x)
{
  x <- as.character(x)
  substr(x, 0, nchar(x) - 7)
}
xpn_v2_c1 <- data.frame(sampleID=rownames(dat_new_xpn),clusts2= all_clusts_new_xpn$kmodes) %>%
  mutate(sampleID = dropChar(sampleID))

cbt_v2_c1 <- data.frame(sampleID=rownames(dat_new_cbt),clusts2= all_clusts_new_cbt$kmodes) %>%
  mutate(sampleID = dropChar(sampleID))

mlabs_xpn <- dplyr::inner_join(xpn_v1_c1,xpn_v2_c1,"sampleID")
mlabs_cbt <- dplyr::inner_join(eb_v1_c1,cbt_v2_c1,"sampleID")

table(mlabs_xpn$clusts2,mlabs_xpn$clusts1)
table(mlabs_cbt$clusts2,mlabs_cbt$clusts1)
