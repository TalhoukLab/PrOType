# build mapping
build_mapping <- function(train.set) {
  # label mapping
  labs <- seq_len(4)
  if (train.set == "ov.afc1_cbt") {
    data.frame(labs, labels = c("C1-MES", "C5-PRO", "C4-DIF", "C2-IMM"))
  } else if (train.set == "ov.afc1_xpn") {
    data.frame(labs, labels = c("C2-IMM", "C4-DIF", "C5-PRO", "C1-MES"))
  } else {
    stop("No valid training set specified")
  }
}
