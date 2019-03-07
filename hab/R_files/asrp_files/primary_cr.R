# Create list of primary creeks to be used when asrp scenarios call for restoration of primary creek only ----
primary_cr_list <- c("Decker-", "Bingham-", "Cedar-", "Sherman-", "Elk Cr-", "Crim Cr-", "Stillman-", "Big (Hump)-", "Stevens-", "Johns-", "Cloquallum-", "Porter-", "Scatter Cr-",
                     "Beaver Cr-", "Lincoln Cr-", "Elk Cr-", "Elk Cr-", "Dry Run-", "Black (Wynoochee)-", "(Wynoochee Resevoir)", "Wynoochee-", "Waddell Cr-", "Garrard Cr-",
                     "Bunker-", "Skookumchuck-", "Hanaford-", "Sterns Cr-", "Thrash Cr-", "Lake Cr-")

primary_cr <- c(lapply(primary_cr_list, function(z) {
  c(grep(z, flowline$Reach, value = TRUE))
}))