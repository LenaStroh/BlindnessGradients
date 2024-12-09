This repo contains code used in XXX by Koba et al.

**Main code and folders**

./scripts/analyses_part1.qmd

This script performs the region-wise comparisons of the gradient scores between the two groups and creates figure 1.

./scripts/analyses_part2.qmd

This script compares the minimum distances within EVC and creates figure 1.

./scripts/figure4.qmd

This script creates figure 4.

**session info**


R version 4.2.2 (2022-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8    LC_MONETARY=English_United Kingdom.utf8
[4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] emmeans_1.8.7       afex_1.3-0          lme4_1.1-33         Matrix_1.5-1        ggpubr_0.6.0        viridis_0.6.4       viridisLite_0.4.2  
 [8] ggseg_1.6.5         ggsegGlasser_1.0.01 extrafont_0.19      lubridate_1.9.3     forcats_1.0.0       dplyr_1.1.2         purrr_1.0.2        
[15] readr_2.1.5         tibble_3.2.1        ggplot2_3.5.1       tidyverse_2.0.0     broom_1.0.6         tidyr_1.3.1         stringr_1.5.1      
[22] data.table_1.16.2  

loaded via a namespace (and not attached):
 [1] splines_4.2.2       carData_3.0-5       Rttf2pt1_1.3.12     numDeriv_2016.8-1.1 pillar_1.9.0        backports_1.4.1     lattice_0.20-45    
 [8] glue_1.6.2          extrafontdb_1.0     ggsignif_0.6.4      minqa_1.2.5         colorspace_2.1-0    cowplot_1.1.1       plyr_1.8.9         
[15] pkgconfig_2.0.3     xtable_1.8-4        mvtnorm_1.2-2       scales_1.3.0        tzdb_0.4.0          timechange_0.3.0    proxy_0.4-27       
[22] generics_0.1.3      farver_2.1.2        car_3.1-2           withr_3.0.2         pbkrtest_0.5.2      cli_3.6.2           crayon_1.5.3       
[29] magrittr_2.0.3      estimability_1.4.1  evaluate_1.0.1      fansi_1.0.4         nlme_3.1-160        MASS_7.3-58.1       rstatix_0.7.2      
[36] class_7.3-20        tools_4.2.2         hms_1.1.3           lifecycle_1.0.4     munsell_0.5.1       compiler_4.2.2      e1071_1.7-16       
[43] rlang_1.1.1         classInt_0.4-10     units_0.8-5         grid_4.2.2          nloptr_2.0.3        ggridges_0.5.6      rstudioapi_0.14    
[50] labeling_0.4.3      boot_1.3-28         gtable_0.3.6        lmerTest_3.1-3      abind_1.4-8         DBI_1.2.3           reshape2_1.4.4     
[57] R6_2.5.1            gridExtra_2.3       knitr_1.49          utf8_1.2.3          KernSmooth_2.23-20  stringi_1.8.4       parallel_4.2.2     
[64] Rcpp_1.0.13-1       vctrs_0.6.3         sf_1.0-19           coda_0.19-4.1       tidyselect_1.2.1    xfun_0.49      