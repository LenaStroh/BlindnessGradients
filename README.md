# Visual experience shapes functional connectome gradients

Cemal Koba, Joan Falcó-Roget, Olivier Collignon, Katarzyna Rączy, Marina Bedny, Mengyu Tian, Marcin Szwed, Anna-Lena Stroh

Code and data for the analyses reported in the paper above: functional connectome gradient
differences between sighted and blind (congenitally and late blind) individuals, their
relationship to cortical structure (thickness, surface area, curvature, sulcal depth, volume),
and structure-function coupling across the cortical hierarchy.

## Repository Structure

```text
.
├── data_tidy/                    # Processed data used as direct input to the analyses
│   ├── gradients.RDS              # Region-wise gradient scores (main cohort: SC/CB)
│   ├── gradients_lb.RDS           # Gradient scores including the late-blind (LB) sample
│   ├── minimum_distances.RDS      # Minimum distances within early visual cortex
│   ├── morphometrics.rds          # Cortical thickness/area/curvature/depth/volume per region
│   ├── vertexwise_sfc.RDS         # Vertex-wise structure-function coupling data
│   └── vertexwise_loadings.csv    # PC loadings for the vertex-wise structural PCA
├── results/                      # Supplementary tables referenced in the manuscript
│   └── table_s01.csv ... table_s23.csv
└── scripts/
    ├── R/
    │   ├── plotting.R              # Shared ggplot2 helper functions used across all figures
    │   └── utils.R                 # Data loading/reshaping helper functions
    ├── analyses_part1.qmd          # Region-wise gradient comparisons (SC vs. CB) + community-based (Yeo network) profiles
    ├── analyses_part1_lb.qmd       # Same as part1, including the late-blind (LB) sensitivity analysis
    ├── analyses_part2.qmd          # Visual hierarchy analysis and minimum-distance comparisons within EVC
    ├── analyses_part3a.qmd         # Structural PCA (thickness/area/curvature/depth/volume) and ROI-level structure-function coupling
    ├── analyses_part3b.qmd         # Vertex-wise structure-function coupling analysis
    └── renv.lock                   # Pinned R package versions for reproducibility
```

## Quick Setup

The analyses were run in R using [renv](https://rstudio.github.io/renv/) to pin package
versions.

```r
# from within scripts/
install.packages("renv")
renv::restore()
```

This installs the exact package versions recorded in `scripts/renv.lock`.

## Analysis Flow

1. **`analyses_part1.qmd`** / **`analyses_part1_lb.qmd`** — region-wise and community-based
   (Yeo network) comparisons of gradient scores between groups
2. **`analyses_part2.qmd`** — visual hierarchy and minimum-distance analyses within early
   visual cortex
3. **`analyses_part3a.qmd`** — structural PCA across morphometric measures and its
   relationship to the functional gradients (ROI-level)
4. **`analyses_part3b.qmd`** — structure-function coupling at the vertex level

Each notebook sources `R/utils.R` and `R/plotting.R`, and reads its inputs from `../data_tidy/`
and writes supplementary tables to `../results/`.
