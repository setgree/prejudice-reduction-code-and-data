# Improve reproducibility: guard unnamed CSV columns, stabilize readr guessing, optionalize beepr, document optional deps

This PR applies several small reproducibility and safety improvements: 

1) **code/1-clean-data.R**: use `readr::read_csv(..., guess_max = 10000)` to stabilize column type guessing and safely drop unnamed CSV export columns if present using `select(-starts_with('...'))`;  
2) **code/3-sub-group.R**: treat `beepr` as an optional dependency (remove `library(beepr)`) to avoid failing on systems without it;  
3) **code/functions/main-meta-function.R**: use safer cluster indexing (`.y[["unique_paper_id"]]`) in `meta_engine` and guard `beepr::beep(...)` with `requireNamespace()` to avoid errors when `beepr` is not installed;  
4) **README.md**: document `beepr` as an optional package and its purpose.

This PR is intended to improve long-term reproducibility without changing analysis results. Please review and merge to main when ready.