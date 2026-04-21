# Survival Analysis Studio

This project packages a Shiny survival-analysis workflow as a static `shinylive` site for GitHub Pages. The published app runs through `webR` directly in the browser, so the analysis runs on the user's own machine without a hosted Shiny server.

The app opens with a complete worked example based on `survival::lung`, so reviewers can immediately see a Kaplan-Meier curve, a stratified Kaplan-Meier curve with risk table, a log-rank test, a multivariable Cox model, PH diagnostics, and downloadable outputs before uploading any file. Users can then switch to their own `.csv` or `.xlsx` data and remap the analysis fields from the sidebar.

## Features

- Overall Kaplan-Meier curve plus stratified and faceted Kaplan-Meier curves.
- Risk table / number at risk using `survminer::ggsurvplot(risk.table = TRUE)`.
- Median survival summary table with 95% confidence intervals.
- Log-rank test for the selected KM grouping variable.
- Multivariable Cox proportional hazards model with selectable predictors, categorical reference levels, `strata()` terms, and ties handling.
- Hazard-ratio forest plot via `survminer::ggforest()`.
- PH diagnostics from `cox.zph()`, including p-value table and Schoenfeld residual plot grid.
- Missing-data summary, event-value frequency table, and `time <= 0` filtering diagnostics.
- Downloadable filtered data, Excel workbook, plot images, a reproducible `.R` script, a Quarto `.qmd` report, and a Typst-rendered `.pdf` when the app is running locally with Quarto installed.

## First Load

The GitHub Pages `shinylive` build may take 10-20 seconds on a first visit while `webR` and its package assets download. Later visits are usually faster thanks to browser caching.

## Local Development

Install the required R packages, then run the app locally:

```r
shiny::runApp(".")
```

To export the static site locally:

```r
source("scripts/export_shinylive.R")
```

This creates a `site/` directory containing the GitHub Pages-ready static build.
The exported site is a single-app build served from the site root, and the build also includes a Cloudflare Pages `_redirects` rule that sends legacy `/app_*` preview paths back to `/`.

## PDF Report Rendering

The app can render the generated `.qmd` to PDF via Quarto's `typst` format when you run the app locally with Quarto installed. This uses Typst rather than LaTeX for faster PDF generation.

The exported `shinylive` site cannot render PDFs in the browser because Quarto is not available inside the static `webR` runtime. Users can still download the `.qmd` source report and render it locally.

## Deployment

This repository includes [.github/workflows/deploy-pages.yml](/d:/dev/bdsfinal/.github/workflows/deploy-pages.yml:1). Its GitHub Pages deployment path:

1. installs the app dependencies,
2. exports the app with `shinylive::export()`,
3. publishes the generated `site/` folder to the `gh-pages` branch,
4. uploads the same `site/` folder as a GitHub Pages artifact,
5. deploys it with `actions/deploy-pages`.

In the repository settings, set **Pages** to use **GitHub Actions** as the source.

## Example Datasets

The app includes three built-in datasets from the `survival` package:

- `lung`
- `veteran`
- `ovarian`

`lung` is loaded by default with a prefilled working variable mapping so the app renders complete output immediately.
