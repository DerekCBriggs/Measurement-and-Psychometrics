# Session Log: Item Parameter Estimation MMLE R Markdown

**Date:** 2026-02-14
**Project:** Measurement and Psychometrics / Parameter Estimation
**Goal:** Create an R Markdown document to improve teaching of MMLE/EM for IRT item parameter estimation

---

## Background

The user is a professor teaching IRT (EDUC 8720) who wanted help improving slides 13-31 of `5_Item_Parameter_Estimation.pptx`, which cover marginal maximum likelihood estimation. The expert source is Chapter 6 of Baker & Kim (2004). Students have varied math backgrounds (some know advanced calculus, some do not).

## Step 1: Reading Source Materials

- Extracted Chapter 6 (pages 157-175) and Appendix D (pages 333-343) from `Item Response Theory_26_02_14_14_32_47.pdf` using PyMuPDF
- Extracted text from all 35 slides of `5_Item_Parameter_Estimation.pptx` using python-pptx
- Reviewed `Person_Parameter_Estimation.Rmd` for style conventions (html_document, flatly theme, toc_float, code_folding, ggplot2)

## Step 2: Analysis of Current Slides

**Strengths identified:**
- Slide 13's conceptual roadmap gives a useful bird's-eye view
- The "Recall Thor" example (slide 24) bridges from person estimation material
- Comparing MMLE to JML estimation equations (slide 22) builds on prior knowledge
- The Gaussian quadrature visual (slide 19) is a smart pedagogical choice

**Weaknesses identified:**
- Many slides are equation-heavy with little annotation (slides 15, 17, 18, 29, 30)
- The narrative arc is fragmented — no single worked example ties concepts together
- The "artificial data" is described but never computed with actual numbers
- Quadrature is introduced visually but never made concrete with real nodes/weights
- No connection to R software (unlike the Person Parameter Estimation Rmd)

## Step 3: Four Improvement Ideas Proposed

1. **"The MMLE Story in Five Acts"** — Full narrative walkthrough with LSAT-6 data
2. **"Quadrature and Posterior Made Tangible"** — Bridge from Thor example to marginal likelihood
3. **"The EM Algorithm as a Kitchen Recipe"** — Demystify EM with step-by-step numbers
4. **"Why Not Just Use JML?"** — Simulation comparison showing MMLE advantages

**User chose:** Combination of Ideas 1-3

## Step 4: R Markdown Creation

Created `Item_Parameter_Estimation_MMLE.Rmd` with the following structure:

1. **Introduction** — Why MMLE, what the EM algorithm does
2. **From Thor to Marginal Likelihood** — Bridges from person estimation using the Thor example; shows the denominator of Bayes' theorem IS the marginal likelihood
3. **Quadrature: Making Integration Concrete** — Contrasts trapezoidal rule (801 points) vs. Gaussian quadrature (10 points); shows actual BILOG nodes/weights; verifies both give same answer for Thor
4. **The LSAT-6 Data** — Classic 5-item, 1000-examinee dataset from Baker & Kim
5. **The EM Algorithm, Step by Step**
   - E-Step 1: Likelihood at each node for each pattern
   - E-Step 2: Marginal probabilities
   - E-Step 3: Posterior probabilities (with full calculation table)
   - E-Step 4: Artificial data (n-bar and r-bar)
   - M-Step: Newton-Raphson item-by-item estimation
6. **Full EM Loop** — Runs 100 cycles with convergence plots
7. **Comparison with `mirt()`** — Side-by-side table and scatter plots
8. **Applied to CDE Data** — Real dataset from person estimation module
9. **Summary** — Big picture recap with practical notes table

## Step 5: Iterative Edits

### Edit 1: Expanded E-Step 3 Table
**Request:** Add columns for likelihood, weight, and their product so students can trace how the posterior is computed.
**Change:** The kable table for pattern 22 now shows: Node, X_k, L_l(X_k), A(X_k), L×A, P_l, and Posterior. Added verification that posterior sums to 1.

### Edit 2: Consistent Response Patterns in E-Step 2
**Request:** Show marginal probabilities for the 10 most common response patterns (matching the earlier table) instead of the first 10 patterns.
**Change:** Updated `e-step-2` chunk to sort by frequency and display the same top-10 patterns shown earlier, with frequency column included.

### Edit 3: Clarify Trapezoidal Rule vs. Quadrature
**Request:** Explain the difference between the two numerical integration approaches.
**Changes:**
- Added explanation of trapezoidal rule in "The Denominator You May Have Ignored" section (801 points, brute force, computationally expensive)
- Added transition text explaining why MMLE needs something more efficient
- Expanded quadrature section with explicit contrast to trapezoidal rule, including a comparison table
- Updated test section labels to say "Trapezoidal rule (801 points)" vs. "Quadrature (10 points)"

### Edit 4: Remove Bioassay References
**Request:** Replace all bioassay references with JMLE framing, since students haven't read Baker & Kim.
**Changes:** Replaced 5 occurrences of "bioassay" throughout the document with references to JMLE estimation equations and observed frequencies from JML.

### Edit 5: Explain Why r-bar Differs Across Items
**Request:** Anticipate student confusion about why items get different r-bar values despite identical starting parameters.
**Change:** Added bolded callout after the artificial data table explaining that n-bar is the same for all items but r-bar depends on u_li (actual responses). Added code chunk showing raw proportion correct for each item.

### Edit 6: Simplify M-Step Equations
**Request:** Remove reference to c parameter and W_ik weighting term, which were never introduced.
**Change:** Replaced 3PL-general equations with clean 2PL equations. Added plain-English explanation of what each equation does and why there are two (one for intercept, one for slope).

### Edit 7: Add Baker & Kim Reference
**Request:** Add citation in intro and full reference at end.
**Changes:** Added "It builds from the more technical presentation of MMLE found in Chapter 6 of Baker and Kim (2004)" in the introduction. Added full APA reference at the end of the document.

## Files Created/Modified

- **Created:** `Item_Parameter_Estimation_MMLE.Rmd` — Main R Markdown document (~910 lines)
- **Created:** `Item_Parameter_Estimation_MMLE.html` — Rendered HTML output
- **Created:** `session_log.md` — This file

## Technical Notes

- Pandoc is not on PATH; must set `RSTUDIO_PANDOC` environment variable to `/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64` when knitting from command line
- PyMuPDF (`fitz`) used for PDF text extraction; `python-pptx` for PowerPoint extraction
- PDF page indexing: printed page numbers are offset from PDF indices by 28 (e.g., printed page 157 = PDF index 185)
- `packageVersion()` returns a list; must wrap in `as.character()` for `cat()`
