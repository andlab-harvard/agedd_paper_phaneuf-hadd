# agedd_paper_phaneuf-hadd

Data and Code

*to accompany*

Phaneuf-Hadd, C.V., & Somerville, L.H. (under review). Time-Based Cognition and Working Memory Explain Changes in Delay Discounting Through Adolescence. *Journal of Cognition and Development*. doi: PENDING.

## Developer Contact Information

Github profile: https://github.com/cphaneuf

Email: (current) cphaneuf@g.harvard.edu, (permanent) cphaneuf@umich.edu

## Contents

### data/ directory

Contains demographic, task, and questionnaire data for study conducted within the Affective Neuroscience & Development Lab @ Harvard University.

### analyses/ directory

*utilities.R* defines variables to be shared across scripts.

*demographics.R* takes demographic data inputs from data/ and writes outputs to results/demographics/.

*pre_analyses.R* takes demographic and questionnaire data inputs from data/ and writes outputs (from linear mixed-effects models and regression models) to results/pre_analyses/.

*analyses.R* takes demographic, task, and questionnaire data inputs from data/ and writes outputs (from linear mixed-effects models, regression models, and directed acyclic graphs) to results/analyses/.

### results/ directory

Contains text and png file outputs from scripts in analyses/, sorted by analysis type.

### figures/ directory

Contains annotated_figs.pptx, which annotates several figures beyond the limits of R.
