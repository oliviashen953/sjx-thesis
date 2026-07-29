# Unsupervised Model Aggregation Methods to Integrate Pre-trained Polygenic Risk Prediction Models

**Jiaxin (Olivia) Shen** · 60-credit Master's thesis, Biostatistics, Harvard T.H. Chan
School of Public Health · Advisor: **Dr. Rui Duan** · Defended 8 May 2024
([defense announcement](https://hsph.harvard.edu/biostatistics/events/thesis-defense-jiaxin-shen/))

Analysis code for the thesis. This is the research code as it was run in 2024 on
Harvard's FASRC cluster — R, MATLAB and SLURM scripts, not a packaged library.

---

## The question

A polygenic risk score (PRS) turns a person's genotype into one number: their
genetic predisposition for a trait. The trouble is that there is never *one*
score. The [PGS Catalog](https://www.pgscatalog.org/) publishes dozens to
hundreds of pre-trained models per trait, each built from different GWAS, on
different SNPs, in different populations — and they disagree about who is at
risk.

The usual way to choose between them is supervised: measure the phenotype in
your target population, then fit or select the model that predicts it best. That
requires labels, and labels are exactly what you often do not have — or have in
a form that misleads. Someone recorded as disease-free in a breast cancer study
may simply not have developed it *yet*, and a model chosen on one ancestry group
or age band may not transfer to another.

**So: can you combine pre-trained PRS models with no phenotype labels at all,
and still identify the high-risk group as well as a supervised method would?**

## The approach: rank aggregation

Each PRS model ranks every individual. Instead of picking one ranking or
regressing on labels, combine all the rankings into a consensus one. Two
unsupervised methods are implemented and compared:

- **MC4** — Markov-chain rank aggregation. Build a pairwise preference matrix
  from all models' rankings (does more than half of the models rank *j* above
  *i*?), normalise it into a transition matrix, make it ergodic with a damping
  factor α = 0.15, and take the stationary distribution as the consensus rank.
- **NNM** — nuclear norm minimisation. Treat the rating matrix as a noisy,
  incomplete matrix and recover a low-rank approximation by singular value
  projection, then rank from the completed matrix. Four ways of forming the
  pairwise matrix are compared: `logs_rank` (log odds ratio, the default here),
  `ams_rank`, `bcs_rank`, `sbs_rank`.

Baselines, unsupervised: **AVG_PRS** (simple average of all PRSs) and
**AVG_RA_Rank** (average of all aggregated rankings, including MCT).
Baselines, supervised: **Supervised_OLS** (OLS on all PRSs as covariates) and
**Supervised_Best** (pick the single PRS with the highest R²), both trained on a
held-out labelled set of 3,000.

## Data

Individual-level genotype and EHR phenotype data from the **eMERGE Network**,
scored with every applicable model in the PGS Catalog:

| Trait | PGS models (available → scored) | Sample size | Mean age | Ancestry |
|---|---|---:|---:|---|
| Body height | 87 → **72** | 36,461 | 63.0 | EUR 88.8%, AFR 9.9% |
| BMI | 100 → **52** | 60,043 | 63.0 | EUR 88.8%, AFR 9.9% |
| Breast cancer | 147 → **108** | 36,899 | 68.5 | EUR 78.0%, AFR 15.3% |
| Type-2 diabetes | 133 → **82** | 43,413 | 67.2 | EUR 99.6%, no AFR |

Models were dropped when none of their SNPs appeared in the eMERGE genotypes.

**No data is included in this repository, and none can be.** eMERGE is
controlled-access; the PGS Catalog models are public but the scored individual
data is not. What is here is the code that produced the results, plus aggregate
summary tables (`NORMAL_summary_result/*.csv`: one row per ranking method ×
percentile) and figures.

## How performance is measured

Every method produces a ranking; the question is whether the people it puts at
the top really are the extreme ones. Evaluation is on 20 randomly drawn test
subsamples of 3,000, disjoint from the supervised training set:

- **Continuous traits** (height, BMI): *standardised difference* between the top
  X% and the remaining (100−X)%, for X = 5, 10, 20.
- **Binary traits** (breast cancer, T2D): *odds ratio* and relative risk of the
  actual diagnosis in the top X% versus the rest.

## Results

**With no data shift** — supervised training drawn i.i.d. from the same
population as the test sets — the unsupervised methods are already competitive.
For body height (72 models), the top-5% standardised difference is **13.35 for
MC4** and **12.79 for NNM**, on par with supervised OLS and with the supervised
best-PRS. For breast cancer, NNM leads the unsupervised methods, and even the
plain average of PRSs (mean OR **1.745**) beats supervised OLS (**1.437**).
Supervised methods retain an edge for T2D.

**With data shift — which is the point of the thesis.** Two biased training
regimes were constructed: train the supervised methods on African-ancestry
individuals and test on European-ancestry ones, and train on individuals aged
65+ and test on a younger population. The rank aggregation methods have no
labels to be biased by, and stay flat (height: mean standardised difference
holds in the 12.5–22 band). The supervised ones fall:

| Breast cancer, supervised best-PRS | Mean OR, top 5% |
|---|---:|
| Trained i.i.d. with the test set | **2.12** |
| Trained on African-ancestry individuals only | **1.38** |
| Trained on individuals aged 65+ | **0.90** |

An odds ratio of 0.90 means the "best" model selected under age-biased
supervision put the *lower*-risk group at the top. Under the same shifts, MC4
becomes the overall best method for BMI.

**Conclusion.** Unsupervised rank aggregation performs comparably to supervised
model selection when the training labels are representative, and better when
they are not. Since the labels are the fragile part — they can be biased,
incomplete, or simply premature for a disease that develops late — being able to
skip them is a real advantage, not just a convenience.

## Repository layout

| Path | What it is |
|---|---|
| `NEWEST_Fastest_Pipeline/` | The pipeline actually used for the final results. `MC4MCT_ALL.ipynb` runs MC4/MCT; `NNM_call*.m` + `.sh` run NNM (SVP) as SLURM jobs, one variant per file (`_AFR`, `_AGE65`, `_TIE`); `update_summary*.R` builds the summary tables and figures for each regime. |
| `NORMAL_R_CODES/` | Step-by-step R pipeline for the no-shift ("NORMAL") setting: split supervised/unsupervised sets, compute per-individual scores and ranks, attach the MC4/NNM columns, then the standardised-difference analysis. |
| `NORMAL_NNM/` | Per-trait MATLAB entry points for NNM (`NORMAL_height.m`, `NORMAL_bmi.m`, `NORMAL_brc.m`). |
| `NORMAL_MC4MCT/` | Notebooks running MC4 and MCT for the no-shift setting. |
| `NORMAL_summary_result/` | Aggregate results: standardised difference (height, BMI) and odds ratio / relative risk (breast cancer) per ranking method and percentile. |
| `NORMAL_initial_graph/` | Top-percentile comparison figures. |
| `OLD_CODES/` | Earlier versions of the same analysis, kept as a record. Superseded by the two directories above. |

Naming conventions across the tree: `NORMAL` = no data shift · `RACE_AFR` and
`AGE65` = the two biased-training experiments · `TIE` = the tie-handling variant
of the conversion step · `SVD` = the singular-value-decomposition variant.

## Notes for anyone reading the code

- Paths are hard-coded to the Harvard FASRC cluster (`/n/holyscratch01/...`),
  and the `.sh` files are SLURM submission scripts. Nothing will run unmodified
  elsewhere, and nothing will run at all without eMERGE access.
- MATLAB R2022b for the NNM solver; R for the pipeline and figures; Python
  notebooks for MC4/MCT.
- `NNM_call_AGE65.out` is a captured solver log (iteration, residual, RMSE) —
  useful only for seeing convergence behaviour.

## Credit

None of the aggregation methods are new here. What this repository contributes
is applying them to polygenic risk scores and testing what happens under
biased supervision.

- **NNM** — Gleich, D. F. & Lim, L.-H. (2011). *Rank Aggregation via Nuclear
  Norm Minimization.* KDD '11, 60–68.
  [arXiv:1102.4821](https://arxiv.org/abs/1102.4821).
  The MATLAB routine called throughout this repository, `ssmcr(A, 'skewtype',
  ...)`, is the implementation accompanying that paper. Its `skewtype` settings
  `lo`, `am`, `bc` and `sb` are the `logs` / `ams` / `bcs` / `sbs` variants
  compared in this work.
- **Singular value projection**, the matrix-completion solver underneath NNM —
  Jain, P., Meka, R. & Dhillon, I. (2010). *Guaranteed Rank Minimization via
  Singular Value Projection.* NeurIPS.
- **MC4** — Dwork, C., Kumar, R., Naor, M. & Sivakumar, D. (2001). *Rank
  Aggregation Methods for the Web.* WWW '01.
- **MCT** — DeConde, R. P. et al. (2006). *Combining Results of Microarray
  Experiments: A Rank Aggregation Approach.* Statistical Applications in
  Genetics and Molecular Biology.
- **Pre-trained models** — the [PGS Catalog](https://www.pgscatalog.org/).

The full reference list is in the thesis.

## Acknowledgements

My thanks to **Dr. Rui Duan**, who advised this thesis — for the question, for
the guidance through every stage of it, and for the feedback that shaped what is
here.
