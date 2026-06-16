# Qualification Report for {gsm.qtl} R Package

## Introduction

Risk-Based Quality Monitoring (RBQM) is a proactive approach to clinical
trial monitoring that focuses on identifying and addressing the most
critical risks to the integrity of study data and patient safety. This
approach aims to ensure that study data are accurate, reliable, and
credible while optimizing the use of resources and minimizing the burden
on study sites.

The `{gsm}` suite of R packages supports RBQM by performing risk
assessments primarily focused on detecting differences in quality at the
site level. This approach is intended to detect potential issues related
to critical data or process(es) across the major risk categories of
safety, efficacy, disposition, treatment, and general quality. Each
category consists of one or more risk assessment(s). Each risk
assessment analyzes the data to flag sites with potential outliers and
provides a visualization to help the user understand the issue.

## Scope

Qualification testing ensures that core functions execute as expected on
a system-wide scale. Qualification includes executing various
functional, performance, and usability testing. Qualification tests are
designed to provide developers with a repeatable process that is easy to
update and document. This document summarizes the qualification testing
performed on `gsm` functions essential to the analysis workflow.

## Process Overview

Each essential `gsm` workflow function is independently qualified using
specifications and test cases compiled in this report. Details are
provided below.

### Specifications

Specifications capture the most critical use cases for a given function.
Each function must have at least one (1) specification, and each
specification must have at least one (1) associated test case. Multiple
specifications may exist for a function, and multiple test cases may
exist for a specification.

Each specification includes the following components:

- **Description:** outlines the use case for the specification

- **Risk Assessment**

- **Risk Level:** assigned a value of “Low”, “Medium”, or “High”,
  corresponding to the risk associated with the specification failing

- **Risk Impact:** assigned a value of “Low”, “Medium”, or “High”,
  corresponding to the severity of the impact associated with the
  specification failing

- **Test Cases:** lists measurable test cases associated with the
  specification

| Spec ID | Spec Description | Risk | Impact | Associated Test IDs |
|----|----|----|----|----|
| S1_1 | Given a thresold and multiplier, function can appropriately calculate an upper funnel metric for comparison | High | High | T1_1 |
| S2_1 | Given appropriate raw participant-level data, an Ineligibility Assessment can be done using methods cited in gsm.qtl | High | High | T2_1 |
| S3_1 | Given appropriate raw participant-level data, an Early Discontinuation Assessment can be done using methods cited in gsm.qtl | High | High | T3_1 |

### One Row Per Test

| Function | Spec ID | Test ID | Test Description | Test Result |
|----|----|----|----|----|
| One Side Prop unit test | S1_1 | T1_1 | Analyze One Side Prop works for Study, when nProRate is clearly violated (#5, \#11, \#39, \#42, \#50, \#73) | Pass |
| Ineligibility Assessment | S2_1 | T2_1 | Given appropriate inclusion/exclusion related data, calculates appropriate QTL threshold. (#2, \#5, \#11, \#17, \#39, \#42, \#50, \#73) | Pass |
| Early Discontinuation Assessment | S3_1 | T3_1 | Given appropriate study discontinuation related data, calculates appropriate QTL threshold. (#3, \#21, \#39, \#42, \#50, \#73) | Pass |

## Qualification Testing Environment

### Session Information

**R version 4.6.0 (2026-04-24)**

**Platform:** x86_64-pc-linux-gnu

**locale:** *LC_CTYPE=C.UTF-8*, *LC_NUMERIC=C*, *LC_TIME=C.UTF-8*,
*LC_COLLATE=C.UTF-8*, *LC_MONETARY=C.UTF-8*, *LC_MESSAGES=C.UTF-8*,
*LC_PAPER=C.UTF-8*, *LC_NAME=C*, *LC_ADDRESS=C*, *LC_TELEPHONE=C*,
*LC_MEASUREMENT=C.UTF-8* and *LC_IDENTIFICATION=C*

**attached base packages:** *stats*, *graphics*, *grDevices*, *utils*,
*datasets*, *methods* and *base*

**other attached packages:** *gsm.qtl(v.1.2.2)*, *testthat(v.3.3.2)*,
*riskmetric(v.0.2.7)*, *stringr(v.1.6.0)*, *gh(v.1.6.0.9000)*,
*pander(v.0.6.6)*, *purrr(v.1.2.2)*, *dplyr(v.1.2.1)*, *knitr(v.1.51)*,
*gt(v.1.3.0)* and *gsm.core(v.1.2.1)*

**loaded via a namespace (and not attached):** *gtable(v.0.3.6)*,
*xfun(v.0.58)*, *bslib(v.0.11.0)*, *ggplot2(v.4.0.3)*,
*htmlwidgets(v.1.6.4)*, *devtools(v.2.5.2)*, *vctrs(v.0.7.3)*,
*tools(v.4.6.0)*, *generics(v.0.1.4)*, *curl(v.7.1.0)*,
*tibble(v.3.3.1)*, *pkgconfig(v.2.0.3)*, *data.table(v.1.18.4)*,
*RColorBrewer(v.1.1-3)*, *S7(v.0.2.2)*, *desc(v.1.4.3)*,
*lifecycle(v.1.0.5)*, *farver(v.2.1.2)*, *compiler(v.4.6.0)*,
*brio(v.1.1.5)*, *textshaping(v.1.0.5)*, *htmltools(v.0.5.9)*,
*usethis(v.3.2.1)*, *sass(v.0.4.10)*, *lazyeval(v.0.2.3)*,
*yaml(v.2.3.12)*, *plotly(v.4.12.0)*, *pillar(v.1.11.1)*,
*pkgdown(v.2.2.0)*, *jquerylib(v.0.1.4)*, *tidyr(v.1.3.2)*,
*ellipsis(v.0.3.3)*, *cranlogs(v.2.1.1)*, *cachem(v.1.1.0)*,
*sessioninfo(v.1.2.4)*, *tidyselect(v.1.2.1)*, *digest(v.0.6.39)*,
*stringi(v.1.8.7)*, *duckdb(v.1.5.2)*, *forcats(v.1.0.1)*,
*rprojroot(v.2.1.1)*, *fastmap(v.1.2.0)*, *grid(v.4.6.0)*,
*here(v.1.0.2)*, *cli(v.3.6.6)*, *magrittr(v.2.0.5)*,
*triebeard(v.0.4.1)*, *pkgbuild(v.1.4.8)*, *withr(v.3.0.2)*,
*waldo(v.0.6.2)*, *scales(v.1.4.0)*, *backports(v.1.5.1)*,
*rmarkdown(v.2.31)*, *httr(v.1.4.8)*, *otel(v.0.2.0)*, *ragg(v.1.5.2)*,
*memoise(v.2.0.1)*, *evaluate(v.1.0.5)*, *log4r(v.0.4.4)*,
*covr(v.3.6.5)*, *rex(v.1.2.2)*, *viridisLite(v.0.4.3)*,
*rlang(v.1.2.0)*, *urltools(v.1.7.3.1)*, *Rcpp(v.1.1.1-1.1)*,
*DBI(v.1.3.0)*, *glue(v.1.8.1)*, *workr(v.1.0.0)*,
*BiocManager(v.1.30.27)*, *xml2(v.1.5.2)*, *pkgload(v.1.5.3)*,
*rstudioapi(v.0.19.0)*, *jsonlite(v.2.0.0)*, *R6(v.2.6.1)*,
*systemfonts(v.1.3.2)* and *fs(v.2.1.0)*

## Pull Request History

The GitHub Pull Request (PR) process begins with the creation of one or
more issues that clearly define the proposed additions or revisions to
the package. Each issue should be assigned to a product developer (PD)
who then creates a `fix` branch named according to the related issue(s),
and implements the necessary code updates. Once the work is complete,
the PD opens a Pull Request to merge the `fix` branch into the target
branch (typically the `dev` branch). They must assign the PR to
themselves, request one or more reviewers, and link the PR to the
associated issue(s). Before the fix branch can be merged, the PR must be
approved by the designated reviewers and pass all required GitHub
qualification checks. Once these conditions are met, the `fix` branch is
merged into the target branch. This process is fully documented in the
[Contributor
Guidelines](https://gilead-biostats.github.io/gsm.core/articles/ContributorGuidelines.html#development-process)

Below, the most recent 10 PRs into gsm.qtl are displayed. [See all Pull
Requests here.](https://github.com/gilead-biostats/gsm.qtl/pulls)

#### Pull Request 119: Fix 118: Standardize activity fields

Merging fix-118-rename_activity_fields into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/119>

|   Requester    |   Date Requested    |      Reviewers       | Review Status |
|:--------------:|:-------------------:|:--------------------:|:-------------:|
| jharmon-gilead | 2026-06-01 15:32:18 | zdz2101 lauramaxwell |   APPROVED    |

#### Pull Request 117: Fix 116: Add active fields to workflows

Merging fix-116-add_active_fields into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/117>

| Requester | Date Requested | Reviewers | Review Status |
|:--:|:--:|:--:|:--:|
| jharmon-gilead | 2026-05-27 14:44:32 | copilot-pull-request-reviewer\[bot\] zdz2101 | COMMENTED |

#### Pull Request 114: Main -\> dev

Merging main into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/114>

|  Requester   |   Date Requested    |   Reviewers    | Review Status |
|:------------:|:-------------------:|:--------------:|:-------------:|
| lauramaxwell | 2026-05-15 13:48:07 | jharmon-gilead |   APPROVED    |

#### Pull Request 113: gsm.qtl v1.2.2 release candidate

Merging gsm.qtl-v1.2.2-rc into main

<https://github.com/Gilead-BioStats/gsm.qtl/pull/113>

|  Requester   |   Date Requested    |   Reviewers    | Review Status |
|:------------:|:-------------------:|:--------------:|:-------------:|
| lauramaxwell | 2026-05-14 19:42:37 | jharmon-gilead |   APPROVED    |

#### Pull Request 112: gsm.qtl v1.2.1

Merging rc-v1.2.1 into main

<https://github.com/Gilead-BioStats/gsm.qtl/pull/112>

| Requester |   Date Requested    | Reviewers | Review Status |
|:---------:|:-------------------:|:---------:|:-------------:|
|  zdz2101  | 2026-05-06 20:48:27 | samussiah |   APPROVED    |

#### Pull Request 111: catch dev up to main

Merging main into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/111>

| Requester |   Date Requested    |  Reviewers   | Review Status |
|:---------:|:-------------------:|:------------:|:-------------:|
|  zdz2101  | 2026-05-06 20:41:03 | lauramaxwell |   APPROVED    |

#### Pull Request 109: Fix eligibility_listing crash on zero-row or all-NA input

Merging bugfix/108-eligibility-listing-zero-row into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/109>

| Requester | Date Requested | Reviewers | Review Status |
|:--:|:--:|:--:|:--:|
| samussiah | 2026-05-04 19:09:57 | zdz2101 copilot-pull-request-reviewer\[bot\] | APPROVED |

#### Pull Request 106: Update workflows

Merging gha-202604 into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/106>

| Requester  |   Date Requested    |  Reviewers   | Review Status |
|:----------:|:-------------------:|:------------:|:-------------:|
| jonthegeek | 2026-04-30 19:41:20 | lauramaxwell |   APPROVED    |

#### Pull Request 104: Main -\> dev

Merging main into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/104>

|  Requester   |   Date Requested    | Reviewers | Review Status |
|:------------:|:-------------------:|:---------:|:-------------:|
| lauramaxwell | 2026-04-20 15:51:28 |  zdz2101  |   APPROVED    |

#### Pull Request 102: Gsm.qtl rc v1.2.0

Merging gsm.qtl-rc-v1.2.0 into main

<https://github.com/Gilead-BioStats/gsm.qtl/pull/102>

| Requester | Date Requested | Reviewers | Review Status |
|:--:|:--:|:--:|:--:|
| zdz2101 | 2026-04-15 17:59:56 | copilot-pull-request-reviewer\[bot\] lauramaxwell zdz2101 samussiah | COMMENTED |
