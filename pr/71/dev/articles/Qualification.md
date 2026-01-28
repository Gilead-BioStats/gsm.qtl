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

| Spec ID | Spec Description                                                                                                             | Risk | Impact | Associated Test IDs |
|---------|------------------------------------------------------------------------------------------------------------------------------|------|--------|---------------------|
| S1_1    | Given a thresold and multiplier, function can appropriately calculate an upper funnel metric for comparison                  | High | High   | T1_1                |
| S2_1    | Given appropriate raw participant-level data, an Ineligibility Assessment can be done using methods cited in gsm.qtl         | High | High   | T2_1                |
| S3_1    | Given appropriate raw participant-level data, an Early Discontinuation Assessment can be done using methods cited in gsm.qtl | High | High   | T3_1                |

### One Row Per Test

| Function                         | Spec ID | Test ID | Test Description                                                                            | Test Result |
|----------------------------------|---------|---------|---------------------------------------------------------------------------------------------|-------------|
| One Side Prop unit test          | S1_1    | T1_1    | Analyze One Side Prop works for Study, when nProRate is clearly violated                    | Pass        |
| Ineligibility Assessment         | S2_1    | T2_1    | Given appropriate inclusion/exclusion related data, calculates appropriate QTL threshold.   | Pass        |
| Early Discontinuation Assessment | S3_1    | T3_1    | Given appropriate study discontinuation related data, calculates appropriate QTL threshold. | Pass        |

## Qualification Testing Environment

### Session Information

**R version 4.5.2 (2025-10-31)**

**Platform:** x86_64-pc-linux-gnu

**locale:** *LC_CTYPE=C.UTF-8*, *LC_NUMERIC=C*, *LC_TIME=C.UTF-8*,
*LC_COLLATE=C.UTF-8*, *LC_MONETARY=C.UTF-8*, *LC_MESSAGES=C.UTF-8*,
*LC_PAPER=C.UTF-8*, *LC_NAME=C*, *LC_ADDRESS=C*, *LC_TELEPHONE=C*,
*LC_MEASUREMENT=C.UTF-8* and *LC_IDENTIFICATION=C*

**attached base packages:** *stats*, *graphics*, *grDevices*, *utils*,
*datasets*, *methods* and *base*

**other attached packages:** *gsm.qtl(v.1.1.1)*, *testthat(v.3.3.2)*,
*riskmetric(v.0.2.6)*, *stringr(v.1.6.0)*, *gh(v.1.5.0)*,
*pander(v.0.6.6)*, *purrr(v.1.2.1)*, *dplyr(v.1.1.4)*, *knitr(v.1.51)*,
*gt(v.1.3.0)* and *gsm.core(v.1.1.7)*

**loaded via a namespace (and not attached):** *gtable(v.0.3.6)*,
*xfun(v.0.56)*, *bslib(v.0.10.0)*, *ggplot2(v.4.0.1)*,
*htmlwidgets(v.1.6.4)*, *devtools(v.2.4.6)*, *remotes(v.2.5.0)*,
*vctrs(v.0.7.1)*, *tools(v.4.5.2)*, *generics(v.0.1.4)*,
*curl(v.7.0.0)*, *tibble(v.3.3.1)*, *pkgconfig(v.2.0.3)*,
*data.table(v.1.18.2.1)*, *RColorBrewer(v.1.1-3)*, *S7(v.0.2.1)*,
*desc(v.1.4.3)*, *lifecycle(v.1.0.5)*, *farver(v.2.1.2)*,
*compiler(v.4.5.2)*, *brio(v.1.1.5)*, *textshaping(v.1.0.4)*,
*htmltools(v.0.5.9)*, *usethis(v.3.2.1)*, *sass(v.0.4.10)*,
*yaml(v.2.3.12)*, *lazyeval(v.0.2.2)*, *plotly(v.4.12.0)*,
*pillar(v.1.11.1)*, *pkgdown(v.2.2.0)*, *jquerylib(v.0.1.4)*,
*tidyr(v.1.3.2)*, *ellipsis(v.0.3.2)*, *cranlogs(v.2.1.1)*,
*cachem(v.1.1.0)*, *sessioninfo(v.1.2.3)*, *tidyselect(v.1.2.1)*,
*digest(v.0.6.39)*, *stringi(v.1.8.7)*, *forcats(v.1.0.1)*,
*rprojroot(v.2.1.1)*, *fastmap(v.1.2.0)*, *grid(v.4.5.2)*,
*here(v.1.0.2)*, *cli(v.3.6.5)*, *magrittr(v.2.0.4)*,
*triebeard(v.0.4.1)*, *pkgbuild(v.1.4.8)*, *withr(v.3.0.2)*,
*waldo(v.0.6.2)*, *scales(v.1.4.0)*, *backports(v.1.5.0)*,
*rmarkdown(v.2.30)*, *httr(v.1.4.7)*, *otel(v.0.2.0)*, *ragg(v.1.5.0)*,
*memoise(v.2.0.1)*, *evaluate(v.1.0.5)*, *log4r(v.0.4.4)*,
*covr(v.3.6.5)*, *rex(v.1.2.1)*, *viridisLite(v.0.4.2)*,
*rlang(v.1.1.7)*, *urltools(v.1.7.3.1)*, *Rcpp(v.1.1.1)*,
*glue(v.1.8.0)*, *BiocManager(v.1.30.27)*, *xml2(v.1.5.2)*,
*pkgload(v.1.4.1)*, *rstudioapi(v.0.18.0)*, *jsonlite(v.2.0.0)*,
*R6(v.2.6.1)*, *systemfonts(v.1.3.1)* and *fs(v.1.6.6)*

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

#### Pull Request 67: Catch up dev from main

Merging main into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/67>

| Requester |   Date Requested    |  Reviewers   | Review Status |
|:---------:|:-------------------:|:------------:|:-------------:|
|  zdz2101  | 2026-01-16 19:24:52 | lauramaxwell |   APPROVED    |

#### Pull Request 66: Update package dependencies.

Merging fix-65 into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/66>

| Requester |   Date Requested    |                       Reviewers                        | Review Status |
|:---------:|:-------------------:|:------------------------------------------------------:|:-------------:|
| samussiah | 2026-01-16 19:23:10 | copilot-pull-request-reviewer\[bot\] zdz2101 samussiah |   COMMENTED   |

#### Pull Request 63: Release v1.1.0

Merging rc-v1.1.0 into main

<https://github.com/Gilead-BioStats/gsm.qtl/pull/63>

| Requester |   Date Requested    |  Reviewers   | Review Status |
|:---------:|:-------------------:|:------------:|:-------------:|
|  zdz2101  | 2026-01-08 18:15:53 | lauramaxwell |   APPROVED    |

#### Pull Request 62: Circular Dependency Workaround

Merging fix-circ-deps into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/62>

| Requester |   Date Requested    |      Reviewers       | Review Status |
|:---------:|:-------------------:|:--------------------:|:-------------:|
|  zdz2101  | 2026-01-07 20:37:07 | zdz2101 lauramaxwell |   COMMENTED   |

#### Pull Request 61: Closes \#60 adds functionality for a percentage based plot for barcharts

Merging fix-60 into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/61>

| Requester |   Date Requested    |                       Reviewers                        | Review Status |
|:---------:|:-------------------:|:------------------------------------------------------:|:-------------:|
|  zdz2101  | 2025-12-04 18:54:22 | jwildfire copilot-pull-request-reviewer\[bot\] zdz2101 |   APPROVED    |

#### Pull Request 59: Closes \#58 removes `gsm.kri` dependency

Merging fix-58 into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/59>

| Requester |   Date Requested    |                       Reviewers                        | Review Status |
|:---------:|:-------------------:|:------------------------------------------------------:|:-------------:|
|  zdz2101  | 2025-10-28 18:07:16 | samussiah copilot-pull-request-reviewer\[bot\] zdz2101 |   COMMENTED   |

#### Pull Request 57: Catch main branch up to dev

Merging main into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/57>

|  Requester  |   Date Requested    | Reviewers | Review Status |
|:-----------:|:-------------------:|:---------:|:-------------:|
| nandriychuk | 2025-10-01 20:20:41 |  zdz2101  |   APPROVED    |

#### Pull Request 56: Release v1.0.1

Merging release-1.0.1 into main

<https://github.com/Gilead-BioStats/gsm.qtl/pull/56>

|  Requester  |   Date Requested    | Reviewers | Review Status |
|:-----------:|:-------------------:|:---------:|:-------------:|
| nandriychuk | 2025-10-01 20:08:39 |  zdz2101  |   APPROVED    |

#### Pull Request 55: Catch dev up to main before pushing qual report

Merging main into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/55>

| Requester |   Date Requested    |  Reviewers  | Review Status |
|:---------:|:-------------------:|:-----------:|:-------------:|
|  zdz2101  | 2025-10-01 15:03:41 | nandriychuk |   APPROVED    |

#### Pull Request 54: Update qualification report

Merging update-qualification-report into dev

<https://github.com/Gilead-BioStats/gsm.qtl/pull/54>

|  Requester  |   Date Requested    |     Reviewers     | Review Status |
|:-----------:|:-------------------:|:-----------------:|:-------------:|
| nandriychuk | 2025-10-01 14:59:01 | zdz2101 jwildfire |   APPROVED    |
