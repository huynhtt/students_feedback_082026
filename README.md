# Student Evaluation of Teaching Dataset

## Overview

This repository contains an anonymized survey dataset on students' perceptions of and engagement with student evaluation of teaching (SET) in a private university context in Vietnam. The released data support analysis of institutional loyalty, trust in the evaluation process, perceived usefulness of teaching evaluations, willingness to provide constructive feedback, perceived bias, academic stage, academic major, gender, and GPA.

The dataset contains 522 valid responses. The final measurement instrument includes 15 retained questionnaire items organized into five constructs.

## Data collection

Data were collected through a cross-sectional survey conducted in May 2026 at FPT University, Ho Chi Minh City Campus, Vietnam. The target population consisted of undergraduate students enrolled in four academic programs: Artificial Intelligence (AI), Software Engineering (SE), Integrated Circuit Design (IC), and Business. The combined student population across these programs was approximately 2,000.

The questionnaire was administered through Google Forms in a bilingual English–Vietnamese format. Survey links and QR codes were provided to students through selected classes, and students completed the questionnaire using their own devices. Participation was voluntary and anonymous. Students could decline participation by not accessing the survey, while those who chose to participate were required to provide informed consent within the online form before proceeding. Participants were informed that their responses would be kept confidential and used solely for research purposes.

All questionnaire fields required for the released dataset were configured as mandatory in Google Forms. Consequently, the dataset contains no missing responses for these variables. A total of 522 students completed the survey, and all 522 responses were retained in the released dataset; no observations were excluded during data preparation.

Because recruitment was conducted through scheduled classes, students who were absent during the survey administration sessions did not have the same opportunity to participate. The resulting dataset should therefore be regarded as a classroom-based convenience sample rather than a probability sample of the approximately 2,000 students in the four programs.

## Ethics and consent

Participation in the survey was voluntary and anonymous. Students were informed of the purpose of the research, the confidentiality of their responses, and their right to decline participation. Students who chose to participate provided informed consent electronically through Google Forms before completing the questionnaire. The collected information was used solely for research purposes.

## Repository contents

| File | Description |
|---|---|
| `dataset.xlsx` | Anonymized respondent-level dataset. The 15 retained questionnaire items are named `x1`–`x15`. |
| `codebook.xlsx` | Data dictionary, value labels, item mapping, and bilingual English–Vietnamese questionnaire wording. |
| `questionnaire_items.docx` | Documentation of the retained questionnaire items, constructs, response scale, original item mapping, and sources. |
| `analysis_reproducibility.R` | Reproducible R script for data preparation, measurement assessment, structural analyses, robustness checks, tables, and figures. |
| `sessionInfo.txt` | R session and package information for computational reproducibility. |
| `README.md` | Repository overview and reuse instructions. |
| `LICENSE` | License terms for the released materials. |

## Dataset structure

Each row represents one respondent.

The main variables are:

- `id`: anonymized respondent identifier.
- `x1`–`x15`: retained questionnaire items measured on a five-point Likert scale.
- `major`: academic major (`1 = AI`, `2 = SE`, `3 = IC`, `4 = Business`).
- `group`: academic stage (`EarlyYear` or `FinalYear`).
- `gender`: gender (`0 = Male`, `1 = Female`).
- `GPA`: grade point average on a 0–10 scale.
- `Loyalty`, `Trust`, `Usefulness`, `Willingness`, and `Bias`: construct scores calculated as the mean of the three corresponding questionnaire items.

The five-point response scale for `x1`–`x15` is:

1. Strongly disagree / Hoàn toàn không đồng ý
2. Disagree / Không đồng ý
3. Neutral / Trung lập
4. Agree / Đồng ý
5. Strongly agree / Hoàn toàn đồng ý

For exact variable definitions, bilingual item wording, coding, and item-to-construct mapping, see `codebook.xlsx`.

## Construct mapping

| Construct | Dataset variables |
|---|---|
| Institutional Loyalty | `x1`, `x2`, `x3` |
| Trust in the Evaluation Process | `x4`, `x5`, `x6` |
| Perceived Usefulness of Teaching Evaluations | `x7`, `x8`, `x9` |
| Willingness to Provide Constructive Feedback | `x10`, `x11`, `x12` |
| Perceived Bias | `x13`, `x14`, `x15` |

The released `x1`–`x15` identifiers are sequential names for the 15 retained items. Their mapping to the original questionnaire item identifiers is documented in the codebook and questionnaire file.

## Reproducibility

Place `dataset.xlsx` and `analysis_reproducibility.R` in the same working directory and run the R script. The script performs the main measurement and structural analyses and generates manuscript tables and figures.

Required R packages include:

- `readxl`
- `dplyr`
- `tidyr`
- `psych`
- `lavaan`
- `semTools`
- `writexl`
- `ggplot2`

The optional SEM figure additionally requires `semPlot`.

## Data reuse

The files are intended to support verification, secondary analysis, methodological reuse, and comparative research on student evaluation of teaching. Users should consult the codebook before analysis, particularly for categorical coding and the distinction between questionnaire items and derived construct scores.

The dataset is anonymized. Users should not attempt to re-identify respondents or combine the released data with external information for re-identification.

## License

The released materials are provided under the terms stated in the `LICENSE` file.
