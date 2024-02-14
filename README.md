## Overview
This paper analyzes the public opinions in various countries (U.S., Denmark, Chile, Brazil, Japan, Philippines) about women's involvement in the workforce under 4 circumstances: “Do you think women should work outside the home full-time, part-time, or not at all when…
1. they are married but with no children?”
2. there is a child under school age?” 
3. the youngest child is still in school?”
4. the child has left home?”

## Repo Structure

inputs/data contains the data sources used in analysis, including raw and cleaned data.

outputs/paper contains the files used to generate the paper, including the Quarto document, the bibliography file, and the finalized version of the paper in the form of a PDF.

scripts contains the R scripts used to simulate, download, and clean data.

## Reproducing the Report
1. Run scripts/01-download_data.R to download raw data
2. Run scripts/02-data_cleaning.R to generate cleaned data
3. Render outputs/paper/gender_wage_gap.qmd to generate the PDF of the paper

## LLM Usage
LLMs were not used in the making of this paper.
