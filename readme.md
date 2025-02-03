
# Addressing panel conservatism in research funding

Thomas Feliciani

## Overview

This repository contains the scripts used for the data preparation, simulation and analysis behind a research paper currently under review. The paper is authored by Thomas Feliciani, Rikke Nørding Christensen, and Chiara Franzoni, and is provisionally titled: "_Addressing conservatism in research funding with lotteries, golden tickets, and novelty-dedicated panels_".

This research uses grant peer review data from the Novo Nordisk Foundation (NNF). These data cannot be included in this repository and must be requested directly from NNF under a data-sharing agreement. While the absence of data prevents full replication of our research, this repository enables inspection of our methods and facilitates the reuse of its components.

This code is licensed under the [GNU General Public License v3.0 (GPL-3.0)](https://www.gnu.org/licenses/gpl-3.0.html). See the [LICENSE](./LICENSE) file in the repository root for details.

The contents of this repository are organized by theme into three folders: *metrics*, *simulation*, and *analysis and visualization*. The purpose, contents and instructions for each of the three themes/folders are provided below.


## Metrics

The two main dependent variables in the paper are two dimensions of 'innovation' or 'novelty' in research grant proposals: _New Vintage_ (i.e. the degree to which a proposal introduces new ideas in the literature) and _Creative Recombination_ (i.e. the degree to which a proposal recombines existing ideas in new or unusual ways). Our paper describes five metrics: two indicators of New Vintage, two of Creative Recombination, and a hybrid measure to simultaneously capture both.

Each of these five metrics is based on either one of these approaches:

* Identification of new N-grams -- that is, single words, two-word strings (aka "bigrams") and three-word strings ("trigrams") that appear in the proposal's text and did not appear in the published academic literature prior to the proposal submission. We use this approach to measure New Vintage.

* Average semantic (dis)similarity among cited references. This approach leverages word embeddings to measure the similarity between the titles (or, alternative, the abstract) of the references cited in the text of proposal. This approach allows to measure Creative Recombination.

* Medical Subject Headings (MeSH). MeSH is a curated taxonomy of keywords in research. The time lag between the appearance of MeSH keywords in the taxonomy and their use in a grant proposal allows to measure New Vintage of the ideas used in the proposal. And by examining _pairs_ of MeSH keywords associated with a given proposal, we create the hybrid metric that simultaneously captures New Vintage and Creative Recombination.

For the calculation of metrics based on *new N-grams*, we resort to the method proposed by Arts et al. (2023a), along with the scripts and datasets they have published in Zenodo (2023b, version v1 -- https://doi.org/10.5281/ZENODO.8283353).

For the calculation of metrics based on the *semantic similarity* among cited references, we have reproduced the approach by Shibayama et al. (2021). Readily available instructions and scripts for the calculation of this index can be found in the "Novelpy" library for Python by Pelletier & Wirtz (2022 -- see also https://novelpy.readthedocs.io/en/latest/).

Finally, for the calculation of *MeSH-based metrics*, we developed our own scripts, generally inspired by the work of Azoulay et al. (2011). The folder "./metrics/" of this repository contains the scripts we used.

### MeSH-based metrics

These scripts are written for R (version 4.4.0) and Python (versions 3.11.7 and 3.12.3). These scripts require API keys for various services provided by the the U.S. National Institutes of Health (NIH). To access these services we applied for two API credentials: (1) for the e-utils by the National Library of Medicine (NIH/NLM), and (2) for the Entrez e-utilities by the National Center for Biotechnology Information (NIH/NCBI). The former is needed to access MeSH metadata and use the MeSH-on-Demand APIs that we used to assign MeSH terms to NNF applications. The latter is needed for querying PubMed to determine when certain pairs of MeSH terms were first used together in the literature. These crentials consists in username-password pairs, and need to be saved in two separate txt files. These files are two-lines long, and the script assumes that the first line will be the username, and the second line the API key / password. NIH/NLM API credential file is assumed to be saved as "./scripts/api key.txt". NIH/NCBI API credential file should be saved as ".scripts/api key NCBI.txt". Txt files will work with standard character encoding (e.g. UTF-8).

Here follows an outline of the scripts enclosed in this folder. They are presented in the order in which we ran them.

### Single MeSH terms

* _MeSH.py_ takes the raw text of each NNF application and queries MeSH-on-Demand APIs to assign the most relevant MeSH terms. Results are exported in table format to "./output/MeSH terms.csv". In actuality, due to the slow server response we parallelized the execution for this script by diving the corpus of NNF applications in three chunks, and then running a cloned instance of this script separaterly for each chunk. The resulting tables were then stitched together by the script ./scripts/MeSH.r (see further below).

* _MeSH - data repair.py_ re-queries the necessary APIs for the MeSH entries that were in some ways broken -- e.g. due to a server-side error message being returned, instead of the expected data.

* _MeSH.r_ takes the output from the previous two scripts on the MeSH terms associated with each NNF applcation and calculates the final indicators of novelty based on single MeSH terms. The resulting table saved as "./output/MeSH_novelty.csv"

### Pairs of MeSH terms

* _1_MeSH pairs.r_ finds all pairs of MeSH terms for which we need to calculate the age/recency/vintage.

* _2_Pubmed esearch API.py_ queries PubMed records (via NIH/NLM/NCBI API) to fetch the date of publication of the oldest paper associated with the two given MeSH terms (aka the "date of first pairing").

* _3_Pubmed esearch correction.py_ fixes some missing data and artifacts stemming from faulty query responses occurred during the running of the previous script.

* _4_MeSH pairs analysis.r_ calculcates the final indicators based on MeSH term pairs. It exports a csv as ./output/MeSH_pairs_novelty.csv"

### Dependencies

MeSH-based indicators enclosed herein were constructed based on the following resources:

* NNF application data (not included in this repository);

* e-utils by the National NIH/NLM (Library of Medicine). These were queried in April 2024;

* PubMed/Entrez e-utilities by the NIH/NCBI (National Center for Biotechnology Information). These were queried between June-August 2024.

All scripts are written for R (version 4.4.0) and Python (versions 3.11.7 and 3.12.3 -- specified at the top of each .py script). The set of external libraries required varies across scripts. A list of needed library is provided in the first few lines of each script.

## Simulation

This folder contains the scripts that we used to simulate counterfactual funding decisions for the real NNF grant review panels.

The scripts were written for R 4.4.1, and two of the scripts, "_aggregation_data_prep.r_" and "_aggregation_counterfactuals.r_", rely on various external libraries that are listed at the top of the scripts. A step-by-step explanation of script instructions is provided in the form of comments embedded in the scripts themselves. In the remainder of this section we provide a summary overview of the contents and function of each of these scripts.

### Script "_utils.r_" 

This script defines some auxiliary functions that are used in the counterfactual experiments.

### Script "_aggregation_data_prep.r_"

This script reads in the data from NNF (not included), along with the associated proposal scores across the various metrics of New Vintage and Creative Recombination. In particular:

* The data.frame "_a_" contains proposal-level data, including the id of each proposal, their date of submission, the self-reported gender of the main applicant, as well as call-level information, such as the number of reviewers, acceptance rate, and all other control variables described in the paper.

* The data.frame "_s_" contains evaluations: a row of _s_ contains information on a particular evaluation made by a reviewer to a specific porposal that they reviewed. Fields include the id of the reviewer, the id of the proposal, the score (on a 1-to-5 or 1-to-6 evaluation scale used by NNF), along with any written comment the reviewer might have added.

* The data.frame "_callclass_" has call-level information, including the year a particular call was held, and various fields that identify the type of funding instrument. Particularly relevant here is the distinction between "Novelty-Dedicated" (Synergy) calls, marked e.g. as "grant_hrhg", and all other "Regular" calls.


After loading these data files, the first half of the script "_aggregation_data_prep.r_" is dedicated to preparing the data for the analyses and simulation experiments. The script formats, cleans and recodes these data, merging the information at the proposal level whenever possible. It then adds to the proposal-level dataset "_a_" all available New Vintage and Creative Recombination scores (see [previous section of this readme file, "Metrics"](#metrics)). It filters out all data from funding calls that are excluded from our analysis (e.g. calls that received fewer than two submissions, calls with no grant winners, calls for which evaluation data were incomplete, etc).

The second part of this script is dedicated to some preliminary analyses -- from descriptive statistics, bivariate correlation, and then hypothesis testing on the evidence for conservatism in funding decisions at NNF.

At the end of the script, we write to disk all processed data from _a_, _s_, and _callclass_  in compressed form (file "./counterfactuals/input.RData").

### Script "_aggregation_counterfactuals_"

This script performs the following:

1. It loads the previously prepared data from real-wrold NNF calls ("./counterfactuals/input.RData").

2. It sets up an experiment in the form of a data.frame, called _treatments_, where each row represents a counterfactual simulation of a given real-world NNF call, generated from a given random seed.

3. It performs the simulations of counterfacutals, using as decision rule for funding decisions the protocol of one of intervetions (Golden Tickets, Tie-Breaking Lottery or Fundable-Pool Lottery). The protocols / algorithms for simulating these interventions are defined inside the function "_sim_".

4. It calculates some statistics to summarize the outcome of each counterfactual call, and writes them to the _treatments_ data.frame. These outcomes include:

    * Cohen's Kappa (the similarity between the decisions by the simulated counterfacutal panel, and the decisions made by their real-world counterpart).

    * For each of the New Vintage and Creative Recombination metrics, the average novelty of funded proposals.

    * Same as above, but for declined proposals.

5. Because simulations are run in parallel, each parallel execution writes a partial _treatments_ data.frame to a different file. These various partial files are joined into one complete 'output' data.frame, saved to disk as a .RData file for futher analyes (see next section).

## Analysis and visualization

This folder contains the script "_./counterfactuals/input.RData_", written for R 4.4.1, which produces the remaining analyses described in the paper (including the robustness tests) and produces the plots reported in the paper and its appendix.

Except for the infographic of Figure 1, all other paper figures are produced with this script using the library _ggplot2_. This script also requires the libraries "_ggpattern_" and "_gridExtra_".




## References

Arts, S., Melluso, N., & Veugelers, R. (2023a). _Beyond Citations: Measuring Novel Scientific Ideas and their Impact in Publication Text_. https://doi.org/10.48550/ARXIV.2309.16437

Arts, S., Melluso, N., Veugelers, R., & Aristodemou, L. (2023b). _Publication text: Code, data, and new measures_ [Dataset]. Zenodo. https://doi.org/10.5281/ZENODO.8283353

Azoulay, P., Graff Zivin, J. S., & Manso, G. (2011). Incentives and creativity: Evidence from the academic life sciences. _The RAND Journal of Economics_, 42(3), 527–554. https://doi.org/10.1111/j.1756-2171.2011.00140.x

Pelletier, P., & Wirtz, K. (2022). _Novelpy: A python package to measure novelty and disruptiveness of bibliometric and patent data_. arXiv preprint arXiv:2211.10346. https://doi.org/10.48550/arXiv.2211.10346

Shibayama, S., Yin, D., & Matsumoto, K. (2021). Measuring novelty in science with word embedding. _PLOS ONE_, 16(7), e0254034. https://doi.org/10.1371/journal.pone.0254034
