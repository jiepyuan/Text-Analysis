# Repository Structure

This repository contains the code and results used in the sentiment analysis tutorial paper. It is organized into two major components:

1. `sentiment extraction and data aggregation/`: sentiment extraction from social media text and aggregation of the resulting sentiment scores.
2. `longitudinal data analysis/`: Bayesian estimation of the longitudinal models described in the manuscript.

The sentiment analyses include three lexicon-based methods (AFINN, Bing, and NRC) and three locally hosted large language models (Gemma, LLaMA, and Mistral). The longitudinal analyses include the Heteroscedastic Linear Mixed Model (HLMM), Growth Curve Model (GCM), autoregressive model [AR(1)], and Trajectory model with Recurrent Autocorrelation and Cumulative Event-triggered effects (TRACE).

## Sentiment-score aggregation methods

Four aggregation methods are used throughout the repository:

* `combine`: Combined monthly score.
* `separate-all-avg`: Monthly mean of all daily scores.
* `separate-pos-avg`: Monthly mean based on positive-only daily scores.
* `separate-neg-avg`: Monthly mean based on negative-only daily scores.

---

## `sentiment extraction and data aggregation/`

This folder contains the code for extracting sentiment scores from social media text data and aggregating those scores for subsequent longitudinal analyses.

It is organized into two subfolders:

* `lexicon/`: Sentiment extraction and data aggregation using lexicon-based methods.
* `llm/`: Sentiment extraction using locally hosted large language models through Ollama, followed by data aggregation.

---

## `sentiment extraction and data aggregation/lexicon/`

This folder contains three subfolders corresponding to the three lexicon-based sentiment analysis methods:

* `afinn/`: Analyses using the AFINN sentiment lexicon.
* `bing/`: Analyses using the Bing sentiment lexicon.
* `nrc/`: Analyses using the NRC sentiment lexicon.

For the lexicon-based methods, sentiment extraction and data aggregation are performed within the same analysis scripts.

### `lexicon/afinn/`

The `afinn` folder contains two subfolders:

* `text/`: Extracts and aggregates sentiment from text only.
* `text and emoji/`: Extracts and aggregates sentiment from both text and emoji. Emoji sentiment scores are calculated using the `EmojiSentR` R package.

#### `lexicon/afinn/text/`

This folder is organized into four subfolders corresponding to the four aggregation methods:

* `text (combine)/`: Produces the combined monthly score.
* `text (separate-all-avg)/`: Produces the monthly mean of all daily scores.
* `text (separate-pos-avg)/`: Produces the positive-only daily mean.
* `text (separate-neg-avg)/`: Produces the negative-only daily mean.

For example, `text (combine)/` contains the following files:

* `afinn_combine_txt.R`

  Extracts text sentiment using AFINN and aggregates the resulting scores into combined monthly scores.

* `function.R`

  Defines the functions called by `afinn_combine_txt.R`.

* `afinn_combine_txt.png`

  Plot of the sentiment scores after data aggregation.

The other three aggregation folders follow the same organization and differ only in the aggregation method applied.

#### `lexicon/afinn/text and emoji/`

This folder follows the same structure as `lexicon/afinn/text/`:

* `text and emoji (combine)/`: Produces the combined monthly score.
* `text and emoji (separate-all-avg)/`: Produces the monthly mean of all daily scores.
* `text and emoji (separate-pos-avg)/`: Produces the positive-only daily mean.
* `text and emoji (separate-neg-avg)/`: Produces the negative-only daily mean.

These analyses incorporate both text sentiment and emoji sentiment, with emoji sentiment scores calculated using `EmojiSentR`.

### `lexicon/bing/` and `lexicon/nrc/`

The `bing` and `nrc` folders follow the same analysis and aggregation workflow as `lexicon/afinn/text/`, but use the Bing and NRC sentiment lexicons, respectively.

Each folder contains a `text/` subfolder with four aggregation-specific folders. Unlike `afinn/`, the Bing and NRC analyses include text-only sentiment extraction and therefore do not contain a `text and emoji/` subfolder.

---

## `sentiment extraction and data aggregation/llm/`

This folder contains the analyses based on locally hosted large language models accessed through Ollama. Because LLM-based sentiment extraction requires calls to Ollama, sentiment extraction and data aggregation are separated into two stages:

* `data generation/`: Generates sentiment-score data using local LLMs.
* `data aggregation/`: Aggregates the generated sentiment scores using the four aggregation methods.

### `llm/data generation/`

This folder contains the following files:

* `LLMs_combine.R`

  Generates the sentiment-score data required to calculate combined monthly scores.

* `LLMs_separate.R`

  Generates the sentiment-score data required to calculate the monthly mean of all daily scores, positive-only daily mean, and negative-only daily mean.

### `llm/data aggregation/`

This folder contains three subfolders corresponding to the three LLMs:

* `gemma/`: Aggregation of sentiment scores generated with Gemma.
* `llama/`: Aggregation of sentiment scores generated with LLaMA.
* `mistral/`: Aggregation of sentiment scores generated with Mistral.

Each model folder contains:

* `text/`: Aggregation of sentiment scores extracted from text only.
* `text and emoji/`: Aggregation of sentiment scores extracted from both text and emoji.

Within these folders, the organization and naming of the four aggregation methods follow the same logic described for `lexicon/afinn/`.

---

## `longitudinal data analysis/`

This folder contains the code and results for estimating the longitudinal models described in the manuscript within a Bayesian framework.

It is organized into two subfolders:

* `lexicon/`: Longitudinal analyses based on sentiment scores obtained using AFINN, Bing, and NRC.
* `llm/`: Longitudinal analyses based on sentiment scores obtained using Gemma, LLaMA, and Mistral.

---

## `longitudinal data analysis/lexicon/`

This folder contains three method-specific subfolders:

* `afinn/`
* `bing/`
* `nrc/`

### `longitudinal data analysis/lexicon/afinn/`

The `afinn` folder contains:

* `text/`: Models fitted to sentiment scores extracted from text only.
* `text and emoji/`: Models fitted to sentiment scores extracted from both text and emoji.

#### `lexicon/afinn/text/`

This folder contains four aggregation-specific subfolders:

* `afinn_txt_combine/`: Analyses using the combined monthly score.
* `afinn_txt_seperate_all_avg/`: Analyses using the monthly mean of all daily scores.
* `afinn_txt_seperate_pos_avg/`: Analyses using the positive-only daily mean.
* `afinn_txt_seperate_neg_avg/`: Analyses using the negative-only daily mean.

The spelling `seperate` is retained here to match the folder names used in the repository.

Each aggregation-specific folder contains four model folders:

* `ar1/`: Autoregressive model [AR(1)].
* `gcm/`: Growth Curve Model (GCM).
* `hlmm/`: Heteroscedastic Linear Mixed Model (HLMM).
* `trace/`: Trajectory model with Recurrent Autocorrelation and Cumulative Event-triggered effects (TRACE).

For example, the `ar1/` folder contains:

* `AR1.R`

  Fits the AR(1) model within a Bayesian estimation framework.

* `result/`

  Stores the model-estimation results produced by `AR1.R`.

The `gcm/`, `hlmm/`, and `trace/` folders follow the same organization: each contains the R code used to estimate the corresponding Bayesian model and a `result/` folder containing its output.

#### `lexicon/afinn/text and emoji/`

This folder follows the same aggregation and model organization as `lexicon/afinn/text/`, but uses sentiment scores that incorporate both social media text and emoji.

### `lexicon/bing/` and `lexicon/nrc/`

The `bing` and `nrc` folders follow the same organization and modeling workflow as the AFINN analyses, using sentiment scores derived from the corresponding lexicon-based method.

---

## `longitudinal data analysis/llm/`

This folder contains three subfolders corresponding to the locally hosted LLMs:

* `gemma/`: Longitudinal analyses using sentiment scores generated with Gemma.
* `llama/`: Longitudinal analyses using sentiment scores generated with LLaMA.
* `mistral/`: Longitudinal analyses using sentiment scores generated with Mistral.

Each model folder follows the same organizational logic as the lexicon-based longitudinal analyses. The analyses are separated by input type (`text/` and `text and emoji/`), sentiment-score aggregation method, and longitudinal model (`ar1/`, `gcm/`, `hlmm/`, and `trace/`). Each model folder contains the corresponding Bayesian estimation script and a `result/` folder containing the model output.

---

## Simplified directory tree

```text
Text Analalysis Tutorial Code/
|-- sentiment extraction and data aggregation/
|   |-- lexicon/
|   |   |-- afinn/
|   |   |   |-- text/
|   |   |   `-- text and emoji/
|   |   |-- bing/
|   |   `-- nrc/
|   `-- llm/
|       |-- data generation/
|       |   |-- LLMs_combine.R
|       |   `-- LLMs_separate.R
|       `-- data aggregation/
|           |-- gemma/
|           |-- llama/
|           `-- mistral/
`-- longitudinal data analysis/
    |-- lexicon/
    |   |-- afinn/
    |   |-- bing/
    |   `-- nrc/
    `-- llm/
        |-- gemma/
        |-- llama/
        `-- mistral/
```
