### Master Thesis Project in Data science FU Berlin
"How do Political Leanings influence on Open-Ended Survey responses"

## Overview
This project analyzes how political leanings influence topics emerging from **open-ended survey responses**.  
It compares **political** (ANES 2020) and **non-political** (Knowledge-Exploration) datasets using:  
- **Latent Dirichlet Allocation (LDA)**  
- **Clustering** (K-means, OPTICS, HDBSCAN) with modern embeddings  

## Project Structure
```text
├── data/                # Raw and preprocessed datasets 
├── embeddings/           # Vectorized text data using each embedding models in file name.
├── scripts/               # Jupyter notebook for EDA, Clustering, Evaluation.
│   ├── anes/              # political dataset 
│   │   ├── participants/  # survey participants' based analysis
│   │   ├── topics/        # survey topic based analysis
│   ├── mpib               # non-political dataset
│   │   ├── participants/   # survey participants' based analysis
│   │   ├── topics/         # survey topic based analysis
└── README.md
```
## Pipeline
1. Preprocessing – lowercasing, stopword removal, lemmatization
2. Feature Extraction – Bag-of-Words, TF-IDF, Word2Vec, GloVe, DistilBERT
3. Dimensionality Reduction – UMAP for visualization and clustering
4. Modeling
  - LDA for probabilistic topic modeling
  - Clustering with K-means / OPTICS / HDBSCAN
5. Evaluation – Coherence, topic diversity, Silhouette, Calinski-Harabasz, Davies-Bouldin
6. Hypothesis verification by statistical Tests – Chi-square, ANOVA for demographic correlations

## Key Findings
* Political context (ANES) shows clear polarization in topic distribution
* Non-political context (gender equality) shows no significant political correlation
* Clustering outperformed LDA in topic diversity and coherence
