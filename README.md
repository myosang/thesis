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

