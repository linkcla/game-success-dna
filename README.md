# What determines the success of a game?

**Course**: 21746 - Data Mining - Research Project
**Institution**: Universitat de les Illes Balears
**Authors**: Antonio Contestí Coll, Josep Ferriol Font, Francesc Gayà Piña, Carlos Gálvez Mena, Jordi Vanyó Mestre, Marc Link Cladera

## Project Overview
This repository contains the code and the final research paper for the project "What determines the success of a game?". The study explores the determinants of video game success on the Steam platform using a data-driven approach. 

The primary objective was to determine whether a game's success is a structurally identifiable phenomenon. To achieve this, we employed multivariate clustering (GMM) to segment the market and identify distinct success profiles based on performance metrics.

## Methodology

### Data Collection and Cleaning
The original dataset exhibited severe quality issues and inconsistencies, particularly in review volume and sentiment analysis. To establish a reliable data foundation, we conducted our own data extraction using the official Steam API and SteamDB. We reduced statistical noise by excluding games with a median playtime of less than 30 minutes that also lacked user reviews, refining our sample from over 80,000 entries to 6,500 records.

### Feature Engineering
Variables were categorized into feature variables (intrinsic game characteristics) and performance variables (market metrics like owners and playtime). Highly correlated growth variables were consolidated into a single component using Principal Component Analysis (PCA) to avoid multicollinearity. Textual descriptions were transformed using TF-IDF and Singular Value Decomposition (SVD) to evaluate their semantic contribution.

### Clustering
We used Gaussian Mixture Models (GMM) due to its probabilistic approach, which is well-suited for the diffuse boundaries found in user behavior data. The algorithm identified 7 distinct market clusters. For visualization, the multidimensional space was projected into 2D using UMAP.

## Key Findings

Our analysis concluded that a game's potential success is encoded in its structural "DNA" rather than in semantic nuances. The main determinants include:

* Global scalability: The number of supported languages has a strong structural correlation with success.
* Mechanical depth: A higher density of assigned Steam tags correlates with better performance, serving as an indicator of feature richness.
* Pricing strategy: Price acts as a market positioning signal and a barrier to entry. The Free-to-Play model effectively lowers this barrier to rapidly acquire users, whereas premium pricing requires stronger structural attributes to succeed.

## Repository Structure

* `What determines the success of a game.pdf`: The complete academic paper detailing the study, methodology, and comprehensive conclusions.
* `scripts/`, `dna_analysis/`, and `econometrics_analysis/`: Source code for scraping, preprocessing, clustering, and analysis.