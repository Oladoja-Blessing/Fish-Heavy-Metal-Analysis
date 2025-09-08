# Fish Metal Analysis

This repository presents the results of a contracted data analysis of heavy metal concentrations in **Hepsetus odoe** and **Parachana obscura**. The study covered both **monthly variations** (May–October) and **body part–specific accumulation** (gill, intestine, tail).

## 📊 Objectives
- Quantify concentrations of heavy metals (**Cu, Cd, Fe, Mn, Ni, Pb**) in fish tissues.  
- Explore temporal variation (monthly differences, May–October).  
- Assess bioaccumulation across tissues (gill, intestine, tail).  
- Identify correlations between different heavy metals.  

## 🧰 Tools & Libraries
- **Primary environment:** R (≥ 4.0), RStudio.  
- **Key R packages:** `tidyverse`, `ggplot2`, `ggpubr`, `janitor`.

## 📂 Repository Structure 
- `results/` → statistical summary tables and generated plots.  
- `scripts/` → R scripts for analysis and visualization.  

## 📈 Summary of Results
- **Correlation:** Pike showed strong Fe–Pb correlation (r = 0.51), while Parachana had weaker overall correlations.  
- **Monthly Variation:**  
  - Pike had unusually high Cu and Ni in October and September, respectively.  
  - Parachana showed consistently stable Fe, with minor increases in Mn from July–October.  
- **Body Part Accumulation:**  
  - Pike gills contained the highest Cu and Ni values.  
  - Parachana intestines accumulated more Fe compared to tail tissue.  
- **Notable Outliers:**  
  - Pike gills (October) had extremely elevated Cu (17.39 ± 17.04) and Ni (33.19 ± 33).  

## 📜 License
This project is licensed under the MIT License.
