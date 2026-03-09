# 🛩️🐋 UAV-based Thermal Flukeprint Imagery for Non-Invasive Monitoring of Humpback Whales 

Scripts, datasets, and supplementary materials accompanying the manuscript:  
**In the Tracks of a Whale: Inferring Size Class, Orientation, and Swimming Speed from Thermal Flukeprints.** , submitted to *Journal of Experimental Biology*.

[![R](https://img.shields.io/badge/R-4.2%2B-blue)]()
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)]()
[![DOI](https://zenodo.org/badge/1075426076.svg)](https://doi.org/10.5281/zenodo.17350087)

---

### 📘 Overview
This repository contains all R scripts, datasets, and example media used to reproduce the analyses presented in the manuscript:

> *In the Tracks of a Whale: Inferring Size Class, Orientation, and Swimming Speed from Thermal Flukeprints.*

The analyses include:
- **Size analysis** — estimating size class of individuals from flukeprints  
- **Speed analysis** — estimating swimming speed of individuals from flukeprints   
- **Orientation analysis** — estimating headings of individuals from flukeprints   

---

### 📂 Repository Structure
```
scripts/
    size_analysis.R
    speed_analysis.R
    orientation_analysis.R
data/
    size_data.txt
    speed_data.txt
    orientation_data.txt
example_video/
    TIR_RGB_example.mp4
```

---

### 🚀 Getting Started
1. Clone the repository:
   ```bash
   git clone https://github.com/USERNAME/flukeprint-monitoring.git
   ```
2. Open an R session (≥4.2)
3. Install required packages:
   ```R
    install.packages(c("tidyverse",  "dplyr",  "ggplot2",  "circular",  "CircStats",  "tidyr",  "lme4",  "irr",  "lmtest",  "merTools",  "purrr",  "lubridate",  "caret",  "pROC"))
   ```
4. Run the desired script (e.g., `size_analysis.R`)  
   Each script automatically loads its dataset and reproduces the figures reported in the paper.

---

### 📊 Example Outputs
- **Figure 4A:** Flukeprint width vs body length  
- **Figure 5A:** Spacing–speed relationship  
- **Figure 6A:** Orientation agreement between TIR and RGB imagery  

Example synchronized RGB–TIR video frames can be accessed in `/example_video`.

---
### 🎥 Example Video
A short example video demonstrating data acquisition is available in the Zenodo archive under “Files” (same DOI).
If viewing directly from GitHub, you can access it here:
> 👉 Download [[example_video.mp4](https://zenodo.org/records/17357562/files/TIR_RGB_example.mp4?download=1)] via Zenodo

---
### 📝 Citation
If you use any materials from this repository, please cite:

> Laporte-Devylder, *et al.* (2026). *In the Tracks of a Whale: Inferring Size Class, Orientation, and Swimming Speed from Thermal Flukeprints.* Dataset and analysis scripts. Zenodo.  
>[![DOI](https://zenodo.org/badge/1075426076.svg)](https://doi.org/10.5281/zenodo.17350087)

*(This is the **concept DOI**, which always points to the latest archived version.)*

Once the article is published, this README will be updated to include the journal citation and paper DOI.

---

### 📜 License
This repository uses a dual license:

- **Code:** MIT License — free to use, modify, and distribute with attribution.  
- **Data and Media:** Creative Commons Attribution 4.0 International (CC BY 4.0).  

You are free to reuse and adapt the materials for research and educational purposes, provided proper credit is given to the original authors.

---

### ⚓ Contact
**[Lucie Laporte-Devylder]**  
[WildDrone - University of Southern Denmark]  
📧 [lucie@biology.sdu.dk] ; [lucie.lprt@gmail.com]   
🌐 [https://www.linkedin.com/in/lucie-laporte-devylder/]
🌐 [https://www.researchgate.net/profile/Lucie-Laporte-Devylder]

---
