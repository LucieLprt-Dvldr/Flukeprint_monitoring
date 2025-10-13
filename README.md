# UAV-based Thermal Flukeprint Imagery for Non-Invasive Monitoring of Humpback Whales

**In the Tracks of a Whale: Thermal Flukeprints Inform on Size and Movement Dynamics.**

[![R](https://img.shields.io/badge/R-4.2%2B-blue)]()
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)]()
[![DOI](https://zenodo.org/badge/DOI_PLACEHOLDER.svg)](DOI_LINK_PLACEHOLDER)

---

### 📘 Overview
This repository contains all R scripts, datasets, and example media used to reproduce the analyses presented in the manuscript:

> *In the Tracks of a Whale: Thermal Flukeprints Inform on Size and Movement Dynamics.*

The analyses include:
- **Size analysis** — estimating size of individuals from flukeprints  
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
   install.packages(c("tidyverse", "lme4", "ggplot2", "irr", "performance", "Metrics", "psych"))
   ```
4. Run the desired script (e.g., `size_analysis.R`)  
   Each script automatically loads its dataset and reproduces the figures reported in the paper.

---

### 📊 Example Outputs
- **Figure 4A:** Flukeprint width vs body length  
- **Figure 5A:** Spacing–speed relationship  
- **Figure 6A:** Orientation agreement between TIR and RGB imagery  

Example synchronized RGB–TIR video frames are available in `/example_video`.

---

### 🧠 Citation
If you use this repository, please cite:

> [Authors]. (2025). *TITLE OF PAPER HERE.* [Journal Name]. DOI: [insert DOI here]

---

### 📜 License
This repository uses a dual license:

- **Code:** MIT License — free to use, modify, and distribute with attribution.  
- **Data and Media:** Creative Commons Attribution 4.0 International (CC BY 4.0).  

You are free to reuse and adapt the materials for research and educational purposes, provided proper credit is given to the original authors.

---

### 📫 Contact
**[Lucie Laporte-Devylder]**  
[WildDrone - University of Southern Denmark]  
📧 [lucie@biology.sdu.dk] ; [lucie.lprt@gmail.com]   
🌐 [https://www.linkedin.com/in/lucie-laporte-devylder/]
🌐 [https://www.researchgate.net/profile/Lucie-Laporte-Devylder]

---
