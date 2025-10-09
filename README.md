# Burnt wood removal reduces snow persistence after wildfire –a remote-sensing assessment
**Authors:** P. Cazorla, Beatriz¹'⁴*, Navarro, Carlos Javier¹*;  Martínez-López, Javier¹'²'⁴; Postma, Thedmer M.¹; Leverkus, Alexandro B.¹'²; Alcaraz-Segura, Domingo¹'³'⁴ ; Castro, Jorge²

¹ Andalusian Institute for Earth System Research IISTA-CEAMA, Spain 
² Ecology Department, Faculty of Sciences, University of Granada, Spain 
³ Botany Department, Faculty of Sciences, University of Granada, Spain
⁴ Andalusian Center for Global Change - Hermelindo Castro (engloba), University of Almería, Spain


## Description

This repository contains information used to study snow occurrence before and after a forest fire (Lanjarón in the southeast of Sierra Nevada, Spain) in plots where different post-fire treatments were applied.

## This repository includes:

### 1) Creating a Landsat grid, filtering pixels and their association with treatments
The [creation of a grid](https://github.com/kampax/SnowFire/blob/main/Scripts%20GEE/Grid%20creation) where each cell has the size of a Landsat pixel was carried out using the ee.Geometry.Polygon.coveringGrid function available in [Google Earth Engine](https://developers.google.com/earth-engine/apidocs/ee-geometry-polygon-coveringgrid).

Then, in QGIS, values were assigned to each cell based on the treatment (NI, PCL, and SL). Some pixels are located at the border between two treatments, so it is ideal not to consider them. For this reason, a new column called "suit" was created with values from 1 to 4, where each code corresponds to a usability level of the pixel. In this coding, suit=1 refers to pixels that belong to a single treatment, 2 = pixels with dead trees, 3 = pixels with rocks or other types of cover that were not affected by the fire, and 4 = edge pixels between two treatments. In the analysis to quantify snow occurrence, only pixels with suit=1 values were taken into account.

### 2) Calculation of _NDSI_ and extraction of values for each scene
In this step, the Normalized Difference Snow Index (NDSI) values [are calculated](NDSI%Calculation) from the [Landsat 5 Surface Reflectance collection](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT05_C02_T1_L2) and [Landsat 7 Surface Reflectance collection](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT07_C02_T1_L2) images available in Google Earth Engine. 
This index is a normalized difference index between the green band (Band 2 in the Landsat 5 collection) and the shortwave infrared 1 band (Band 5) that is widely used to identify snow.

To facilitate comparison between treatments, all scenes with cloud cover in the plot area (which can be confused with snow or mask its value) were excluded. For this purpose, since this was a local study and we were interested in ensuring no cloud cover, we performed a visual inspection using a [Google Earth Engine app](https://ee-carlosnavarro.projects.earthengine.app/view/landsat-cloud-selector) designed for this purpose. Once we identified scenes with clouds over the study area, we removed them before calculating the NDSI.

With this dataset, the values were extracted for each cell of the previously created grid. Thus, the resulting grid contains information on an ID, the treatment it represents, and NDSI values for each scene.

### 3) Extraction of values from ancillary layers for the area
Additionally, [we calculated the values of ancillary layers](https://github.com/kampax/SnowFire/blob/main/Scripts%20GEE/Ancillary%data%extraction) that can influence snow occurrence, such as elevation, slope, shadows, etc., which were later included in the models.

### 4) Snow percentage calculation at pixel level
Since the objective was to compare treatments in the periods before and after the fire, we first filtered to retain only scenes with snow. Subsequently, using a threshold of 0.4, we created binary layers for each scene where a value of 1 represents snow occurrence and 0 represents snow absence. We then calculated the percentage of pixels occupied by snow for each treatment and period.

### 5) Comparison between treatments
Post-fire management treatments were established between March and May 2006, i.e., 6–8 months after the wildfire. These treatments were salvage logging (SL), partial cut plus lopping (PCL), and non-intervention (NI).

In each period, before and after the fire, [we evaluated whether there were differences between treatments](https://raw.githubusercontent.com/kampax/SnowFire/refs/heads/main/Scripts%20R/Analysis%Lanjaron.R). We found that in the pre-fire period, there were no significant differences between treatments. However, after the fire, significant differences were observed between treatments, with the non-intervention (NI) treatment having a higher percentage of snow cover (35% more on average) than the other two treatments, as shown in the figure. 

![image](https://raw.githubusercontent.com/kampax/SnowFire/refs/heads/main/Scripts%20R/Figures/Snow_Pixel_Level.jpg) 

When examining the percentage values of snow occurrence in the post-fire period, we observed that the non-intervention treatments showed a higher percentage of snow occurrence, especially compared to the salvage logging treatment.
![image2](https://raw.githubusercontent.com/kampax/SnowFire/refs/heads/main/Scripts%20R/Figures/PixelChart_postfire.png)

---
**Article Citation**
If you use these codes in your research, please cite the reference article:

> Cazorla, P., Beatriz¹'⁴*, Navarro, C.J.¹*, Martínez-López, J.¹'²'⁴, Postma, T.M.¹, Leverkus, A.B.¹'², Alcaraz-Segura, D.¹'³'⁴, Castro, J.² (2025). *Burnt wood removal reduces snow persistence after wildfire –a remote-sensing assessment*. [Unpublished manuscript].

---
© 2025 - [Carlos Javier Navarro / IISTA-CEAMA-UGR]
