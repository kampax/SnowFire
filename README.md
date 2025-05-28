# Assessing postfire burned wood management effects on snow duration in a Mediterranean mountain using remote sensing
Authors: Carlos Javier Navarro; Beatriz P. Cazorla, Martínez-López, Javier; Postma, Thedmer M.; Leverkus, Alexandro B.; Alcaraz-Segura, Domingo; Castro, Jorge 
## Description

This repository includes information that was used to study the occurrence of snow before and after a forest fire (Lanjaron in the southeast of Sierra Nevada, Spain) in plots where different post-fire treatments were applied.

## Are included:

### 1) Creating a Landsat grid, the filtering of the pixels and their association with a treatment
The [creation of a grid](https://github.com/kampax/SnowFire/blob/main/Scripts%20GEE/Grid%20creation) where each cell has the size of a Lansat pixel was carried out using the ee.Geometry.Polygon.coveringGrid function available in [Google Earth Engine](https://developers.google.com/earth-engine/apidocs/ee-geometry-polygon-coveringgrid)

Then in Qgis values were assigned for each cell based on the treatment (NI, PCL and SL). There are some pixels that are border between two treatments so it is ideal not to consider them. For this reason, a new column called suit was created that has values from 1 to 4 where each code corresponds to a usability level of l pixel. In this coding suit=1 refers to pixels that are present for a single treatment, 2 = pixels with dead trees, 3 = pixels with rocks or other type of cover that was not affected by the fire and 4 = edge pixels between two treatments. Then, in the analysis to quantify the occurrence of snow, only pixels that had values of suit=1 were taken into account.

### 2) Calculation of _NDSI_ and extraction of values for each scene
In this step, the Normalized Difference Snow Index (NDSI) values are calculated from the [Landsat 5 Surface Reflectance collection](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT05_C02_T1_L2) and  [Landsat 7 Surface Reflectance collection](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT07_C02_T1_L2) images available in Google Eath Engine. 
This index is a normalized difference index between the green bands (Band 2 in the Landsat 5 collection) and the shortwave infrared 1 (Band 5) that is widely used to identify snow.

To facilitate comparison between treatments, all scenes that had cloud cover in the area of the plots (which are confused with snow or mask its value) were excluded. With this dataset, the values were extracted for each cell of the grid created previously. Thus, this resulting grid has information on an ID, the treatment it represents and NDSI values for each scene.

### 3) Extraction of values of ancillary layers for the area
Additionally, we calculate the values of some ancillary layers that can influence the occurrence of snow such as elevation, slope, shadows, etc., which will later be included in the models.

### 4) Snow percentage calculation at pixel level
Since the idea is to compare the treatments in the periods before and after the fire, we first filter and are left only with the scenes that had snow. Subsequently, using a threshold of 0.35, we created binary layers of each scene where the value of 1 represents the occurrence of snow and 0 the absence of snow. We then calculated the percentage of pixels occupied by snow for each of the treatments in each of the scenes.

### 5) Comparison between treatments

In each period, before and after the fire, we evaluated whether there were differences between treatments. We found that in the pre-fire period, there were no significant differences between treatments. However, after the fire, significant differences can be observed between treatments, with the no-intervention (NI) treatment having a higher percentage of snow cover (35% more on average) than the other two treatments, as can be seen in the figure.
![image](https://raw.githubusercontent.com/kampax/SnowFire/refs/heads/main/Scripts%20R/Figures/1_Fire_Pre_Post.jpg) 

![Image](https://github.com/user-attachments/assets/2abaa263-3827-4721-ade6-e491de0ed885)

---
**Article Citation**
If you use these codes in your research, please cite the reference article:

> [Authors]. (2025). *Assessing postfire burned wood management effects on snow duration in a Mediterranean mountain using remote sensing*. [Unpublished manuscript].

---
© 2025 - [Carlos Javier Navarro / IISTA-CEAMA-UGR]
