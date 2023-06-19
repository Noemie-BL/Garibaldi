## Quadrat Analysis

### Description

Analysis code for estimating plant coverage in quadrats located around Black Tusk and Taylor Meadows in Garibaldi Provincial Park. Due to file size, these images are not hosted on this GitHub page.

Contributors: 
Carly Hilbert (chilbe02@student.ubc.ca) - preliminary photo cropping and collation
Allen Zhao (allen10to11@gmail.com) - Image analysis code.

Last edited: 18 June 2023

### Data collection - Who, when, where, why and how the data were collected

Dates: July-August 2022

Location: Garibaldi Provincial Park on Taylor Meadow (TM), Black Tusk (BT), and Panorama Ridge (PR) trails.

Samplers: Nathalie Chardon, Carly Hilbert, Mackenzie Urquhart-Cronish, Brianna Ragsdale, Teagan MacLachlan, Vickie Lee, Christian Lauber, Carolyn Chong

Data entered by: Carly Hilbert

Methods: In the summer of 2022, we established long-term transects near the major trails in the park (Taylor Meadows, Black Tusk, Panorama Ridge) to quantify the effects of trampling by recreational users. To address how trampling affects charismatic plant communities (blueberry, heather and sedge meadows) along elevational gradients, we chose sites at multiple elevations per trail. We established transects directly adjacent to the trail and at least a 5 m perpendicular distance away from the trail (control) to compare the effects of trampling on the same vegetation types. 

Plant communities. We recorded landscape characteristics (slope, aspect, latitude, longitude, presence of trees) for each transect and used 0.5m x 1m quadrats to record height, maximum diameter, and bud/flower/fruit counts of our focal plant species (Vaccinium ovalifolium, Cassiope mertensiana, Phyllodoce empetriformis, Phyllodoce grandiflora, Carex sp.). Because we only found Phyllodoce grandiflora at one site, we did not use this species in our analyses. Please see N. Chardon’s recent work on human trampling for greater detail on this sampling approach: 
https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.13384
https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.4276

We also used a standardized approach to photograph each quadrat, and are used these images to calculate how vegetation greenness is impacted by human trampling. We calcluated plant percent cover with a custom algorithm. 

Microbial communities. We selected representative transects per trail to study microbial activity. As a proxy for measuring litter decomposition by microbes, we buried tea bags at these transects to undergo microbial decomposition for one year.

Upcoming work. In Summer 2023, we will return to each transect to re-survey transects for species diversity. We will dig out the buried teabags, and bury new ones to decompose for another year. The species diversity and microbial data are not currently used in analyses.

Goals: We aim to quantify the continued effects of human trampling on these plant and microbial communities by returning each year, thus generating a multi year dataset. Such a dataset will allow us to answer how these plant communities are responding to continuous trampling and ambient warming. We will incorporate BC Parks data on visitation numbers in our analyses to answer how strongly yearly visitation rates correlate with community responses, or if these responses show more of a lag response to past visitation rates. 

### Data files

#### compiled_data
_FILENAME <description of file contents, date modified, whether mid or final version>_

quad.RData: gps & transect data matched to quad data; created in merge_fielddata.R; modifed on 25 Apr 2023; final version; variables described in merge_fielddata.R 

trans_ALL.RData: all transect field data, gps, and altitude; created on 15 Nov 2022; final version; variables described in merge_fielddata.R 

P_albicaulis_Garibaldi_Aug2022.csv: Pinus albicaulis locations for BC Rangers; emailed data to Kym Welstead (Kym.Welstead@gov.bc.ca) on 7 Dec 2022

plant-percent-cover.csv: Calculated plant percent cover values for each quadrat (Allen Zhao, allen10to11@gmail.com); created on 27 March 2023; final version; analyses in Quadrat_Analysis folder

#### scripts
_SCRIPTNAME <description of script purpose, coding language, versions of software/libraries used>_



#### Quadrat_Analysis

****Allen fill in info here****

Dependencies:

* Numpy
* Matplotlib (for optional coverage visualizer)
* exifread
