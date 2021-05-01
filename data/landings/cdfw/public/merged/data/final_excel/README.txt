This README.txt file was generated on 2021-03-10 by CHRISTOPHER FREE

GENERAL INFORMATION

1. Title of Dataset: The CALFISH database: open-sourcing a century of California’s non-confidential fisheries landings and participation data

2. Author Information
	A. Principal Investigator Contact Information
		Name: Christopher Free
		Institution: University of California, Santa Barbara
		Address: Bren School of Environmental Science and Management, University of California, Santa Barbara, 2400 Bren Hall, Santa Barbara, CA 93106-5131
		Email: cfree14@gmail.com

3. Date of data collection (single date, range, approximate date): 1916-2020 

4. Geographic location of data collection: California coast 

5. Information about funding sources that supported the collection of the data: Funding for this project was provided by the David and Lucile Packard Foundation.


SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: None

2. Links to publications that cite or use the data:

Free CM, Vargas Poulsen C, Bellquist LF. The CALFISH database: open-sourcing a century of California’s non-confidential fisheries landings and participation data. In first review at Fisheries Research.

3. Links to other publicly accessible locations of the data: 

https://github.com/cfree14/wcfish/

4. Links/relationships to ancillary data sets: 

https://www.pfeg.noaa.gov/products/las/CA_market_catch.html

5. Was data derived from another source?

CDFW Landings Series published in the Fish Bulletin and on its website

6. Recommended citation for this dataset: 

Free CM, Vargas Poulsen C, Bellquist LF. The CALFISH database: open-sourcing a century of California’s non-confidential fisheries landings and participation data. In first review at Fisheries Research.


DATA & FILE OVERVIEW

1. File List: 

CDFW_1916_1976_annual_kelp_harvest_by_bed_type.xlsx

CDFW_1916_1999_total_landings_shipments.xlsx

CDFW_1916_2019_annual_kelp_harvest.xlsx

CDFW_1916_2020_n_fishers_by_area_of_residence.xlsx

CDFW_1934_1956_n_comm_vessels_by_port_complex.xlsx

CDFW_1934_1976_n_comm_vessels_by_length_class.xlsx

CDFW_1934_2020_n_comm_vessels.xlsx

CDFW_1936_2019_annual_cpfv_effort_by_port_complex.xlsx

CDFW_1936_2019_landings_by_waters_species.xlsx

CDFW_1941_2019_landings_by_port_species.xlsx

CDFW_1946_2019_annual_cpfv_landings_by_port_complex_species.xlsx


2. Relationship between files, if important: 

3. Additional related data collected that was not included in the current data package: 

4. Are there multiple versions of the dataset? yes/no
	A. If yes, name of file(s) that was updated: 
		i. Why was the file updated? 
		ii. When was the file updated? 


METHODOLOGICAL INFORMATION

1. Description of methods used for collection/generation of data: 

The California Department of Fish and Wildlife (CDFW) has been monitoring statewide fisheries landings and participation since 1916 and releases confidential versions of this data through authorized data requests and non-confidential summaries of this data in its quasi-annual landings reports. The non-confidential data published in the landings reports provide a rich history of California’s fisheries but are scattered across 1000s of tables in 100s of documents, limiting their accessibility to researchers, fishers, and other interested stakeholders. We reviewed the 58 landing series reports published by CDFW from 1928 to 2020 and extracted and curated 13 datasets of long length (years) and wide public interest. In general, these datasets describe landings and participation in commercial fishing and the CPFV sector of recreational fishing (i.e., recreational fishing from private boats and shore are not described in these reports). We rigorously quality controlled all of the extracted data and enhanced the datasets with additional attributes of interest where possible. Notably, these enhancements included harmonizing common names across years and datasets and linking common names with scientific names.

2. Methods for processing the data: 

The landings datasets curated below describe landings in terms of both volume (pounds) and value (dollars). The values reflect nominal ex-vessel values and have not been adjusted for inflation. The volumes are reported “without regard to condition” and reflect the volumes reported on the original landings receipt (i.e., they have not been universally converted to round weights). Although most fish and shellfish are landed in round (whole) condition, some species may be eviscerated (gutted), dressed, or beheaded before being brought ashore, but this is not recorded in the data. This is especially common for barracuda, shark, salmon, sablefish, white seabass, and swordfish. A few market categories do include descriptions of condition (i.e., Pacific herring roe, Pacific herring roe on kelp, Chinook/coho salmon roe, spider/sheep crab claws, and crab claws) but there is no guidance on how to interpret these descriptions. We provide an attribute for condition with four options -- roe, roe on kelp, claws, and not specified -- but caution against using these attributions without further clarification from the state.

The CDFW datasets report landings by market categories that are not always species specific. Furthermore, these market categories are described using common names rather than scientific names. Although a key for relating common and scientific names is provided at the beginning of each Fish Bulletin-hosted landings report, the conventions for common names and alignment with scientific names varies throughout the landings series. We rigorously harmonized common names across years and datasets and associated common names with updated scientific names with guidance from the Fish Bulletin species keys. To ease analysis, maintain transparency, and allow users to make different decisions regarding species identities, every dataset with species-specific information includes the original common name, the harmonized common name, and the updated scientific name. We also provide a key for appending additional taxonomic information (i.e., phylogenetic groups and/or commercial categories) to any of the curated datasets. Overall, the landings data include 397 market categories representing 12 phyla, 25 classes, 68 orders, 130 families, and 200 genera.

Finally, many of the datasets published in the landings series report statistics for individual fishing ports or for groups of fishing ports called “port complexes”. However, the naming conventions for ports and the delineation of port complexes varies throughout the landings series. To ease analysis, we harmonized port and port complex attributes across years and datasets. In most cases, harmonizing port names involved straightforward decisions (e.g., “Bay”, “Bay (Bodega)”, and “Bodega Bay” all refer to Bodega Bay). However, in some cases, nuanced decisions were required. Namely, we decided that references to “Tomales Bay (Marshall)”, “Princeton (Half Moon Bay)”, and “Point Reyes (Drakes Bay)” imply “Tomales Bay & Marshall”, “Princeton & Half Moon Bay”, and “Point Reyes & Drakes Bay”. This decision was based on the fact that, in some years, statistics are separated for these commonly paired ports. We used slashes to denote grouped ports (e.g., “Tomales Bay/Marshall” indicates both Tomales Bay and Marshall together) in the harmonized port names. We retained the original port name in the curated datasets to make our decisions transparent and to allow users to make different decisions. The geographical delineation of port complexes varied throughout the landings series (Figure S1) with: 13 complexes defined by county lines in FB 15-44 (1926-1930), 8 complexes defined by natural landmarks in FB 44-49 (1931-1935), 7 complexes defined by county lines in FB 57-173 (1936-1986), and 9 complexes defined by county lines in FB 181 and the website-hosted reports (1987-2019). We used the recent 9-complex typology in the curated datasets but provide a key to summarize data based on the older typologies. This key also includes the coordinates (lat/long) of each port.

3. Instrument- or software-specific information needed to interpret the data: None

4. Standards and calibration information, if appropriate: None

5. Environmental/experimental conditions: None

6. Describe any quality-assurance procedures performed on the data: See above

7. People involved with sample collection, processing, analysis and/or submission: 

Chris Free
Camila Vargas Poulsen
Lyall Bellquist



