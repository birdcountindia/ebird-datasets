# Working with eBird datasets

This concerns the Indian eBirding community and data. The repo has code mainly for importing, morphing and exporting various datasets for various usecases, storing them in appropriate forms for further use. It also contains a master script that automates many of Bird Count India's regular (monthly) data tasks.

## Automated BCI data tasks

**One master R script linked to a master batch file to run several monthly data tasks in one go: `master-monthly-script.R`**

*If latest users data is available, the group accounts list should be updated before running the automated master monthly script.*

## `group-accounts/`

The objective here is to create a list of group accounts present in the eBird dataset from India (accounts not used by an individual, instead created in order to aggregate data from certain birding events or groups), and to regularly update this. This is being maintained by Bird Count India in order to ensure that analyses being performed using eBird public data are unbiased, particularly those involving birding- and birder-related statistics and information. Since many of these analyses assume that one individual birder is the source of the data, group accounts such as those listed here, which aggregate observations from several observers, exaggerate many patterns and hence should be excluded.

For the current purposes, group accounts are defined at two levels: 
  1. (GA1) Those additional accounts with which existing eBirders (individuals) share their lists to track certain aggregate summaries. (To be removed for both bird- and birder-related analyses.) 
  2. (GA2) Those accounts aside from the above, which are used to bring sightings on board that might not otherwise be on eBird. Lists from such accounts are usually not shared with any personal/individual observer ID, so their exclusion will result in reduced checklist counts. Such accounts include, i.a., rarity, historical and young (school)children accounts. (To be removed for only birder-related analyses.)
 
The code produces a list of likely accounts obtained by filtering using keywords in R, which is saved into a draft `.csv` (file name ending with `_0.csv`). The group account classification metadata (GA1 or GA2) are then added manually, and this is finally saved as a `.csv`.

