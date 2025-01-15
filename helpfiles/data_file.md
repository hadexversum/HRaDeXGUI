## Accepted data file

The required data format is the `cluster` format produced by DynamX, Waters.

Therefore, the data file should be in csv format with required columns:

| Column name | Type |
| ----------- | ----------- |
| Protein | chr |
| Start | int |
| End | int | 
| Sequence | chr |
| Modification | logi / chr |
| Fragment | logi / chr |
| MaxUptake | num |
| MHP | num |
| State | chr |
| Exposure | num | 
| File | chr |
| z | int |
| RT | num |
| Inten | num |
| Center | num |

Please make sure that there is no empty line on the bottom of the file, as it affects parsing of the file.

Example file is available to check [here](https://github.com/hadexversum/HaDeX/blob/master/inst/HaDeX/data/KD_190304_Nucb2_EDTA_CaCl2_test02_clusterdata.csv).

*In the new version other data formats will be accepted.*
