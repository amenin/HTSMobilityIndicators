# mobility_indicators

R scripts for generating mobility indicators from HTS data (France model)

Necessary files:
- personne, deplacement, menage and trajet files from the HTS
- a file variables_ref.csv with the corresponding variable name in the above files, describing the following (next the example for grenoble):
  - for all tables:
    - the pulling sector (secteur de tirage - TIRA)
    - the sample id (Numéro d'échantillon - ECH)
    - zone fine de residence (DP2, MP3, PP2, respectively)
  - deplacement table
    - the person id (numéro de personne - P0)
    - the displacement number (numéro du déplacement - D1)
    - the depart and arrival motivations (motif à l'origine de la personne - D2, motif à la destination de la personne - D5)
    - the origin and destination sectors (zone origine du déplacement - D3, zone destination du déplacement - D7)
    - the depart and arrival hour and minute (heure du départ - D4A, minute du départ - D4B, heure d'arrivée - D8A, minute d'arrivée - D8B)
  - personne table:
    - the person id (numéro de personne - P0)
  - menage table:
    - the redressement coefficient (coefficient de redressement - COEm)
  - trajet table:
    - the trip number (numéro du trajet - T1)
    - the mode of transport (mode utilisé - T3)
    
- a semantic_ref.csv that describes each possible answer to the following semantic variables:
  - sex (P2)
  - age (P4)
  - occupation (P9)
  - freedom to choose the working hours (P9c)
  - working shift (P9d)
  - pcs (P11)
  - use frequency of mode of transport (P19, P20, P21, P22, P23b, P23c, P24, P24b, P24c) (verify this information in the questions sheet)
  
- a reference file for the activities (activity_ref.csv), resuming the existing activities into the following categories:
  - home, work, studying, shopping, leisure, other
  
- a reference file for modes of transport (modes_ref.csv), resuming the existing modes into the following categories:
  - car, walk, bike, pts (Public Transportation System), other
  
- the shape files for fines zones and every other desired territorial partition
  - required columns: code, name, geometry
  
- a reference file for the territorial partitions
  - required columns: OD (origin and destination codes), DTIR (codes for the pulling sectors)
  - other columns can be included for different partitions
  - the scripts can be feed with whatever column you find useful, but the names should be given in the partitions array
  - however, so far, eSTIMe is only prepared to receive DTIR, D30 (a smaller partition), and D10 (an even smaller partition)
  
- from the HTS, 4 files are expected:
  - deplacement.sav
  - menage.sav
  - personne.sav
  - trajet.sav
  In case the data each of these files is originally split in two, like the Lyon dataset (face à face and telephone), you can use the bind_ori_files.r script to bind them together
  
How to use the files:

1. create the reference files and transform them into rds using the csv_to_rds.r script
  - run on Rscript csv_to_rds.r -paths path_to_csv_filt output_path_to_rds_file
2. Classify the trajectories, since the classes are later used for computing the indicators (see readme inside traminer folder for more information)
3. Prepare the raw data for the indicators
  - run prepare_data.r
4. Generate the files for each indicator 
  - run indicators.r
  - if you don't have the trajectories classes yet: Rscript indicators.r -class FALSE
