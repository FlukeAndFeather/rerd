#!/bin/zsh -i

# Parameters
XMIN=-180
XMAX=180
YMIN=-90
YMAX=90
TMIN=2021-06-29T00:00:00
TMAX=2021-06-30T00:00:00
ZMIN=0
ZMAX=3000

# Download data (note: this is interactive)
conda activate cmems
copernicusmarine subset \
    --dataset-id cmems_mod_glo_phy_my_0.083deg_P1D-m \
    --dataset-version 202311 \
    --variable bottomT --variable mlotst --variable siconc --variable sithick \
    --variable so --variable thetao --variable uo --variable usi --variable vo \
    --variable vsi --variable zos \
    --start-datetime $TMIN \
    --end-datetime $TMAX \
    --minimum-longitude $XMIN \
    --maximum-longitude $XMAX \
    --minimum-latitude $YMIN \
    --maximum-latitude $YMAX \
    --minimum-depth $ZMIN \
    --maximum-depth $ZMAX
