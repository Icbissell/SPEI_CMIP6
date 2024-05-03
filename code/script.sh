cd /Users/icbissell/Documents/research/thesis/tasmax/ssp585/BCC-CSM2-MR

# Loop through all .nc files
for file in *.nc; do
    # Check if the file is a regular file
    if [[ -f "$file" ]]; then
        # Extract the file name without extension
        filename="${file%.*}"
        # Run the command with the respective file name
        ncks --mk_rec_dmn time "$file" "${filename}_ok.nc"
    fi
done

# Store the first two letters of the first .nc file in a variable
prefix=$(ls *.nc | head -n 1 | cut -d '_' -f 1)

# Run the ncrcat command to concatenate all .nc files into a single output file
ncrcat *_ok.nc "combo_${prefix}.nc"

#remove temp files
mv *_ok.nc ~/.Trash