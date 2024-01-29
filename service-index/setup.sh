
ldd_output=$(ldd ./bin/plutus-chain-index)

paths=$(echo "$ldd_output" | awk -F '/' '/=> \/nix\/store\// {print $4}')

destination="$(pwd)/tmp"

for folder in $paths; do
    source_path="/nix/store/$folder"
    sudo cp -r "$source_path" "$destination"
    echo "Copied: $folder"
done
