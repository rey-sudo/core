WORKING_DIR=$(pwd)

rm ./product/images/transparent/*

rm ./remove-background/setup/files/*
rm ./remove-background/setup/output/*

cp ./product/images/* ./remove-background/setup/files

cd remove-background/setup

npm run start

cd "$WORKING_DIR"

cp ./remove-background/setup/output/* ./product/images/transparent
