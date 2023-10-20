#!/bin/bash

while true; do
    skaffold dev
    exit_code=$?
    
    if [[ $exit_code -eq 0 ]]; then
        # Skaffold dev succeeded, exit the loop
        break
    else
        # Skaffold dev failed, display error message
        echo "Skaffold dev failed with exit code $exit_code"
        echo "Restarting Skaffold dev..."
    fi
done

