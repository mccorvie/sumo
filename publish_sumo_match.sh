#!/bin/bash

# Exit on error
set -e

echo "================================================"
echo "Starting sumo analysis pipeline"
echo "Started at: $(date)"
echo "================================================"

echo ""
echo "[1/3] Running elo_history.R..."
export SUMO_CACHE="1"

Rscript elo_history.R

echo ""
echo "[2/3] Rendering sumo_matches.qmd..."
quarto render sumo_matches.qmd

echo ""
echo "[3/3] Opening rendered HTML in browser..."
# Open in browser 
if command -v open &> /dev/null; then
    # macOS
    open sumo_matches.html
else
    echo "Could not automatically open browser. Please open sumo_matches.html manually."
fi

echo ""
echo "================================================"
read -p "Copy results to website? (y/n): " -n 1 -r
echo ""

if [[ $REPLY =~ ^[Yy]$ ]]; then
    DEST_DIR=~/website/martingale.ai/public_html
    
    echo "Copying files to $DEST_DIR..."
    
    # Remove old files if they exist
    if [ -f "$DEST_DIR/sumo_matches.html" ]; then
        echo "Removing old sumo_matches.html..."
        rm "$DEST_DIR/sumo_matches.html"
    fi
    
    if [ -d "$DEST_DIR/sumo_matches_files" ]; then
        echo "Removing old sumo_matches_files directory..."
        rm -rf "$DEST_DIR/sumo_matches_files"
    fi
    
    # Copy new files
    echo "Copying sumo_matches.html..."
    cp sumo_matches.html "$DEST_DIR/"
    
    if [ -d "sumo_matches_files" ]; then
        echo "Copying sumo_matches_files directory..."
        cp -r sumo_matches_files "$DEST_DIR/"
    else
        echo "Warning: sumo_matches_files directory not found, skipping..."
    fi
    
    echo "✓ Files copied successfully!"

    echo "Publishing to martingale.ai"

    cd ~/website/martingale.ai
    . web_publish.sh

    echo "✓ Published to external website"

else
    echo "Copy cancelled."
fi

echo ""
echo "================================================"
echo "Pipeline completed at: $(date)"
echo "================================================"
