#!/bin/bash

# Script to count #excercises in .typ files
# Usage: ./count_exercises.sh [directory]

# Set default directory to current directory if no argument provided
DIR="${1:-.}"

# Check if directory exists
if [ ! -d "$DIR" ]; then
    echo "Error: Directory '$DIR' does not exist."
    exit 1
fi

# Initialize counters
total_count=0
file_count=0

echo "Counting #excercises in .typ files in directory: $DIR"
echo "=================================================="

# Find all .typ files and process them
while IFS= read -r -d '' file; do
    # Count occurrences of #excercises in the file (case-insensitive)
    count=$(grep -i "#exercise" "$file" | wc -l)
    
    if [ "$count" -gt 0 ]; then
        echo "$(basename "$file"): $count"
        total_count=$((total_count + count))
        file_count=$((file_count + 1))
    fi
done < <(find "$DIR" -name "*.typ" -type f -print0)

echo "=================================================="
echo "Files with #excercises: $file_count"
echo "Total #excercises found: $total_count"

# If no .typ files found
if [ "$file_count" -eq 0 ]; then
    typ_files=$(find "$DIR" -name "*.typ" -type f | wc -l)
    if [ "$typ_files" -eq 0 ]; then
        echo "No .typ files found in directory: $DIR"
    else
        echo "No #excercises found in any .typ files"
    fi
fi