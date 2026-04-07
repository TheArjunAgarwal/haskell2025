#!/usr/bin/env bash

set -euo pipefail

# CSV header
echo "author,file,fractional_word_count"

# Loop through all tracked files
git ls-files | while read -r file; do
    # Skip binary files and empty files
    if file "$file" | grep -q 'text' && [ -s "$file" ]; then
        
        # 1. Get total word count for the file
        total_words=$(wc -w < "$file")
        
        # 2. Get total line count for the file
        total_lines=$(wc -l < "$file")
        
        # Avoid division by zero if file is just newlines
        if [ "$total_lines" -eq 0 ]; then continue; fi

        # 3. Use git blame to count how many lines each author owns
        # 'sort | uniq -c' gives us: "count author_name"
        git blame --line-porcelain "$file" | grep "^author " | sed 's/^author //' | sort | uniq -c | while read -r line_count author; do
            
            # 4. Calculate fractional share
            # (Author Lines / Total Lines) * Total Words
            # We use 'bc' for floating point math
            fractional_count=$(echo "scale=2; ($line_count / $total_lines) * $total_words" | bc)

            echo "$author,$file,$fractional_count"
        done
    fi
done