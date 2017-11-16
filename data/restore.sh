#!/usr/bin/bash
find . -type f -name "*.bak" -exec rename .txt.bak .txt {} \;
echo "restored .bak files"
