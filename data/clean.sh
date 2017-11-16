#!/usr/bin/bash
find . -type f -name "*.txt" -print0 | xargs -0 sed -i.bak 's/\s*$//'
echo "got rid whitespace at each line's end"
echo "backups are listed as .bak"
