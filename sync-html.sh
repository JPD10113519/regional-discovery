#!/bin/bash

# Find all HTML files, excluding tmp and logs directories
# Copy them to docs/ with a flattened structure
find . -name "*.html" \
  -not -path "*/tmp/*" \
  -not -path "*/logs/*" \
  -not -path "./docs/*" \
  -not -path "./.git/*" \
  -exec cp {} docs/ \;

# Find and copy matching folders (same name as HTML files without .html)
find . -name "*.html" \
  -not -path "*/tmp/*" \
  -not -path "*/logs/*" \
  -not -path "./docs/*" \
  -not -path "./.git/*" | while read htmlfile; do
    # Get the base name without .html extension
    basename="${htmlfile%.html}"
    # Check if a folder with that name exists
    if [ -d "${basename}_files" ]; then
        # Copy the folder to docs
        cp -r "${basename}_files" docs/
    fi
done

# Create an index page
cat > docs/index.html << 'EOF'
<!DOCTYPE html>
<html>
<head>
    <title>Research Reports</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        ul { list-style-type: none; }
        li { margin: 10px 0; }
    </style>
</head>
<body>
    <h1>Research Reports</h1>
    <ul>
EOF

# Add links to all HTML files (except index.html)
for file in docs/*.html; do
    if [ "$(basename "$file")" != "index.html" ]; then
        echo "        <li><a href=\"$(basename "$file")\">$(basename "$file")</a></li>" >> docs/index.html
    fi
done

cat >> docs/index.html << 'EOF'
    </ul>
    <p><em>Last updated: $(date)</em></p>
</body>
</html>
EOF

echo "✓ HTML files and folders synced to docs/"
