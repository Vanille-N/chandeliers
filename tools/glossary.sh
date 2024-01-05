#! /usr/bin/env bash

# Generate a glossary for the project.
# Pass as argument the target file (markdown, but don't write the extension).

id() { tee; }

rg --only-matching --no-heading --no-line-number '[A-Za-z][a-z]*' -- **/src/*.rs |
    sort |
    tee `# path/to/file:Word` |
    tr ':' ' ' |
    tr 'A-Z' 'a-z' |
    tee `# path/to/file word` |
    sort --stable --field-separator=' ' --key=2,2 |
    uniq --count |
    tee `# n path/to/file word` |
    awk '{ print $3 " " $1 " " $2 }' |
    tee `# word n path/to/file` |
    sort --stable --field-separator=' ' --key=1,1 --key=2,2nr `# Secondary sort on the numerical count` |
    (
        echo "# Glossary"
        # Now we have them grouped by word and sorted by count.
        # We then merge adjacent lines with the same word.
        CUR=''
        while read word count file; do
            if [[ $word != $CUR ]]; then
                echo "- $word";
                CUR=$word
            fi
            echo "  [$count]($file)";
        done
    ) > $1.md
