#!/usr/bin/env zsh

ropoemsdb="./db"

main() {
    mkdir -p $ropoemsdb

#    parse_cerculpoetilor
#    parse_romanianvoice
    parse_versuri-si-creatii
}

replace_diacritics() {
    echo "$@" | awk '{ gsub(/\342/, "a"); \
                       gsub(/\343/, "a"); \
                       gsub(/\302/, "A"); \
                       gsub(/\303/, "A"); \
                       gsub(/\356/, "i"); \
                       gsub(/\316/, "I"); \
                       gsub(/\252/, "S"); \
                       gsub(/\272/, "s"); \
                       gsub(/\336/, "T"); \
                       gsub(/\376/, "t");}1'
}

save_poem() {
    local _title=$1
    local _author=$2
    local _poem=$3

    # Nothing to do with incomplete info.
    if [[ -z "${_title// }" ]] || [[ -z "${_author// }" ]] || [[ -z "${_poem// }" ]]; then
        return
    fi
    
    local author_folder=$(replace_diacritics $ropoemsdb/$_author)
    local poem_file=$(replace_diacritics $author_folder/$_title)
    poem_file=$(echo $poem_file | awk '{gsub("?", ""); gsub("!", "")}1')
    
    mkdir -p $author_folder
    # Poem already exists in the database.
    if [[ -f $poem_file ]]; then
        continue
    fi
    replace_diacritics "$_poem" > $poem_file
}

# wget --mirror http://cerculpoetilor.net/
parse_cerculpoetilor() {
    for file in ./cerculpoetilor.net/*.html; do
        # The poem title and author are contained in the html file name.
        # The poem title comes first, separated by an underscore.
        file_name=${$(basename $file)%.*}
        if [[ ! $file_name =~ "_" ]]; then
            continue
        fi
        # Split string at underscore and make an array.
        title_author=("${(@s/_/)file_name}")
        title=$title_author[1]
        author=$title_author[2]
        poem=$(cat $file | hxnormalize -x | hxselect -ci "span.n_text")
        save_poem $title $author $poem
    done
}

# wget --mirror http://romanianvoice.com/poezii/
parse_romanianvoice() {
    for file in ./romanianvoice.com/poezii/poezii/*.php; do
        title=$(cat $file | grep "\+1" | hxselect -ci 'font' | awk '{gsub(" ", "-")}1')
        # Author can be found in at least two different html tags.
        author=$(cat $file | grep middletoplink | hxselect -ci 'a')
        if [[ -z "${author// }" ]]; then
            author=$(cat $file | grep "\+2" | hxselect -ci 'font')
        fi
        author=${author// /-}
        # remove all lines starting with html tag, remove remaining br tags,
        # remove duplicate empty lines
        poem=$(cat $file | sed -e '/^</d' | sed 's/<br>//g' | sed '/^$/N;/^\n$/D')
        save_poem $title $author $poem
    done
}

parse_versuri-si-creatii() {
    for file in ./versuri-si-creatii.ro/www.versuri-si-creatii.ro/poezii/**/*.html; do
        author_title=$(cat $file | grep h1 | hxselect -c 'h1')
        if [[ $author_title =~ "Poezii" ]]; then
            # Generic page, not a poem.
            continue
        fi
        author_title=("${(@s/-/)author_title}")
        author=$author_title[1]
        title=$author_title[2]
        echo $author   $title
        poem=$(cat $file | hxnormalize -x | hxselect 'font' |   \
                   awk '{gsub("</font><font.*", "</font>")}1' | \
                   hxnormalize -x | hxselect -c 'font' | awk '{gsub("<br/>", "")}1')
        echo $poem

    done
}

main
