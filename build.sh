#!/bin/sh

set -e



## MAKE PAGE HTML


function makePageHtml {
  cat <<EOF > $1
<!DOCTYPE HTML>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>$2</title>
    <link href="https://fonts.googleapis.com/css?family=Roboto:100" rel="stylesheet"> 
    <link rel="stylesheet" href="index.css">
</head>
<body>
    <script type="text/javascript">
    $(cat $3)
    var app = Elm.Main.init();
    </script>
</body>
</html>
EOF

}



## SET PATH


if ! [ -x "$(command -v uglifyjs)" ]
then
  PATH=$(pwd)/node_modules/uglify-js/bin:$PATH
fi



## GENERATE HTML


mkdir -p docs
mkdir -p _temp

## pages

echo "PAGES"
for elm in $(find pages -type f -name "*.elm")
do
    subpath="${elm#pages/}"
    name="${subpath%.elm}"
    js="_temp/$name.js"
    html="docs/$name.html"

    if [ -f $html ] && [ $(date -r $elm +%s) -le $(date -r $html +%s) ]
    then
        echo "Cached: $elm"
    else
        echo "Compiling: $elm"
        mkdir -p $(dirname $js)
        mkdir -p $(dirname $html)
        rm -f elm-stuff/*/Main.elm*
        elm make $elm --optimize --output=$js > /dev/null
        uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
          | uglifyjs --mangle \
          | makePageHtml $html $name
    fi
done


## REMOVE TEMP FILES

rm -rf _temp
