#n extractor.sed: in the error output: extracts the file name, text and line
# number in {[error] path: $LINENUMBER: $TEXT

s/ *found:.*//g
s/ *expected:.*//g

/\[error\] / {
s/.*\/\(.*\.neve\):/\1,/
s/ \([0-9][0-9]*\):/ \1,/
s/ \(.*\)/ \1/
p
}
