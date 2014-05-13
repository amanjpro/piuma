#n detector.sed: in the test file: extracts the line number and text in 
# {// @fail(text)} 


/\/\/ *@fail.*/ {
=;
s/\/\/ *@fail(\(.*\))/,\1/p
}


