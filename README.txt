Labs for Stanford CS240H, Fall 2011.
Author: Greg Horn

###### Building and running Lab1 ######

  >> cabal configure
  >> cabal build
  >> ./dist/build/lab1/lab1 someFile.txt someOtherFile.txt asManyAsYouWant.txt
  >> cat * | ./dist/build/lab1/lab1

############### ISSUES ################
The only problem I know about but didn't solve is that the histogram is aligned
with the longest word EVEN if that word has too few instances to be shown in
the histogram. If I changed the alignment to fix this, it could actually cause
that long word to be included after all in some cases, and a short solution did
not occur to me. I figured the people reading this code would appreciate
conciseness over completeness.
