Labs for Stanford CS240H, Fall 2011.
Author: Greg Horn

###### Building and running Lab1 and Lab2 ######

  >> cabal configure
  >> cabal build


############### Running Lab 2 ##################
To run the main program:
  >> ./dist/build/lab2/lab2 someFile.txt

To run the hilbert curve quickCheck program:
  >> ./dist/build/runTests/runTests

To draw a hilbert curve of order 5
  >> ./dist/build/drawHilbertCurve/drawHilbertCurve 5

To draw all the rectangles in a file
  >> ./dist/build/drawRects/drawRects someFile.txt


############### Running Lab 1 ##################
  >> ./dist/build/lab1/lab1 someFile.txt someOtherFile.txt asManyAsYouWant.txt
  >> cat * | ./dist/build/lab1/lab1


############### ISSUES WITH LAB 1 ################
The only problem I know about but didn't solve is that the histogram is aligned
with the longest word EVEN if that word has too few instances to be shown in
the histogram. If I changed the alignment to fix this, it could actually cause
that long word to be included after all in some cases, and a short solution did
not occur to me. I figured the people reading this code would appreciate
conciseness over completeness.
