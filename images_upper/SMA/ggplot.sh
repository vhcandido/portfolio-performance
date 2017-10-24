#!/bin/bash

echo 'Converting...'
convert -delay 10 ggplot/ggplot_*.png -loop 1 ggplot.gif
