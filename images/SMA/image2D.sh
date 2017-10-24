#!/bin/bash

echo 'Converting...'
convert -delay 10 image2D/image2D_* -loop 1 image2D.gif
#convert -loop 1 -quality 100 -compress None +antialias -scale 1000 -delay 10 image2D_* ../image2D_2.mpeg
