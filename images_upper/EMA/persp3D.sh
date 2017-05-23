#!/bin/bash

echo 'Converting...'
convert -delay 15 persp3D/persp3D_{140..310..5}.png persp3D/persp3D_{310..140..5}.png -loop 0 persp3D.gif
