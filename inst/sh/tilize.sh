#! /bin/bash

gdal_retile.py -targetDir rePopTiles rePop.tif
gdal_retile.py -targetDir rePopATiles rePopA.tif
gdal_retile.py -targetDir lcAUrbTiles lcAUrb.tif
gdal_retile.py -targetDir lcAAgTiles lcAAg.tif
gdal_retile.py -targetDir lcAPadTiles lcAPad.tif
