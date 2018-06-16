# How to make video with CHART/COUPE ?

## General ideas:

 1. prepare a script which produces a nice image, loop on time to produce a time series of images, in native NCL format (cgm )
 2. Transform cgm file into raster type file, using ctrans (NCL)  and convert (ImageMagick)

  Example:
  
    ctrans -d sun -res 1024x1024 file.cgm \> file.sun 
    convert file.sun file.gif

  Note that convert offer various option to control the quality of the raster image (-density -quality etc... ). Note that I use gif format because I want to produce animated gif video ans a starting point.

 3. Produce the video using gifsicle program (assuming that at this stage you have a bunch of files correctly ordered.)

  Example:
  
    gifsicle -d0 -l0 file*.gif > animation.gif

  Note that -d0  force the shortest delay between frames, -l0 force the animation to loop infinitely.  For slowing down the movie you can use -d nn (nn is a delay in miliseconds). 

  Note that delay and loop parameter can easily be changed directly on the animation.gif file :
  
    gifsicle -b -d10 -l20  animation.gif

## Advanced tricks: 
### reshaping the images: 
   gifsicle offers the possibility to crop part of the image into another image.
   
    gifsicle --crop X1,Y1-X2,Y2 file_in.gif > file_out.gif

   ImageMagick display tool is usefull for determining the crop windows (X1,Y1-X2-Y2) (in pixel).

### Assembling images in a mosaic of images:
   ImageMagick montage tool does the job : 

  Example:
  
    montage -tile 2x1 -geometry 950x500 file1.gif file2.gif montage.gif

  -tile define the layout of the mosaic. -geometry gives the dimension of each tile. 

### Using transparency: 
  * In CHART use the -spback option to use the background color for the missing values.
  * Then once the resulting image is in gif, you can transform the backgroud color to be transparent, using gifsicle :

  Example:
  
    gifsicle -t 0 file_in.gif > file_out.gif

  * In order to overlay an image with transparency over another image, composite tool from ImageMagick can be use :
  Example:
  
    composite  file_transparent.gif file_background.gif composite.gif 

  * gif image format does not support a gradual opacity. So in order to fully use opacity control, you need to transform your images in png format (for example), using convert: 
  
  Example:
  
     convert image.gif image.png 

  * then the compsite tool can be use with specific opacity options (-dissolve) :
  
  Example:
    
    composite -dissolve 70% file_transparent.png file_background.png composite.png

 * you can come back to gif (for using gifsicle) just by convert again

  Example:
  
    convert composite.png composite.gif

 * Of course when using transparency, you must take care in the image preparation so that no text or unwanted features (colorbar, label etc... )  interfere between superposed images.
  
