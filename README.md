# CHART_COUPE
CHART_COUPE is a plotting package based on NCAR Graphics library, available in NCL distribution. It was formely developped in the early 1990's by E. Brown (MEOM), and has been maintained so far with major periodical rewriting. This plotting package aims at producing nice plots from geophysical model output.


It is written in Fortran 90 and two programs are available once the package is compiled:

 * chart : for horizontal views of model results.
 * coupe : for visualization of vertical slices.

It is now available on GitHub under the CeCILL license (<http://www.cecill.info/licences/Licence_CeCILL_V2-en.html>).

CHART_COUPE is an open source package and contributions from other group are welcomed. The Git workflow policy is still to be defined.

## Using CHART_COUPE

#### Cloning the git repository
To retrieve a copy of the CHART_COUPE source code and create a working directory, run the following on the command line: 

```> git clone https://github.com/meom-group/CHART_COUPE ```

#### Compiling CHART_COUPE
There is a Makefile for compiling the sources. The compiler/machines related definitions are supposed to be collected in a `make.macro` file. Some examples of `make.macro` are given in the Macrolib directory and can be used as template for a new compiler or new machine. Then the good practice is to make a link 

```>  ln -sf Macrolib/macro.MACHINE  make.macro ```

In the `make.macro` file, the PATH for the netcdf library is specified, as well as compiler name and used flags.  

Worth to mention that before compiling you need to have a correct installation of NCL library, which gives you access to `ncargf90` command, which is a wrapper for the fortran compiler at load time.

Then using `make` (or even `make -j n` if you can compile on n cores), you will have the `chart` and `coupe` programs executable available in the directory. The executable files are ignore by git.


#### Running `chart` or `coupe`
 `chart` program uses online options which are documented in a french document <http://lgge.osug.fr/meom/Outils/WEB_CHART>

`coupe` uses almost the same  options, with few additional one dedicated to vertical slicing of the data.

General syntax is   
``` > chart [options]```  
or  
``` > coupe [options]```

The whole story in learning the options [hundred of options available]!

`chart` and `coupe` are widely used in the plotting part of the Drakkar MONitoring system (DMONTOOLS).




