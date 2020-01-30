# The Dactyl Keyboard
The Dactyl is a parameterized, split-hand, concave, columnar, ergonomic keyboard.

This fork is an experiment in changing the mounting style of the dactyl. Originally designed as an integrated-plate/tray-mount keyboard, this version creates a bottom-mount dactyl variant. 

* [This guide from Thomas Baart can tell you more about keyboard mounting styles.](https://thomasbaart.nl/2019/04/07/cheat-sheet-custom-keyboard-mounting-styles/)

Other changes include:

* thinner plate. 
* snap-in Alps keyswitch and stabilizer cutouts.
* reversion to 1u outer keys from 1.5u keys in kennykaye fork.
* wider bezels to accommodate mounting hardware.
* more column stagger.  
* more space in bottom case to accommodate Alps switch depth.
* separation of top plate from top case.
* post-processing of top case STL files in Blender (still a work in progress, but case seems to fit)

## Assembly

You will need:

* 70 Alps SKCM or SKCL keyboard switches
* 4 Alps stabilizers (optional)
* 66 Alps-compatible 1u keycaps
* 4 Alps-compatible 2u keycaps
* 18 M3 screws
* 18 heat-set m3 screw inserts
* diodes
* wires
* 2 pro micro microcontrollers (ARM -can- be done via serial but it's still rather experimental in QMK)
* soldering iron
* solder
* time, specifically
* a lot of time
* patience

### Generating a Design

**Setting up the Clojure environment**
* [Install the Clojure runtime](https://clojure.org)
* [Install the Leiningen project manager](https://leiningen.org/)
* [Install OpenSCAD](https://www.openscad.org/) /// [Recommended: 22-12-2019 builds from here](https://files.openscad.org/snapshots/) 
* [Install Blender](https://blender.org/) or any other STL editor

**Generating the design**
* Run `lein repl`
* Load the file `(load-file "src/dactyl_keyboard/dactyl.clj")`
* This will regenerate the `things/*.scad` files
* Use OpenSCAD to open a `.scad` file.
* Make changes to design, repeat `load-file`, OpenSCAD will watch for changes and rerender.
* When done, use OpenSCAD to export STL files
* Smooth out rough edges with Blender or your STL editor of choice

**Tips**
* [Some other ways to evaluate the clojure design file](http://stackoverflow.com/a/28213489)
* [Example designing with clojure](http://adereth.github.io/blog/2014/04/09/3d-printing-with-clojure/)


### Printing
Pregenerated STL files are available in the [things/](things/) directory.

### Wiring
Masks for the flexible PCBs that aderath used are not recommended due to changes in dimension from his original design.

A [very rough guide for the brave is here](guide/README.org#wiring) - It will be improved over time (**TODO**)!

## License

Copyright © 2015 Matthew Adereth

The source code for generating the models (everything excluding the [things/](things/) and [resources/](resources/) directories is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).  The generated models and PCB designs are distributed under the [Creative Commons Attribution-NonCommercial-ShareAlike License Version 3.0](LICENSE-models).
