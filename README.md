# Implementation of Extended Finite State Machines for Creating RESTful Read-Write Interfaces for Assets
## Installation

You will need to install [PureScript](https://www.purescript.org/) to build the project. You usually can do this by running `npm install -g purescript`.

You also need to clone and build the [RDFPS repository](https://github.com/wintechis/rdfps). Then open the `packages.spago` file in this repository. At the bottom of the file you will find the following lines:
```
in  upstream
  with rdfps = /home/daniel/Git/rdfps/spago.dhall as Location
```
Adapt the path to your RDFPS repository accordingly.

You can then just run `spago build` to build the project.

## Run Robot Arm example
There is a Robot Arm example defined in `src/RobotArm.purs`. You can run it using `spago run`.

## Further Information
[A RESTful Interaction Model for Semantic Digital Twins](https://solid.ti.rw.fau.de/public/2022/A_RESTful_Interaction_Model_for_Semantic_Digital_Twins.pdf)