Thermograph
===========
This is an experimental project to implement a domain specific language
for metaprogramming C source code which solves sets of non-linear
equations.


Motivation
----------
In technical systems we ofen encounter network of flows.  Understanding
and designing them is a basic task of engineering. At some point in the
workflow, the engineer has to model the system based on the real world
setup and solve for its unknown parameters. 

Of course there are very powerful, free and non-free tools available for
such tasks, originating from academics or industry.  Dependend on the
discipline, companies and engineers will choose the most appropriate
tool for their task at hand.  Very often, the UI is graphical and the
network can be defined based on visual guidance. On the other hand,
experience users or professionals value tools with a textual interface
as it is more suitable to tighten and automate the workflow.

This project is a very simple and basic approach or proof-of-concept
to implement a new toolchain for solving such kind of problems. 

Its core goals are

* Define a DSL for definition of models 
* Parse the model definition and create a source code file for the system of
  equations
* Apply boundary conditions as provided in the definition input
* Solve the system of equations

Sample Implementation
`````````````````````
I chose a well-arranged thermodynamic problem to keep the development of
the meta program simple: Two flows of water will be mixed. The first flow has
to be throttled upstream the mixing point. For both fluid streams the
inflow conditions are known. The final result shall be the temperature
(and enthalpy) of the mixed flow. A visual representation of this
situation looks like this: 

.. image:: _static/docs/ex1.png


The green parameters are known and must be provided as boundary conditions
to the mathematical system. The red parameter is the solution to the
problem. Considering the underlying data structure, it is just one item
out of all calculation results.  The system of equation has been solved
for all parameters—green, black and red. 

It is straightforward to map the model above to the DSL description of
it::
    
    (valve ws (in 1) (out 2) (param 0.99)) 
    (header (in '(2 3)) (out '(4 5)))

    (set-bcs!
       '(1 p m t)
       '(3 m t)
       '(5 m))

The ``param 0.99`` defines the inherent relative pressure loss of the
valve. ``ws`` is a placeholder which shall define the media type, here
water. In this prototype it has no functionality as only water/steam is
implemented. (The later C program binds to a free *IAPWS IF97* library
for calculation of thermodynamic properties of water/steam).

Realization
-----------

The model is provided in ``def.scm`` using a domain specific language
which is parsed by several *Guile* Scheme scripts. Guile was chosen as
a scripting language which can be easily embedded in the output model, a
C code to interface with the user for provision of boundary conditions
and other parameters. 

Running the script ``tg-build`` will direct all actions to compose a set
of C source code files, the final output of this metaprogram. Please
have a look into it as it will guide you through all other scripts.

Currently there are two aggregates implemented, a valve and a (multi)
mixer, primarily denoted as *header* in the code base. The aggregates
provide the equations which have to be added to the overall system. You
will find two variants for each aggregate, the one with the ending
``*-inject.*`` is more complicated. It injects the boundary conditions
directly into the equations thus reducing the number of equation and
consequently the computational effort for solving the set of equations
later. Of course this will only become relevant for very large systems,
not in this simple example.

The output is set of C source code files which define the system of
equation. It uses `GSL's root finding algorithm`_ for solving this
system. If necessary, further external libraries have to be included.

.. _`GSL's root finding algorithm`: 
   https://www.gnu.org/software/gsl/doc/html/multiroots.html

Status
------
This project was just a proof-of-concept. I abandoned it because Scheme
is not my prefered language to develop a more scalable setup, although
it might provide some advantages as an embedded scripting language for a
more mature prototype. I welcome anyone to play around or extend it. In
principle the concept should allow for straightforward scalability, i.e.
increasing the network and extending the DSL with new components. 

Personally, I have implemented a simpler but more powerful version in
Python. It has more potential to evolve in a fully-fledged network
solving system. 
