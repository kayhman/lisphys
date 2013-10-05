Lisphys 
########

:author: Guillaume Saupin
:summary: Presentation of the lisphys simulation engine

Lisphys : Physical simulation using Automatic Differentiation
==============================================================
Motivation
----------
Lisphys is a small project whose main goal is to experiment with Automatic Differentiation. More precisely, it is a kind of proof of concept that Automatic Differentiation can be used to create complex physics engines. 

However, Lisphys wants to address the following points:

* Demonstrate that Automatic Differentiation can ease development of physics engines
* Provide a uniform framework to compute all the various derivative used in physics engines : Stiffness, Damping, Jacobian, Velocity, Acceleration, ...
* Offer a pedagogic tool to discover physics simulation, by letting learner experiment by themselves.


Functions : Lisphys for robotic simulation
-------------------------------------------
Currently, Lisphys allow the dynamic simulation of simple multi body system made of rigid bodies connected by simple joints like hinge or prismatic joints.

To illustrate the use of Automatic Differentiation to write efficiently and easily physics engine, I've decided to implement a robot simulator.

Our robots are modelized using generelized coordinates and a Lagrangian approach. See `[Murray Ly Sastry]`_.

Hence Lie groups, i.e. SO(3), are used to modelize position, and Lie algebra, so(3) to modelize velocity. The exponetial maps is used to locally project the algebra onto the group.

Examples
*********
Progressive examples can be found in the `examples directory`_.

Features
*********
In order to achieve its goal, Lisphys provides the followings utilities:

* Automatic Differentiation, with Jacobian computation and support for 3D vectors and matrices
* 3D vector class, with basic operation on it : cross product, dot product, norm, ...
* Matrix class, with arithmetic operation and LU factorisation.
* Twist, i.e. element of se(3) and Displacement, i.e. element of SE(3) classes, used to describe rigid bodies position and velocities.

Methodology : How can Automatic Differentiation help to write physics engine ?
--------------------------------------------------------------------------------
When you're designing physics engine for multibodies system, two physical properties are important : position and forces.

All the others significant physical properties can be derived from this two ones :

* Velocity is the time derivative of position p : $ v = \\frac{dp}{dt}$ 
* Acceleration is the time derivative of velocity, hence, the second time derivative of position p :  $a = \\frac{dp}{dt^2}$
* Stiffness is the derivative of force f with restect to position p : $ k = \\frac{df}{dp} $
* Damping is the the derivative of force f with respect to velocity v : $ d = \\frac{df}{dv} $
* Jacobian, as used for robot control, are the derivative of the 6D position with respect to the robot degrees of freedom.
* ...

Hence, if you have a formula to describe the positions and forces of, and have the necessary tools to perform Automatic Differentiation, then it becomes easy to write a time integrator based on euler explicit or implicit methods. 

And these remarks also stand for fluid simulation, particules simulation, financial simulation, and so on.

Macro readers
**************
#m
#v
#q
#d

Main classes
*************

* vector3 and vector3-ad
* matrix and matrix-ad
* quaternion
* displacement


Automatic differentiation
*************************
.. code-block:: shell

  CL-USER> (defun )



Installing Lisphys
-------------------
Lisphys is distributed as a library written in Common Lisp. The code is hosted on `Liphys github repository`_. 
You can see a screencast showing its installation on vimeo_.

It can be downloaded using git :

.. code-block:: shell

  # cd my-projects
  # git clone git@github.com:kayhman/lisphys.git
  # cd lisphys

Lisphys has the following dependencies :
* A common lisp compiler. I've been using sbcl.
* Quicklisp.

I also recommend to use Emacs.

Lisphys use `quicklisp`_ to ease the installation process. Follow the instructions on the quicklisp website to install it.

Then, loading Lisphys into Emacs is quite simple:

.. code-block:: shell

  # cd lisphys
  # emacs &

Launch slime using M-x slime, and then run (ql::quickload "lisphys"). You now have access to the (beta) Lisphys library. 
Enter 

.. code-block:: lisp

  CL-USER> (in-package #:lisphys)
  LISPHYS> (start)

To start a small simulator simulating a double pendulum.

Tests
------

Conclusion
-----------
There is still much to be done to make Lisphys fully usable. First, its api needs to be finished. Currenlty, there is only a few high level methods available to described the kinematic of the system simulated : make-hinge, add-rigid-body. More are needed :

* make-prismatic
* make-ball-joint
* make-joint-effort
* make-spring-joint
* add-joint-effort
* add-external-force
* ...

There should be also support for constraints, using differential inclusing and iterative solver (Gauss Seidel type).


.. _`examples directory`: https://github.com/kayhman/lisphys/tree/master/src/examples
.. _`[Murray Ly Sastry]`: https://www.google.fr/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0CCwQFjAA&url=http%3A%2F%2Fwww.cds.caltech.edu%2F~murray%2Fbooks%2FMLS%2Fpdf%2Fmls94-complete.pdf&ei=6sQpUsG_DoWp7Qbb4oGwAQ&usg=AFQjCNF7UfyyR12eG5iyqqikSzEJMsRZew&sig2=iqZtwiDWEnXwjZ-vo-Vdiw&bvm=bv.51773540,d.ZGU)
.. _`Liphys github repository`: https://github.com/kayhman/lisphys
.. _`quicklisp`: http://www.quicklisp.org
.. _vimeo: http://www.vimeo.com/...
