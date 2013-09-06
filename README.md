Differentiation to write efficiently and easily physics engine, I've decided to implement a robot simulator.

Our robots are modelized using generelized coordinates and a Lagrangian approach. See [Murray Ly Sastry](https://www.google.fr/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0CCwQFjAA&url=http%3A%2F%2Fwww.cds.caltech.edu%2F~murray%2Fbooks%2FMLS%2Fpdf%2Fmls94-complete.pdf&ei=6sQpUsG_DoWp7Qbb4oGwAQ&usg=AFQjCNF7UfyyR12eG5iyqqikSzEJMsRZew&sig2=iqZtwiDWEnXwjZ-vo-Vdiw&bvm=bv.51773540,d.ZGU).

Hence Lie groups, i.e. SO(3), are used to modelize position, and Lie algebra, so(3) to modelize velicity.

#### Examples
Progressive examples can be found in the [examples directory](https://github.com/kayhman/lisphys/tree/master/src/examples).

##### Features
In order to achieve its goal, Lisphys provides the followings utilities:

* Automatic Differentiation, with Jacobian computation and support for 3D vectors and matrices
* 3D vector class, with basic operation on it : cross product, dot product, norm, ...
* Matrix class, with arithmetic operation and LU factorisation.
* Twist, i.e. element of se(3) and Displacement, i.e. element of SE(3) classes, used to describe rigid bodies position and velocities.

