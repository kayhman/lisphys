### Lisphys : Physical simulation using Automatic Differentiation
Lisphys is a small project whose main goal is to experiment with Automatic Differentiation. More precisely, it is a kind of proof of concept that Automatic Differentiation can be used to create complex physics engines. 

However, Lisphys want to address the following points:
* Demonstrate that Automatic Differentiation can ease development of physics engines
* Provide a uniform framework to compute all the various derivative used in physics engines : Stiffness, Damping, Jacobian, Velocity, Acceleration, ...
* Offer a pedagogic tool to discover physics simulation, by letting learner to experiment by themselves.

#### How can Automatic Differentiation help to write physics engine ?
When you're designing physics engine for multibodies system, two physical properties are important : position and forces.

All the others significant physical properties can be derived from this two ones :

* Velocity is the time derivative of position p : v = dp / dt 
* Acceleration is the time derivative of velocity, hence, the second time derivative of position p :  a = dp / dt^2
* Stiffness is the derivative of force f with restect to position p : k = df/dp
* Damping is the the derivative of force f with respect to velocity v : d = df/dv
* Jacobian, as used wot robot control, are the derivative of the 6D position with respect to the robot degrees of freedom.
* ...

Hence, if you have a formula to describe the positions and forces of, and have the necessary tools to perform Automatic Differentiation, then it becomes easy to write a time integrator based on euler explicit or implicit methods. 

And these remarks also stand for fluid simulation, particules simulation, financial simulation, and so on.

#### Lisphys for robotic simulation
To illustrate the use of Automatic Differentiation to write efficiently and easily physics engine, I've decided to implement a robot simulator.

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
