#include <iostream>
#include <Eigen/Lgsm>


int main(int argc, char* argv [])
{
  Eigen::Vector3d pos(1, 0, 0);
  Eigen::Displacementd H_1_0 = Eigen::Displacementd(pos);
  Eigen::Displacementd H_2_1 = Eigen::Displacementd(pos);

  Eigen::Vector3d axis(0, 0, 1);

  Eigen::Twistd tw(Eigen::AngularVelocityd(axis), pos.cross(axis));

  tw *=  M_PI / 2.0;

  // See Murray Lie Sastry page 50. 
  Eigen::Displacementd H_2_0 = tw.exp()  * H_1_0 * H_2_1;


  std::cout << "twist : " << tw.transpose() << std::endl;
  std::cout << "H_1_0 : " << H_1_0 << std::endl;
  std::cout << "H_2_1 : " << H_2_1 << std::endl;
  std::cout << "Brockett factor : " << tw.exp()  * H_1_0 << std::endl;
  std::cout << "pos H_2_0 " << H_2_0.getTranslation().transpose() << std::endl;
  
  return 0;
}
