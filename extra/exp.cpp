void cmlTwistBase::exp (cmlDisplacementBase& H_displ, const double epsilon) const
{
  cmlRotation3DBase& rotation = H_displ.getRotation ();
  cmlQuaternionBase& quaternion = rotation.getQuaternion ();
  cmlDTinyVectorBase3& translation = H_displ.getTranslation ();

  const double omegaNorm = angularVelocity.norm ();
  const double omegaNorm2 = omegaNorm * omegaNorm;
  
  if (omegaNorm2 < epsilon) {
    const double scalarValue = (1.0 + (-1.0 + 0.0125 * omegaNorm2) * omegaNorm2 / 24.0) / 2.0;
    
    angularVelocity.scalarMult (scalarValue, quaternion);
    quaternion[3] = 1.0 + (-1.0 + omegaNorm2 / 48.0) * omegaNorm2 / 8.0;
    
    angularVelocity.crossProduct (pointVelocity, translation);
    //l'ajout de l'ordre 6 ne change rien avec le reglage du seuil a 1.0e-5
    translation *= 0.5 + (-1.0 + /*(1 - omegaNorm2 / 56.0) * */omegaNorm2 / 30.0) * omegaNorm2 / 24.0;
    
    const double scalarValue2 = 1. + (-1. + omegaNorm2 / 20.) * omegaNorm2 / 6.;
    pointVelocity.axpy (scalarValue2, translation);
    
    const double scalarValue3 = ((1.0 + (-1.0 + omegaNorm2 / 42.0) * omegaNorm2 * 0.05) / 6.0) * angularVelocity.dotProduct (pointVelocity);
    angularVelocity.axpy (scalarValue3, translation);
  }
  else {
    const double inv_omegaNorm  = 1. / omegaNorm;
    const double inv_omegaNorm2 = 1. / omegaNorm2;

    angularVelocity.scalarMult (sin (omegaNorm / 2.0) * inv_omegaNorm, quaternion);
    quaternion[3] = cos (omegaNorm / 2.0);
    
    angularVelocity.crossProduct (pointVelocity, translation);
    translation *= (1.0 - cos (omegaNorm)) * inv_omegaNorm2;
  
    const double scalarValue = sin (omegaNorm) * inv_omegaNorm;
    pointVelocity.axpy (scalarValue, translation);
    
    const double scalarValue2 = (1. - scalarValue) * angularVelocity.dotProduct (pointVelocity) * inv_omegaNorm2;
    angularVelocity.axpy (scalarValue2, translation);
  }
}


void cmlDisplacementBase::log (cmlTwistBase& logTwist, const double epsilon) const
{
  cmlTinyVectorBase<3>& angular_velocity = logTwist.getAngularVelocity ();
  cmlTinyVectorBase<3>& point_velocity = logTwist.getPointVelocity ();

  this->rotation.log (angular_velocity, epsilon);

  const double omegaNorm2 = angular_velocity.norm2 ();

  angular_velocity.crossProduct (this->translation, point_velocity);

  point_velocity.scalarMultInPlace (-0.5);
  //point_velocity += translation;

  double scalarValue;

  if (omegaNorm2 < epsilon) 
  {
    // scalarvalue = (1.0 + (1.0 + omeganorm2 / 42.0) * omeganorm2 / 60.0) / 
    //	12.0 * angular_velocity.dotproduct(translation);

    //scalarvalue = (1.0 + (1.0 + omeganorm2 / 42.0) * omeganorm2 / 60.0) / 12.0;

    point_velocity = translation;
    return;
  }
  else 
  {
    const double omegaNorm = sqrt (omegaNorm2); /* opt: sqrt */
    const double sin_omegaNorm = sin(omegaNorm);
    const double cos_omegaNorm = cos(omegaNorm);

    //scalarValue = (2.0 * sin_omegaNorm - omegaNorm * (1.0 + cos_omegaNorm)) / 
    //  (2.0 *omegaNorm2 * sin_omegaNorm) * angular_velocity.dotProduct(translation);

    scalarValue = (2.0 * sin_omegaNorm - omegaNorm * (1.0 + cos_omegaNorm)) / 
      (2.0 *omegaNorm2 * sin_omegaNorm);

  }

  //angular_velocity.axpy (scalarValue, point_velocity);

  translation.axpy ( ( 1. - scalarValue * omegaNorm2 ), point_velocity );

  angular_velocity.axpy (scalarValue * angular_velocity.dotProduct(translation), point_velocity);
}