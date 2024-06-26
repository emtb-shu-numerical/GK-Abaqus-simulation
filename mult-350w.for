      FUNCTION project(a, b)
      REAL :: project
      REAL, DIMENSION(3), INTENT(IN) :: a, b
      project = (a(1) * b(1) + a(2) * b(2) + a(3) * b(3)) / sqrt(sum(b(:)**2))
      END FUNCTION project

      SUBROUTINE cross(crossVec,a, b)
      IMPLICIT NONE
      REAL, DIMENSION(3) :: crossVec
      REAL, DIMENSION(3), INTENT(IN) :: a, b
      crossVec(1) = a(2) * b(3) - a(3) * b(2)
      crossVec(2) = a(3) * b(1) - a(1) * b(3)
      crossVec(3) = a(1) * b(2) - a(2) * b(1)
      RETURN
      END

      SUBROUTINE RotateVector(rVector, vec, angle, axis)
      IMPLICIT NONE
      REAL, INTENT(IN) :: angle
      REAL, DIMENSION(3), INTENT(IN) :: axis, vec
      REAL, DIMENSION(3) :: crossVec, rVector, unitAxis
      REAL, DIMENSION(3) :: item1, item2, item3, item4
      unitAxis(:) = axis(:)/sqrt(sum(axis(:)**2))
      call cross(crossVec, unitAxis, vec)
      item1 = vec * cosd(angle)
      item2 = crossVec * sind(angle)
      item3 = unitAxis*dot_product(unitAxis, vec)*(1-cosd(angle))
      rVector = item1+item2+item3
      RETURN
      END
C   ****************************************************
C              Freely   Moving   Heat   Source
C   ****************************************************
      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,JLTYP,
     1 TEMP,PRESS,SNAME)
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION COORDS(3), FLUX(2), TIME(2)
      REAL, DIMENSION(3) :: COORDT, COORDN, VECTORX, VECTORY, VECTORZ
      CHARACTER*80 SNAME
      REAL, PARAMETER :: PI = 3.14159265359
      REAL curLen, curtime, xshape, xheat, xflux
      REAL :: project
C     ! State CORE      REAL CORE(3,npath)
      REAL CORE(3,25)
C     ! State&Assign Array T0s
      REAL :: T0s(25) = [
     * 0.0,
     * 0.007142,
     * 0.014284,
     * 0.021426,
     * 0.028568,
     * 0.03571,
     * 0.042852,
     * 0.049994,
     * 0.057136,
     * 0.064278,
     * 0.07142,
     * 0.078562,
     * 0.085704,
     * 0.092846,
     * 0.099988,
     * 0.10713,
     * 0.114272,
     * 0.121414,
     * 0.128556,
     * 0.135698,
     * 0.14284,
     * 0.149982,
     * 0.157124,
     * 0.164266,
     * 0.171408	]
C     ! State&Assign Array T1s
      REAL :: T1s(25) = [
     * 0.00714285710294,
     * 0.0142848571029,
     * 0.0214268571429,
     * 0.0285688571029,
     * 0.0357108571029,
     * 0.0428528571029,
     * 0.0499948571029,
     * 0.0571368571429,
     * 0.0642788571029,
     * 0.0714208571029,
     * 0.0785628571029,
     * 0.0857048571029,
     * 0.0928468571429,
     * 0.0999888571029,
     * 0.107130857103,
     * 0.114272857103,
     * 0.121414857103,
     * 0.128556857143,
     * 0.135698857103,
     * 0.142840857103,
     * 0.149982857103,
     * 0.157124857103,
     * 0.164266857143,
     * 0.171408857103,
     * 0.178550857103	]
C     ! State&Assign Array speeds
      REAL :: speeds(25) = [
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4,
     * 1.4	]
C     ! State&Assign Array powers
      REAL :: powers(25) = [
     * 98.0,
     * 98.0,
     * 98.0,
     * 98.0,
     * 98.0,
     * 96.0,
     * 96.0,
     * 96.0,
     * 96.0,
     * 96.0,
     * 94.0,
     * 94.0,
     * 94.0,
     * 94.0,
     * 94.0,
     * 92.0,
     * 92.0,
     * 92.0,
     * 92.0,
     * 92.0,
     * 90.0,
     * 90.0,
     * 90.0,
     * 90.0,
     * 90.0	]
C     ! State&Assign Array ffs
      REAL :: ffs(25) = [
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0,
     * 1.0	]
C     ! State&Assign Array afs
      REAL :: afs(25) = [
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001	]
C     ! State&Assign Array ars
      REAL :: ars(25) = [
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001	]
C     ! State&Assign Array bs
      REAL :: bs(25) = [
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001	]
C     ! State&Assign Array cs
      REAL :: cs(25) = [
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001,
     * 0.0001	]
C     ! State&Assign Array paths
      REAL :: paths(3, 2, 25) = [
     * -5.54298360441e-18, -0.000199999989862, 0.0,
     * -5.54298360441e-18, -0.000199999989862, 0.00999999994412,
     * 0.000119999953313, -0.000199999985183, 0.00999999994412,
     * 0.000119999953313, -0.000199999985183, 0.0,
     * 0.00024, -0.000199999980504, 0.0,
     * 0.00024, -0.000199999980504, 0.01,
     * 0.000359999953313, -0.000199999975825, 0.00999999994412,
     * 0.000359999953313, -0.000199999975825, 0.0,
     * 0.000479999953313, -0.000199999971146, 0.0,
     * 0.000479999953313, -0.000199999971146, 0.00999999994412,
     * -5.54298360279e-18, -0.000149999989862, 0.0,
     * -5.54298360279e-18, -0.000149999989862, 0.00999999994412,
     * 0.000119999953313, -0.000149999985183, 0.00999999994412,
     * 0.000119999953313, -0.000149999985183, 0.0,
     * 0.00024, -0.000149999980504, 0.0,
     * 0.00024, -0.000149999980504, 0.01,
     * 0.000359999953313, -0.000149999975825, 0.00999999994412,
     * 0.000359999953313, -0.000149999975825, 0.0,
     * 0.000479999953313, -0.000149999971146, 0.0,
     * 0.000479999953313, -0.000149999971146, 0.00999999994412,
     * -5.51587855414e-18, -9.99999898622e-05, 0.0,
     * -5.51587855414e-18, -9.99999898622e-05, 0.00999999994412,
     * 0.000119999953313, -9.99999851833e-05, 0.00999999994412,
     * 0.000119999953313, -9.99999851833e-05, 0.0,
     * 0.00024, -9.99999805043e-05, 0.0,
     * 0.00024, -9.99999805043e-05, 0.01,
     * 0.000359999953313, -9.99999758254e-05, 0.00999999994412,
     * 0.000359999953313, -9.99999758254e-05, 0.0,
     * 0.000479999953313, -9.99999711464e-05, 0.0,
     * 0.000479999953313, -9.99999711464e-05, 0.00999999994412,
     * -5.51587855414e-18, -4.99999898622e-05, 5.58793497585e-11,
     * -5.51587855414e-18, -4.99999898622e-05, 0.01,
     * 0.000119999953313, -4.99999851833e-05, 0.01,
     * 0.000119999953313, -4.99999851833e-05, 5.58793497585e-11,
     * 0.00024, -4.99999805043e-05, 0.0,
     * 0.00024, -4.99999805043e-05, 0.01,
     * 0.000359999953313, -4.99999758254e-05, 0.01,
     * 0.000359999953313, -4.99999758254e-05, 5.58793497585e-11,
     * 0.000479999953313, -4.99999711464e-05, 5.58793497585e-11,
     * 0.000479999953313, -4.99999711464e-05, 0.01,
     * -4.96022493912e-18, 0.0, 0.0,
     * -4.96022493912e-18, 0.0, 0.00999999994412,
     * 0.000119999953313, 0.0, 0.00999999994412,
     * 0.000119999953313, 0.0, 0.0,
     * 0.00024, 0.0, 0.0,
     * 0.00024, 0.0, 0.01,
     * 0.000359999953313, 0.0, 0.00999999994412,
     * 0.000359999953313, 0.0, 0.0,
     * 0.000479999953313, 0.0, 0.0,
     * 0.000479999953313, 0.0, 0.00999999994412	]
C     ! State&Assign Array lenPaths
      REAL :: lenPaths(2, 25) = [
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.01,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.01,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.01,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.01,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.01,
     * 0.0,
     * 0.00999999994412,
     * 0.0,
     * 0.00999999994412	]
C     ! State&Assign Array normals
      REAL :: normals(3, 2, 25) = [
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, 1.0, 0.0,
     * 0.0, 1.0, 0.0,
     * 0.0, 1.0, 0.0,
     * 0.0, 1.0, 0.0,
     * 0.0, 1.0, 0.0,
     * 0.0, 1.0, 0.0,
     * 0.0, 1.0, 0.0,
     * 0.0, 1.0, 0.0,
     * 0.0, 1.0, 0.0,
     * 0.0, 1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0,
     * 0.0, -1.0, 0.0	]
C
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Main Program  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      JLTYP = 1
      FLUX(1) = 0.0
      curtime = TIME(2)
      IF (curtime.lt.MinVal(T0s) .or. curtime.gt.MaxVal(T1s)) THEN
        RETURN
      END IF
C
      DO J=1, size(CORE, dim=2)
        IF (curtime.lt.T0s(J) .or. curtime.gt.T1s(J)) THEN
          xflux = 0.
        ELSE
          curLen = (curtime-T0s(J)) * speeds(J)
          DO I=1, size(paths, dim=2)
            IF (curLen.ge.lenPaths(I,J) .and. curLen.lt.lenPaths(I+1,J)) THEN
              x = (curLen - lenPaths(I,J))/(lenPaths(I+1,J) - lenPaths(I,J))
              CORE(:,J) = paths(:,I,J)*(1-x) + paths(:,I+1,J)*x
              COORDT = COORDS(:) - CORE(:,J)                        ! 热源中心平移坐标系
              VECTORX = paths(:,I+1,J) - paths(:,I,J)
              VECTORZ = normals(:,I,J)*(1-x) + normals(:,I+1,J)*x
C
C              CALL RotateVector(VECTORZ, VECTORZ, angles(J), VECTORX)
              CALL cross(VECTORY, VECTORX, VECTORZ)
C
              COORDN(1) = project(COORDT, VECTORX)
              COORDN(2) = project(COORDT, VECTORY)
              COORDN(3) = project(COORDT, VECTORZ)
              EXIT
            END IF
          END DO
          IF (COORDN(1) .GE. 0.0) THEN
            xshape = exp(-3.0*( (COORDN(1)/afs(J))**2
     *                        + (COORDN(2)/bs(J) )**2 
     *                        + (COORDN(3)/cs(J) )**2))
            xheat = 6.*sqrt(3.) * powers(J) / (afs(J)*bs(J)*cs(J)*PI*sqrt(PI))
     *                        * ffs(J)
          ELSE
            xshape = exp(-3.0*( (COORDN(1)/ars(J))**2
     *                        + (COORDN(2)/bs(J) )**2 
     *                        + (COORDN(3)/cs(J) )**2))
            xheat = 6.*sqrt(3.) * powers(J) / (ars(J)*bs(J)*cs(J)*PI*sqrt(PI))
     *                        * (2. - ffs(J))
          END IF
          xflux = xshape * xheat
        END IF
C
        FLUX(1) = FLUX(1) + xflux   ! Gauss Heat Source; the distribution of heat flux
      END DO
C
      RETURN
      END
      
