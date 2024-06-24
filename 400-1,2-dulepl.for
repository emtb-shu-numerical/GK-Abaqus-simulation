	  SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 	    		   JLTYP,TEMP,PRESS,SNAME)
	  INCLUDE 'ABA_PARAM.INC'

	  DIMENSION FLUX(2),TIME(2),COORDS(3)
	  CHARACTER*80 SNAME

C	  wu,焊接电压
C	  wi,焊接电流
C	  effi,焊接效率系数
C	  v,焊接速度m/s
C	  q,电弧有效热功率W

	  v=1.2
	  q=260
	  d=v*TIME(2)

	  x=COORDS(1)
	  y=COORDS(2)
	  z=COORDS(3)

C	  从坐标x0,y0,z0开始，沿着z方向移动
	  x0=0
	  y0=0
	  z0=0

C	  a1,a2,b,c为双椭球的形状参数
	  a1=0.00006
	  a2=0.00009
	  b=0.00006
	  c=0.00006

C	  f1为热源分配系数
	  f1=1.0
	  PI=3.1415926

	  heat1=6.0*sqrt(3.0)*q/(a1*b*c*PI*sqrt(PI))*f1
	  heat2=6.0*sqrt(3.0)*q/(a2*b*c*PI*sqrt(PI))*(2.0-f1)

	  shape1=exp(-3.0*(z-z0-d)**2/(a1)**2-3.0*(x-x0)**2/b**2
     1  -3.0*(y-y0)**2/c**2)
	  shape2=exp(-3.0*(z-z0-d)**2/(a2)**2-3.0*(x-x0)**2/b**2
     1  -3.0*(y-y0)**2/c**2)

C	  JLTYP=1 表示为体热源
	  JLTYP=1
	  IF(z .GE.(z0+d)) THEN
	    FLUX(1)=heat1*shape1
	  ELSE
		FLUX(1)=heat2*shape2
	  ENDIF
	  RETURN
	  END