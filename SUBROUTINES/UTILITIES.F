        !********************************************************
        !*                      TOYCODE-1.0                     *
        !*                                                      *
        !*      A MD PLATFORM FOR TESTING ENHANCED              *
        !*      SAMPLING ALGORITHMS.                            *
        !*                                                      *
        !*      DEVELOPED BY VENKAT KAPIL                       *
        !*      CONTACT : venkat@iitk.ac.in                     *
        !*                                                      *
        !*      COPYRIGHT (C) 2014 VENKAT KAPIL                 *
        !*      SOME RIGHTS RESERVED.                           *
        !*                                                      *                                                       
        !********************************************************

	!THIS FILE CONTAINS SUBROUTINES OF GENERAL UTILITY. 
	!THEY ARE AS FOLLOWS
	!	*) CREATE_FILE
	!	*) CHANGE_UNIT
	!	*) GET_GAUSSRAND

	!SUBROUTINE *) CREATE_FILE
	!THIS SUBROUTINE CREATES AN EMPTY FILE.
	SUBROUTINE CREATE_FILE( FILENAME )
	IMPLICIT NONE
	LOGICAL EXIST_STAT
	CHARACTER (LEN =*) FILENAME
	!CHECKS IF FILE EXISTS.
	INQUIRE(FILE= FILENAME, EXIST=EXIST_STAT)
	IF(EXIST_STAT .EQV. .TRUE.) THEN
	!REPLACES ORIGINAL FILE.
		OPEN( UNIT =1, FILE = FILENAME, STATUS = 'REPLACE' )
	ELSE 
	!CREATES NEW FILE
                OPEN( UNIT =1, FILE = FILENAME, STATUS = 'NEW' )
	ENDIF
	CLOSE(1)
	RETURN
	END SUBROUTINE
	
	!SUBROUTINE *) CHANGE_UNIT
	!THIS SUBROUTINE CHANGES THE UNITS OF PARAMETES TO ATOMIC UNITS.
	SUBROUTINE CHANGE_UNIT
	USE PARAM
        USE UMBRSAMP
        USE METADYN
	USE CONSTANTS
	IMPLICIT NONE
	INTEGER ITER
	REAL*8 DUMMY
	!GENERAL PARAMETERS.
	TEMP = TEMP / KO
	DELTA_T = DELTA_T / KO
	MASS = MASS * MP
	BARRIER = BARRIER * TEMP
	!UMBRELLA SAMPLING PARAMETERS.	
	IF( UMBR .EQV. .TRUE. ) THEN
	!FINDS THE LARGEST FORCE CONSTANT.
		DUMMY=0.0D0
		DO ITER=1, UMBR_DIM
			IF( UMBR_K(ITER) .GT. DUMMY ) DUMMY = UMBR_K(ITER)
		ENDDO
	!TUNES TIMESTEP TO UMBRELLA.
		DT = DT / DSQRT(DUMMY)
	!CONVERTS UMBRELLA CONSTANTS.
		DO ITER=1, UMBR_DIM
			UMBR_K(ITER)  = UMBR_K(ITER) * 4.0 *
     &         		 PI * PI * MASS / ( 673.0D0 * 673.0D0 )
		ENDDO
	ENDIF
	RETURN
	END SUBROUTINE

	!SUBROUTINE *) GET_GAUSSRAND
	!THIS SUBROUTINE RETURNS A GAUSSAIN RANDOM NUMBER.
	!IT USES BOX-MUELLER SAMPLING.
	SUBROUTINE GET_GAUSSRAND( GRAND, SIGMA )
	USE CONSTANTS
	IMPLICIT NONE
	REAL*8 GRAND, SIGMA, DUMMY1, DUMMY2
	!CHOOSES TWO PSEUDO RANDOM NUMBERs.
	DUMMY1 = RAND()
	DUMMY2 = RAND()
	!DOES A BOX MULELLER TRANSFORM.
	GRAND = SIGMA * DSQRT( -2.0 * DLOG( DUMMY1) ) *
     &	DCOS( 2*PI* DUMMY2 )
	RETURN
	END SUBROUTINE
