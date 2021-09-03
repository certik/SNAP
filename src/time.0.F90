!-----------------------------------------------------------------------
!
! MODULE: time_module
!> @brief
!> This module contains the variables that measure SNAP execution
!> times for different pieces of code and the subroutine used to get the
!> time. It also has the timing summary print.
!
!-----------------------------------------------------------------------

MODULE time_module

  USE global_module, ONLY: r_knd, i_knd, zero, ounit

#ifdef MPI
  USE mpi
#endif

  IMPLICIT NONE

  PUBLIC

  SAVE
!_______________________________________________________________________
!
! Run-time variables
!
! tsnap    - total SNAP run time
! tparset  - parallel environment setup time
! tinp     - input run time
! tset     - setup run time
! tslv     - total solution run time
! tparam   - time for setting up solve parameters
! totrsrc  - time for outer source computations
! tinners  - total time spent on inner iterations
! tinrsrc  - time for inner source computations
! tsweeps  - time for transport sweeps, including angular sourc compute
! tinrmisc - time for miscellaneous inner ops
! tslvmisc - time for miscellaneous solution ops
! tout     - output run time
! tgrind   - transport grind time
!_______________________________________________________________________

  REAL(r_knd) :: tsnap=zero, tparset=zero, tinp=zero, tset=zero,       &
    tslv=zero, tparam=zero, totrsrc=zero, tinners=zero, tinrsrc=zero,  &
    tsweeps=zero, tinrmisc=zero, tslvmisc=zero, tout=zero, tgrind=zero


  CONTAINS


  SUBROUTINE wtime ( time )

!-----------------------------------------------------------------------
!
! Get the current time
!
!-----------------------------------------------------------------------

    REAL(r_knd), INTENT(OUT) :: time
!_______________________________________________________________________

#ifdef MPI
    time = MPI_WTIME()
#else
!    CALL CPU_TIME ( time )
#endif
!_______________________________________________________________________
!_______________________________________________________________________

  END SUBROUTINE wtime


  SUBROUTINE time_summ

!-----------------------------------------------------------------------
!
! Print the timing summary to the output file.
!
!-----------------------------------------------------------------------
!_______________________________________________________________________
!
!  Local variables
!_______________________________________________________________________

    CHARACTER(LEN=1) :: star='*'

    INTEGER(i_knd) :: i
!_______________________________________________________________________

    tinrmisc = tinners - ( tinrsrc + tsweeps )
    tslvmisc = tslv - ( tparam + totrsrc + tinners )

!    WRITE( ounit, 401 ) ( star, i = 1, 80 )
    WRITE( ounit, * )
    WRITE( ounit, * ) tparset
    WRITE( ounit, * ) tinp
    WRITE( ounit, * ) tset
    WRITE( ounit, * ) tslv
    WRITE( ounit, * ) tparam
    WRITE( ounit, * ) totrsrc
    WRITE( ounit, * ) tinners
    WRITE( ounit, * ) tinrsrc
    WRITE( ounit, * ) tsweeps
    WRITE( ounit, * ) tinrmisc
    WRITE( ounit, * ) tslvmisc
    WRITE( ounit, * ) tout
!_______________________________________________________________________

!_______________________________________________________________________
!_______________________________________________________________________

  END SUBROUTINE time_summ


END MODULE time_module
