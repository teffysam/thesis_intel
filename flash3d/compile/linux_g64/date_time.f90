!*****************************************************************
!
! MODULE NAME:
!	date_time
! FUNCTION:
!	define date and time with year, month, day, hour, minute, second
! CONTAINS:
!-----------------------------------------------------------------
!
! TYPE: date
!-----------------------------------------------------------------
!
! NAME:
!	deltatime_in_seconds
! FUNCTION:
!	calculate the time in seconds since calculation start
! SYNTAX:
!	int=deltatime_in_seconds(int.arr,int.arr)
! ON INPUT:
!	i_date0: date and time of calculation start		integer
!	i_date1: date and time of eruption change (rhs change)		integer
! ON OUTPUT:
!	i_dt: time in seconds since calculation start
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!
! COMMENTS:
!	
! USES:
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		e. gerwing 4/2015
!
!*****************************************************************

MODULE date_time
	  PUBLIC

!*****************************************************************

  TYPE:: date
    INTEGER                               :: year = 1      ! year (1 - ????)
    INTEGER                               :: month = 1     ! month (1 - 12)
    INTEGER                               :: day = 1       ! day (1 - 31)
    INTEGER                               :: hour = 0      ! hour (0 -23)
    INTEGER                               :: minute = 0    ! minute (0 - 59)
    INTEGER                               :: second = 0    ! second (0 - 59)
  END TYPE
  	  CONTAINS
!*****************************************************************
	FUNCTION deltatime_in_seconds(i_date0,i_date1) RESULT (i_dt)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (date), INTENT(in)                     :: i_date0,i_date1
	  INTEGER                                     :: i_dt
	  INTEGER                                     :: i_m
	  INTEGER                                     :: i_year, i_month, i_day, i_hour, i_minute, i_second
	  INTEGER, DIMENSION(12)                      :: i_days_per_month

!---------- Number of days per month [jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec]
	  i_days_per_month = [31,28,31,30,31,30,31,31,30,31,30,31]

!---------- Difference between the years in seconds
	  i_year = (i_date1%year - i_date0%year)*365*24*60*60

!---------Difference between the months in seconds

	  i_month = 0
	  IF (i_date0%month < i_date1%month) THEN
	    DO i_m = i_date0%month,i_date1%month-1
	      i_month = i_month + i_days_per_month(i_m)
	    END DO 
	  ELSE IF (i_date0%month > i_date1%month) THEN
	    DO i_m = i_date0%month,12
	      i_month = i_month + i_days_per_month(i_m)
	    END DO
	    DO i_m = 1,i_date1%month
	      i_month = i_month + i_days_per_month(i_m)
	    END DO
	  END IF
	  
	  i_month = i_month * 24*60*60

!---------Difference between the days in seconds
	  i_day = (i_date1%day - i_date0%day)*24*60*60

!---------Difference between the hours in seconds
	  i_hour = (i_date1%hour - i_date0%hour)*60*60

!---------Difference between the minutes in seconds
	  i_minute = (i_date1%minute - i_date0%minute)*60

!---------Difference between the seconds
	  i_second = (i_date1%second - i_date0%second)

!---------Total difference in seconds
	  i_dt = i_year + i_month + i_day + i_hour + i_minute + i_second


	  END FUNCTION deltatime_in_seconds

!*****************************************************************
  
END MODULE
