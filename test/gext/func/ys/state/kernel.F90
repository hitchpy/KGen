

module kernel

        USE types_mod

        implicit none

        !integer(kind=4), parameter :: np =4
        integer(kind=4), parameter :: niter =10

        public add

        contains

        subroutine add(t1)

                type(complex_type), intent(inout), target :: t1(:)
                integer :: i

                INTEGER, SAVE :: kgen_unit 
                OPEN (NEWUNIT=kgen_unit, FILE="test.txt", STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="WRITE", &
                &CONVERT="BIG_ENDIAN") 
                do i=1,niter
                        t1(i)%elem1%c(:,:) = (t1(i)%elem2%level3_1%param1(:,:) + &
                                             t1(i)%elem2%level3_1%param2(:,:)) * &
                                             (t1(i)%elem1%a(:,:) + &
                                             t1(i)%elem1%b(:,:))
                        ctype%elem1%c = t1(i)%elem1%c
                end do

                atype(1)%elem2%d = t1(1)%elem2%d
                CALL kp_types_mod_complex_type__complex_type_dim1(atype, kgen_unit) 
                CALL kp_types_mod_complex_type(ctype, kgen_unit) 
                WRITE (*, *) "     value = ", niter 

                !print *, "t1(1)%elem1%c(:,:) = ", t1(1)%elem1%c(:,:)

                CLOSE (UNIT=kgen_unit) 
        end subroutine add

        !print state subroutine for kp_types_mod_complex_type__complex_type_dim1 
        SUBROUTINE kp_types_mod_complex_type__complex_type_dim1(var, kgen_unit) 
            TYPE(complex_type), INTENT(IN), DIMENSION(:) :: var 
            INTEGER, INTENT(IN) :: kgen_unit 
            LOGICAL :: kgen_istrue 
            REAL(KIND=8) :: kgen_array_sum 
            INTEGER :: idx1 
              
            IF (SIZE(var)==1) THEN 
                IF (UBOUND(var, 1)<LBOUND(var, 1)) THEN 
                    kgen_istrue = .FALSE. 
                ELSE IF (UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0) THEN 
                    kgen_istrue = .FALSE. 
                ELSE 
                    kgen_istrue = .TRUE. 
                END IF   
            ELSE IF (SIZE(var)==0) THEN 
                kgen_istrue = .FALSE. 
            ELSE 
                kgen_istrue = .TRUE. 
            END IF   
            IF (kgen_istrue) THEN 
                DO idx1=LBOUND(var,1), UBOUND(var,1) 
                    CALL kp_types_mod_complex_type(var(idx1), kgen_unit) 
                END DO   
            END IF   
              
        END SUBROUTINE kp_types_mod_complex_type__complex_type_dim1 
          
end module