

module types_mod

        implicit none

        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 4
        type, public :: level_3

                real(real_kind) :: param1(np,np)
                real(real_kind) :: param2(np,np)

        end type level_3

        type, public :: elem_mimic

                        real(kind=real_kind), dimension(np,np) :: a
                        real(kind=real_kind), dimension(np,np) :: b
                        real(kind=real_kind), dimension(np,np) :: c

        end type elem_mimic

        type, public :: elem_mimic2
 
                        real(kind=real_kind) :: d
                        real(kind=real_kind) :: e
                        real(kind=real_kind) :: f
                        
                        type(level_3) :: level3_1

        end type elem_mimic2


        type, public :: complex_type

                real(kind=real_kind) :: element_1

                type(elem_mimic) :: elem1

                type(elem_mimic2) :: elem2

        end type complex_type

    type(complex_type), target :: ctype
    type(complex_type), target, dimension(2) :: atype

    public ctype, atype

    PUBLIC kp_types_mod_level_3 
    PUBLIC kp_types_mod_elem_mimic 
    PUBLIC kp_types_mod_elem_mimic2 
    PUBLIC kp_types_mod_complex_type 
      
    CONTAINS 
      
    !read state subroutine for kp_types_mod_level_3 
    RECURSIVE SUBROUTINE kp_types_mod_level_3(var, kgen_unit) 
        TYPE(level_3), INTENT(IN) :: var 
        INTEGER, INTENT(IN) :: kgen_unit 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        WRITE (*, *) "     REAL(SUM(param1), 8) = ", kgen_array_sum 
          
        WRITE (*, *) "     REAL(SUM(param2), 8) = ", kgen_array_sum 
          
    END SUBROUTINE kp_types_mod_level_3 
      
    !read state subroutine for kp_types_mod_elem_mimic 
    RECURSIVE SUBROUTINE kp_types_mod_elem_mimic(var, kgen_unit) 
        TYPE(elem_mimic), INTENT(IN) :: var 
        INTEGER, INTENT(IN) :: kgen_unit 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        WRITE (*, *) "     REAL(SUM(a), 8) = ", kgen_array_sum 
          
        WRITE (*, *) "     REAL(SUM(b), 8) = ", kgen_array_sum 
          
        WRITE (*, *) "     REAL(SUM(c), 8) = ", kgen_array_sum 
          
    END SUBROUTINE kp_types_mod_elem_mimic 
      
    !read state subroutine for kp_types_mod_elem_mimic2 
    RECURSIVE SUBROUTINE kp_types_mod_elem_mimic2(var, kgen_unit) 
        TYPE(elem_mimic2), INTENT(IN) :: var 
        INTEGER, INTENT(IN) :: kgen_unit 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        WRITE (UNIT=kgen_unit) "     value = ", var%d 
          
        WRITE (UNIT=kgen_unit) "     value = ", var%e 
          
        WRITE (UNIT=kgen_unit) "     value = ", var%f 
          
        CALL kp_types_mod_level_3(var%level3_1, kgen_unit) 
          
    END SUBROUTINE kp_types_mod_elem_mimic2 
      
    !read state subroutine for kp_types_mod_complex_type 
    RECURSIVE SUBROUTINE kp_types_mod_complex_type(var, kgen_unit) 
        TYPE(complex_type), INTENT(IN) :: var 
        INTEGER, INTENT(IN) :: kgen_unit 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        WRITE (UNIT=kgen_unit) "     value = ", var%element_1 
          
        CALL kp_types_mod_elem_mimic(var%elem1, kgen_unit) 
          
        CALL kp_types_mod_elem_mimic2(var%elem2, kgen_unit) 
          
    END SUBROUTINE kp_types_mod_complex_type 
      
end module types_mod
 