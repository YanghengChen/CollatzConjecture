program collatz_conjecture 
  implicit none
  character(len=10), dimension(2)  :: args !< Array to store two cmd arguments
  ! A structure to contain a number with its collatz sequence length
  type Pairs
    integer (kind = 8) :: num      !< The integer
    integer (kind = 8) :: sequLength  !< Collatz sequence length of num
  end type Pairs
  
  integer (kind = 8)  :: a1, a2, num, i, j, z
  integer (kind = 8)                :: temp  
  integer             :: stat        !< A variable for input checking
  type(Pairs)                  :: apair
  type(Pairs), dimension(10)   :: numpairs !< An array of structures
  
  ! Read the two command line arguments 
  call get_command_argument(1, args(1))
  call get_command_argument(2, args(2))
  ! Convert them into the required data types
  read(args(1),*,iostat=stat)a1

  read(args(2),*, iostat=stat)a2

  ! If inputs are ok, call a subroutine for further checking
    if (a1 > a2) then
    temp = a1
    a1 = a2
    a2 = temp
  end if
  
  ! Initializes the array with all struct fields as -1
  ! This is a way of marking empty indices so that it is not filled with garbage
  ! values.
  do num = 1, 10
    apair%num = -1
    apair%sequLength = -1
    numpairs(num) = apair
  end do

  i = 0
  do num = a1, a2
    apair%num = num
    apair%sequLength = find_sequence_iterator(num)  
    i = i + 1
    call add_to_array(numpairs, apair, i)       
  end do
 
  print*, "Sorted based on sequence length"
  call sort(numpairs, .true.)
   do j = 1, 10 
     if (numpairs(j)%num == -1) then
       exit
     end if
     print*, numpairs(j)
   end do
   
   
  print*, "Sorted based on integer size"
  call sort(numpairs, .false.)

   do z = 1, 10 
     if (numpairs(z)%num == -1) then
       exit
     end if
     print*, numpairs(z)
   end do
 
contains

!> Funtion that checks if a number is even.
function is_even(num) result(even)
  integer (kind = 8), intent(in) :: num
  logical :: even
  
  if (mod(num,2) == 0) then
    even = .true.
  else
    even = .false.
  end if
end function is_even

!> Funtion that finds the collatz sequence length of an integer
function find_sequence_iterator(num) result(counter)
  integer (kind = 8), intent(in) :: num
  integer (kind = 8)             :: counter, m 
  
  counter = 0
  m = num
  do while (m /= 1)
    counter = counter + 1
    if (is_even(m)) then
      m = m / 2
    else
      m = 3*m + 1
    end if
  end do
end function find_sequence_iterator


!> Subroutine that adds a Pairs struct into an array based
!  on some specific conditions
subroutine add_to_array(arr, elem, i)
  type(Pairs), dimension(10), intent(inout) :: arr
  type(Pairs), intent(in)                   :: elem
  integer (kind = 8), intent(inout)           :: i                    
  integer (kind = 8)                          :: x
  logical                                     :: same_length_found 
  
  same_length_found = .false.
  ! Adding first element
  if (i == 1) then 
    arr(i) = elem
  ! Adding to partially filled array
  else if (i <= 10) then
    do x = 1, i 
      if (elem%sequLength == arr(x)%sequLength) then
        same_length_found = .true.
        i = i - 1 ! Since current index will remain empty
        exit
      end if
    end do

    if (.not. same_length_found) then
      arr(i) = elem
    end if

    if (i == 10) then
      call sort(arr, .true.)
    end if
  ! When array is full (i > 10)
  else 
    do x = 1, 10
      if (elem%sequLength == arr(x)%sequLength) then
        same_length_found = .true.
        exit
      end if
    end do

    if ((.not. same_length_found) .and. (elem%sequLength > arr(10)%sequLength)) then
      arr(10) = elem
      call sort(arr, .true.)
    end if
  end if     
end subroutine add_to_array


    
!> Function that returns the location of the maximum in the section
!! between start and finish.
!! @note This is needed for sorting.
!! Reference: "https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/sorting.f90"
function  find_maximum(arr, start, finish, by_length) result(maxpos)
  type(Pairs), dimension(10), intent(in) :: arr
  integer, intent(in)                      :: start, finish
  logical, intent(in)                      :: by_length
  integer                                  :: location, i, maxpos
  double precision                         :: maximum

  if (by_length) then
    maximum  = arr(start)%sequLength
  else 
    maximum = arr(start)%num
  end if       
  location = start                        
  do i = start+1, finish
    ! For sorting by sequence length
    if (by_length) then               
      if (arr(i)%sequLength > maximum) then    
        maximum  = arr(i)%sequLength          
        location = i                        
      end if
    ! For sorting by integer size
    else
      if (arr(i)%num > maximum) then
        maximum = arr(i)%num
        location = i
      end if
    end if
  end do
  maxpos = location                      
end function  find_maximum

!> Subroutine that swaps the values of its two arguments.
!! The elements that are to be swapped are of the Pairs structure.
!! @note This is needed for sorting.
subroutine  swap(a, b)
  type(Pairs), intent(inout) :: a, b
  type(Pairs)                :: temp

  temp = a
  a = b
  b = temp
end subroutine  swap

!> Subroutine that receives an array and sorts it in descending order.
!! Reference: "https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/sorting.f90"
subroutine  sort(arr, by_length)
  type(Pairs), dimension(10), intent(inout) :: arr
  integer, parameter                          :: s = 10  !> Array size is fixed
  logical, intent(in)                         :: by_length
  integer                                     :: i, location
  
  do i = 1, s-1                                           ! Exclude the last
    location = find_maximum(arr, i, s, by_length)         ! Find maximum from this to last
    call swap(arr(i), arr(location))                      ! Swap this and the maximum
  end do
end subroutine sort

end program collatz_conjecture
