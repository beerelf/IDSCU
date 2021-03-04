      SUBROUTINE SORT(id, num, count)

C***************************************************************************
C
C   Function        : sort.f
C   Author          : LA Garcia
C   Date            : March 1996 
C   Purpose         : This sorts an integer array.
C   Calling program : monthly.f 
C   Called programs : none 
C   Input arguments : none 
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C
C***************************************************************************

C-----Local variable declaration

      INTEGER last, count, num, j, k, jj, ptr, first
      INTEGER id(num), hold
      last = count
      DO 10 j = 1, count - 1
        ptr = j
        first = j + 1
        jj = j+1

        DO 5 k = first, last
          IF(id(k).LT.id(ptr)) ptr = k
5       CONTINUE

        hold = id(j)
        id(j) = id(ptr)
        id(ptr) = hold
10    CONTINUE

      RETURN
      END

