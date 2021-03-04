! This function will grab the first token and return it as a number.  The string will get set to the remaining portion.
REAL FUNCTION PARSE_NUM(instr)
  CHARACTER*120 instr!, outstr
  INTEGER idx, ios

  ! Find the first blank in instr
  instr = adjustl(instr)
  idx = index(instr, " ")

  read(instr(:idx-1),fmt=*,iostat=ios) PARSE_NUM
  if (ios .gt. 0) then
     PARSE_NUM = -1
  else
     instr = instr(idx+1:)
  end if

  return
end function PARSE_NUM
