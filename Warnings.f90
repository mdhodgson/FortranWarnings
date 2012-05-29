Module FortranWarnings
Implicit None
Private

Integer, Dimension(1:1000) :: Warnings=0
 CHARACTER*155, Dimension(1:100) :: Warning_message=""

Public :: WARN, WarnEnd,WarnInit,ERROR

contains
Subroutine WarnInit()
Print*, "Initialising Warnings"

Warning_message="Warning issued but message not found"
include 'warnings.txt'
End Subroutine

Subroutine Warn(X,limit,calls)
!Generates a warning for warning type X
Integer, Intent(in) :: X
Integer, Intent(in), Optional :: limit
Integer, Intent(out), Optional :: calls
if (Present(limit)) then
  Print*, Warnings(X)+1,limit
  if (Warnings(X)+1>limit) then
    Print*, "Exceeded limit of Warnings for Warning", X
    call Error(X)
  end if
end if
Warnings(X)=Warnings(X)+1
if (Warnings(X)==1) then
  Print*, "A warning has been triggered"
  Print*, Warning_message(X)
end if
if (Present(calls)) then
  calls=Warnings(X)
end if
End Subroutine

Subroutine Error(X)
Integer, Intent(in) :: X

Print*, "FATAL ERROR:"
  Print*, Warning_message(X)
Print*,"Additionally:"
call WarnEnd()
STOP
End Subroutine

Subroutine WarnEnd()
Integer :: n
If (SUM(Warnings)>0) then 
  Print*, "The following warnings were issued"
  Do n=1,100
    if (Warnings(n)>0) then
      Print*, "Warning", n
      Print*, "Issued ", Warnings(n), "times"
      Print*, "Message: ",Warning_message(n)
      Print*, "======"
    end if
  End Do
Else
  Print*, "No warnings issued"
End if
End Subroutine
End Module


