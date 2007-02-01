c###
	function lnblnk(line)
	character*(*) line
	k=len(line)
	do i=k,1,-1
	if (line(i:i).ne.' ') goto 10
	enddo
10	lnblnk=i
	return
	end
