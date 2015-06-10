!check!
module file_variables
	integer ::fileid_inc=11
	character(len=30) ::filename_inc="./crash_start.txt"
	integer ::fileid_up=13
	integer ::fileid_down=15
	integer ::fileid_sta=17
	character(len=30) ::filename_sta="STA_ID.txt"
	character(len=30) ::filename_speed
	character(len=30) ::filename_flow
	integer ::fileid_output=19
	character(len=30) ::filename_output="./log/bef_cra_VHT.txt"
	integer ::fileid_test=9
	character(len=30) ::filename_test="./log/test"
	integer ::fileid_free=7
end module
module variables
	integer i,j,k,JJ
	integer status_1,status_2
	integer crash_num,valid_crash_num
	integer,pointer,dimension(:) ::start_month,start_day,start_year,start_hour,start_minute,crash_avail,up_station,down_station
	integer,pointer,dimension(:) ::end_month,end_day,end_year,end_hour,end_minute,emoy,smoy
	real,pointer,dimension(:)	:: start_mp
	integer sta_num
	integer ::sta_id(100)
	real	::sta_mp(100)
	real	station_coef
	integer line_num,ref_num
	integer doy
	integer ::dom(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
	integer day_of_year1,hour1,minute1,day_of_year2,hour2,minute2
	real	value1,value2
	character(len=30) ::up_sta_str
	character(len=30) ::down_sta_str
	integer ::inc_id(7),inc_sta(7),inc_doy(7),inc_hour(7),inc_minute(7),inc_real(7)
	real	::inc_speed(7)
	integer check_j
	integer,pointer,dimension(:) ::inc_period1,inc_period2
!	part 3
	integer,pointer,dimension(:,:) ::target_id,target_up_sta,target_s_minute,target_doy,target_hour,target_minute
	real, pointer,dimension(:,:)	::target_speed,target_sta_coef
	integer	select_id,select_doy,select_comp
	real,pointer,dimension(:) ::rsme
	integer,pointer,dimension(:) ::rsme_doy
	real,pointer,dimension(:,:) ::rsme_speed
	integer,pointer,dimension(:,:) ::rsme_hour
	integer,pointer,dimension(:,:) ::rsme_minute
	real	up_value,down_value,temp_rsme
	integer up_doy,up_hour,up_minute,down_doy,down_hour,down_minute,up_doy1
	real	select_value
	real	::his_speed(7)
end module
!module fun_print
!end fun_print
program rsme_calculation
	use file_variables
	use variables
!	use fun_print
	implicit none
	! read in incident information	
		open (fileid_inc,file=filename_inc)
		crash_num=0

		do while (.true.)
			read(fileid_inc,*,iostat=status_1)
			if (status_1.lt.0) exit
			crash_num=crash_num+1
		end do
		close (fileid_inc)
		write(*,*) "there are ",crash_num," crashes"
		
		call ALL_ALLOCATE
!		call ALL_ALLOCATE2
		open (fileid_inc,file=filename_inc)

		valid_crash_num=0
		do i = 1, crash_num
			read (fileid_inc,*) start_month(i),start_day(i),start_year(i),start_hour(i),start_minute(i),start_mp(i),crash_avail(i)
			!write(*,*) crash_avail(i)
		enddo
		close (fileid_inc)
		write (*,*) "crash information is done"
	! read in station information
		open (fileid_sta,file=filename_sta)
		read (fileid_sta,*) sta_num
		do i = 1, sta_num
			read (fileid_sta,*) sta_mp(i),sta_id(i)
		enddo
		close (fileid_sta)
		write(*,*) "station_information is done"
	! station matching
		do i = 1, crash_num
			!if (mod(i,20).eq.0) then
			!	read(*,*)
			!endif
			if (crash_avail(i).eq.0)then
			valid_crash_num=valid_crash_num+1
			do j= 1,sta_num-1
				if ((sta_mp(j).le.start_mp(i)).and.(sta_mp(j+1).gt.start_mp(i)))then
					up_station(i)=j
					down_station(i)=j+1
					!write(*,*) "incident",i, up_station(i),down_station(i)
					exit
				endif
			enddo
			endif

		enddo
	! start read in flow information
		open (fileid_output,file=filename_output)
		do i = 1 , crash_num
			if (crash_avail(i).eq.0) then
				!write(*,*) up_station(i),down_station(i)
				write(up_sta_str,"(I6)") sta_id(up_station(i))
				write(down_sta_str,"(I6)") sta_id(down_station(i))
				station_coef=(start_mp(i)-sta_mp(up_station(i)))/(sta_mp(down_station(i))-sta_mp(up_station(i)))
				!write(*,*) trim(adjustl(up_sta_str)),' ',trim(adjustl(down_sta_str)),' ',station_coef
				open (fileid_up,file="./output/N_"//trim(adjustl(up_sta_str))//"_VHT.txt")
				open (fileid_down,file="./output/N_"//trim(adjustl(down_sta_str))//"_VHT.txt")
				doy=0
				if (start_month(i).ne.1) then

					do j = 1, start_month(i)-1
						doy=doy+dom(j)
					enddo
				endif
				doy=doy+start_day(i)
				line_num=(doy-1)*288+12*start_hour(i)+start_minute(i)/5+1
				ref_num=288*68+12*2+1
				if (line_num.ge.ref_num)then
					line_num=line_num-12
				endif
				do j = 1, line_num-7
					read(fileid_up,*)
					read(fileid_down,*)
				enddo
				do j =1 ,7
					!write(fileid_output,*) "incident",i,doy,start_hour(i),start_minute(i),station_coef
					read (fileid_up,*)day_of_year1,hour1,minute1,value1

					read (fileid_down,*) day_of_year2,hour2,minute2,value2
					!write(fileid_output,*)  up_station,day_of_year,hour,minute,value
					!write(fileid_output,*)
					!write(fileid_output,*) down_station,day_of_year,hour,minute,value
					!write(fileid_output,*)
					if ((day_of_year1.ne.day_of_year2).or.(hour1.ne.hour2).or.(minute1.ne.minute2)) then
						write(*,*) "DATA NOT MATCH"
						STOP
					ENDIF
					write(fileid_output,*) i, up_station(i),start_minute(i),day_of_year1,hour1,minute1,(1-station_coef)*value1&
						+station_coef*value2,station_coef
					write(fileid_output,*)
					!if (mod(i,20).eq.0) then
					!	read(*,*)
					!endif
				enddo	
				close (fileid_up)
				close (fileid_down)
			endif
		enddo
	close (fileid_output)
	!deallocate all the variables	
	deallocate(start_month,start_day,start_year,start_hour,start_minute,start_mp,up_station,down_station)
	call ALL_ALLOCATE
	call ALL_ALLOCATE2
	! read in crash duration file
	write(*,*) "valid crash number is", valid_crash_num
	!read(*,*)
	open (fileid_inc,file="incident_period.txt")
	open (fileid_test,file=filename_test)
	valid_crash_num=0
		do i = 1, crash_num
			read(fileid_inc,*) start_month(i),start_day(i),start_year(i),start_hour(i),&
				start_minute(i),end_month(i),end_day(i),end_year(i),end_hour(i),&
				end_minute(i),crash_avail(i)
			if(crash_avail(i).eq.0) then
				valid_crash_num=valid_crash_num+1
				write(*,*) valid_crash_num
			endif

			
			doy=0
			if (start_month(i).ne.1) then
				do j = 1, start_month(i)-1
					doy=doy+dom(j)
				enddo
			endif
			doy=doy+start_day(i)
			smoy(i)=(doy-1)*1440+60*start_hour(i)+start_minute(i)
			doy=0
			if (end_month(i).ne.1) then
				do j = 1, end_month(i)-1
					doy=doy+dom(j)
				enddo
			endif
			doy=doy+end_day(i)
			emoy(i)=(doy-1)*1440+60*end_hour(i)+end_minute(i)
			write(fileid_test,*) i,smoy(i),emoy(i),crash_avail(i)
		enddo
	!read(*,*)
	close (fileid_inc)
	write(*,*) valid_crash_num
	write (*,*) "************************************************START PART II**********************************************"
	!read(*,*)
!****************************************************************************************************************************
!****************************************************************************************************************************
	!read(*,*)
	! find available days
	open (fileid_inc,file=filename_output)
	open (fileid_output,file="./log/start_avail_day.txt")
		
		allocate (inc_period1(crash_num))
		allocate (inc_period2(crash_num))
		do i= 1, valid_crash_num
			
				
				!read(*,*)
				do j = 1, 7
					read(fileid_inc,*) inc_id(j),inc_sta(j),inc_real(j),inc_doy(j),inc_hour(j),inc_minute(j),inc_speed(j)
					write(*,*) inc_id(j),inc_sta(j),inc_doy(j),inc_hour(j),inc_minute(j),inc_speed(j)		
				!if (inc_id(1).gt.36) then
				!	read(*,*)
				!endif						
				enddo
				!if (inc_id(1).eq.36) then
				!	read(*,*)
				!endif
				inc_period1(i)=1440*(inc_doy(1)-1)+inc_hour(1)*60+inc_minute(1)
				inc_period2(i)=1440*(inc_doy(7)-1)+inc_hour(7)*60+inc_minute(7)

				!write(*,*) inc_period1(i),inc_period2(i),"period_detected"
				

					do j = 0, 288*365-7
						if (mod(j*5,1440*7).eq.mod(inc_period1(i),1440*7)) then
							check_j=0
							do jj= 0,6+(emoy(i)-smoy(i))/5+1
						
								do k= 1, crash_num
									if ((((j+jj)*5).ge. smoy(k)).and.((5*(jj+j)).lt.(emoy(k)+30))) then
										!write(*,*) j/288,(j-j/288*288)/12, mod(j,60)
										check_j=1
										exit
									endif
								enddo
								if (check_j.eq.1) exit
							end do
							if (check_j.eq.0) then
								write(fileid_output,*) inc_id(1),inc_doy(1),j/288+1
							endif
						endif
					enddo
				
			
		enddo
	close(fileid_output)
	close(fileid_inc)
	DEALLOCATE(inc_period1,inc_period2)
!**********************************************************************************************************************************
!**********************************************************************************************************************************
!	part 3 calculate the rsme
	write(*,*) "*************************************************START PART III**********************************************"
	!READ(*,*)
	CALL ALL_ALLOCATE3
	open (fileid_inc,file=filename_output)
	open (fileid_output,file="./log/VHT_RMSE_data.txt")
		do i = 1, valid_crash_num
			rsme(i)=1000000
			rsme_doy(i)=0
			do j = 1, 7
				read(fileid_inc,*) target_id(i,j),target_up_sta(i,j),target_s_minute(i,j),target_doy(i,j),target_hour(i,j),&
						target_minute(i,j),target_speed(i,j),target_sta_coef(i,j)
				write(*,*) target_id(i,j),target_doy(i,j),target_hour(i,j),target_minute(i,j)
			enddo
			if((target_doy(i,1).ne.69).or.((target_hour(i,1).ne.2).and.(target_hour(i,2).ne.2).and.(target_hour(i,3).ne.2).and.&
			(target_hour(i,4).ne.2).and.(target_hour(i,5).ne.2).and.(target_hour(i,6).ne.2).and.(target_hour(i,7).ne.2))) then

			write(up_sta_str,"(I6)") sta_id(target_up_sta(i,1))
			write(down_sta_str,"(I6)") sta_id(target_up_sta(i,1)+1)
			open (fileid_free,file="./log/start_avail_day.txt")

				do while (.true.)
					read (fileid_free,*,iostat=status_2) select_id,select_doy,select_comp
					if (status_2.lt.0) exit
					if ((select_id.eq.target_id(i,1)).and.(select_doy.ne.select_comp).and.&
						((select_comp.ne.69).or.((target_hour(i,1).ne.2).and.(target_hour(i,2).ne.2)&
							.and.(target_hour(i,3).ne.2).and.(target_hour(i,4).ne.2)&
							.and.(target_hour(i,5).ne.2).and.(target_hour(i,6).ne.2)&
							.and.(target_hour(i,7).ne.2)))) then
						open (fileid_up,file="./output/N_"//trim(adjustl(up_sta_str))//"_VHT.txt")
						open (fileid_down,file="./output/N_"//trim(adjustl(down_sta_str))//"_VHT.txt")
						line_num=(select_comp-1)*288+12*target_hour(i,1)+target_minute(i,1)/5+1
						ref_num=288*68+12*2+1
						if (line_num.gt.ref_num) then
							line_num=line_num-12
						endif
						do j = 1, line_num-1
							read(fileid_up,*)
							read(fileid_down,*)
						enddo
						temp_rsme=0
						do j = 1, 7
							read(fileid_up,*) up_doy,up_hour,up_minute,up_value
							if(j.eq.1) then
								up_doy1=up_doy
							endif
							!write(*,*) "up station", up_doy,up_hour,up_minute,target_doy(i,j),&
							!target_hour(i,j),target_minute(i,j)
			
							read(fileid_down,*) down_doy,down_hour,down_minute,down_value
							if ((up_hour.ne.target_hour(i,j)).or.(down_hour.ne.target_hour(i,j))&
							.or.(up_minute.ne.target_minute(i,j)).or.(down_minute.ne.target_minute(i,j))&
							.or.(up_doy.ne.down_doy).or.(select_comp.ne.up_doy1))then
								write (*,*) "LOCATION ERROR"
								write (*,*) up_hour,down_hour,up_minute,down_minute,up_doy,down_doy,target_hour(i,j),&
									target_minute(i,j),select_comp
								stop
							endif
							select_value=(1-target_sta_coef(i,j))*up_value+target_sta_coef(i,j)*down_value
							temp_rsme=temp_rsme+(select_value-target_speed(i,j))**2
							his_speed(j)=select_value

						enddo				
						temp_rsme=sqrt(temp_rsme/7.0)
						if (temp_rsme.lt.rsme(i)) then
							rsme(i)=temp_rsme
							rsme_doy(i)=select_comp
							do j = 1, 7
								rsme_speed(i,j)=his_speed(j)
								rsme_hour(i,j)=target_hour(i,j)
								rsme_minute(i,j)=target_minute(i,j)	
							enddo
						endif
						
						close (fileid_up)
						close (fileid_down)
					endif
				enddo
			endif
			close (fileid_free)
			write(fileid_output,*) target_id(i,1),target_doy(i,1),  rsme_doy(i),rsme(i)
			!write(fileid_output,*) target_speed(i,1:7)
!			write(fileid_output,*) rsme_speed(i,1:7)
!			write(fileid_output,*) rsme(I)
!			do j=1,7
!				write(fileid_output,*) target_id(i,j),rsme_hour(i,j),rsme_minute(i,j),target_speed(i,j),rsme_speed(i,j),rsme(i)
!			enddo
		enddo
	deallocate(target_id,target_up_sta,target_s_minute,target_hour,target_doy,target_minute,target_speed,target_sta_coef)
	deallocate (rsme_hour,rsme_minute)
	close (fileid_inc)
	close (fileid_output)
end program
subroutine ALL_ALLOCATE
use file_variables
use variables
implicit none
	allocate(start_month(crash_num))
	allocate (start_day(crash_num))
	allocate (start_year(crash_num))
	allocate (start_hour(crash_num))
	allocate (start_minute(crash_num))
	allocate (start_mp(crash_num))
	allocate (crash_avail(crash_num))
	allocate (up_station(crash_num))
	allocate (down_station(crash_num))
end subroutine
subroutine ALL_ALLOCATE2
use file_variables
use variables
implicit none
	allocate (end_month(crash_num))
	allocate (end_day(crash_num))
	allocate (end_year(crash_num))
	allocate (end_hour(crash_num))
	allocate (end_minute(crash_num))
	allocate (emoy(crash_num))
	allocate (smoy(crash_num))
end subroutine
subroutine ALL_ALLOCATE3
USE file_variables
use variables
implicit none
	allocate (target_id(valid_crash_num,7))
	allocate (target_up_sta(valid_crash_num,7))
	allocate (target_s_minute(valid_crash_num,7))
	allocate (target_doy(valid_crash_num,7))
	allocate (target_hour(valid_crash_num,7))
	allocate (target_minute(valid_crash_num,7))
	allocate (target_speed(valid_crash_num,7))
	allocate (target_sta_coef(valid_crash_num,7))
	allocate (rsme(valid_crash_num))
	allocate (rsme_doy(valid_crash_num))
	allocate (rsme_hour(valid_crash_num,7))
	allocate (rsme_minute(valid_crash_num,7))
	allocate (rsme_speed(valid_crash_num,7))
end subroutine

