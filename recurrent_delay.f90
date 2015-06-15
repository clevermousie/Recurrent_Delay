!CHECK!
module file_variables
	integer ::fileid_int=10
	character(len=50) ::filename_int="incident_period.txt"
	integer ::fileid_tot=11
	character(len=50) ::filename_tot="./log/occupancy_station.txt"
	integer ::fileid_rec=13
	character(len=50) ::filename_rec="./log/VHT_RMSE_data.txt"
	integer ::fileid_delay=15
	integer ::fileid_delay2=16
	integer ::fileid_sta=17
	character(len=50) ::filename_sta="STA_ID.txt"
	integer ::fileid_inc=19
	character(len=50) ::filename_inc="incident_mp.txt"
	integer ::fileid_output=21
	character(len=50) ::filename_output="./log/recur_total_delay.txt"
	integer ::fileid_check=22
	character(len=50) ::filename_check="./log/check.txt"
    integer ::fileid_spe=23
    integer  ::fileid_spe2=24
    integer ::fileid_flow=25
    integer ::fileid_flow2=26
    integer  ::fileid_flowt=27
    integer  :: fileid_flowt2=28
end module
module variables
	integer i,j,k,ii
	real,pointer,dimension(:) ::time_coef1,time_coef2,station_coef,rec_delay
	integer station_num
	integer,pointer,dimension(:) ::station_id
	real, pointer,dimension(:) ::station_mp
	integer incident_num
	integer status_1
	real ::incident_mp(2000)
	integer valid_inc_num
	integer ::incident_id(2000),incident_doy(2000),incident_comp(2000)
	real	::incident_rmse(2000)
	real	temp_rmse
	integer total_incident_num
	integer ::tot_incident_id(2000), tot_incident_sta(2000),tot_start_moy(2000),tot_end_moy(2000),&
			tot_priority(2000),tot_severity(2000),tot_impact(2000)
	real	:: tot_delay(2000)
	character(len=20) ::up_sta_id_str,down_sta_id_str
	integer line_num_start,line_num_end,ref_line_num
	integer delay_period_count
	integer ::up_delay_doy(2000),up_delay_hour(2000),up_delay_minute(2000) 	
	integer ::down_delay_doy(2000),down_delay_hour(2000),down_delay_minute(2000)
	real	::up_delay_value(2000),down_delay_value(2000)
	integer found
	integer temp_doy
	integer temp_comp
    integer temp_day,temp_hour,temp_minute
    real    temp_spe,temp_spe2,temp_flow,temp_flow2,temp_flowt,temp_flowt2,speed,flow,flowt
    integer speed_count
	integer line_flow_start,line_flow_end
	integer inc_time_count
	integer ::inc_period(2000)
	integer inc_s_month,inc_s_day,inc_s_year,inc_s_hour,inc_s_minute,inc_e_month,inc_e_day,inc_e_year,inc_e_hour,&
		inc_e_minute
end module
program recurrent_delay
	use file_variables
	use variables
	implicit none
!****************************************read in incident time*************************************************
		open (fileid_int,file=filename_int)
		inc_time_count=0
		do while (.true.)
			read(fileid_int,*,iostat=status_1) inc_s_month,inc_s_day,inc_s_year,inc_s_hour,inc_s_minute,inc_e_month,&
					inc_e_day,inc_e_year,inc_e_hour,inc_e_minute
			if (status_1.lt.0) exit
			if ( inc_s_hour.gt.inc_e_hour) then
				inc_e_hour=inc_e_hour+24
			endif
			inc_time_count=1+inc_time_count
			inc_period(inc_time_count)=(inc_e_hour-inc_s_hour)*60+inc_e_minute-inc_s_minute

		enddo
!************************************** read in station information********************************************
		open (fileid_sta,file=filename_sta)
		read (fileid_sta,*) station_num
		write(*,*) "fileid_sta opened"

		allocate (station_mp(station_num))
		allocate (station_id(station_num))
		do i = 1, station_num
			read(fileid_sta,*) station_mp(i),station_id(i)
			write(*,*) station_mp(i),station_id(i)
		enddo
		close (fileid_sta)
		open (fileid_inc,file=filename_inc)
		incident_num=1
!*************************************read in incident mp and calculate station_coef***************************
		do while (.true.)
			read(fileid_inc,*,iostat=status_1) incident_mp(incident_num)
			if (status_1.lt.0) exit
			incident_num=incident_num+1
		enddo
		write(*,*) " inc mp is done"
		close (fileid_inc)
		allocate (station_coef(incident_num))
		allocate (time_coef1(incident_num))
		allocate (time_coef2(incident_num))
		allocate (rec_delay(incident_num))
		incident_num=incident_num-1
		do i = 1, incident_num
			do j = 1, station_num-1
!				write(*,*) station_mp(j),station_mp(j+1), incident_mp(i)

				if ((station_mp(j).le.incident_mp(i)).and.(station_mp(j+1).gt.incident_mp(i))) then
					station_coef(i)=(incident_mp(i)-station_mp(j))/(station_mp(j+1)-station_mp(j))
					write(*,*) station_coef(I),station_mp(j),station_mp(j+1),incident_mp(i)

				endif	

			enddo

		enddo
!**************************************** read in recurrent delay compared day******************************
		open (fileid_rec,file=filename_rec)
		valid_inc_num=1
		do while (.true.)
			read(fileid_rec,*,iostat=status_1) incident_id(valid_inc_num),incident_doy(incident_id(valid_inc_num))&
			,incident_comp(incident_id(valid_inc_num)), incident_rmse(incident_id(valid_inc_num))
			if (status_1.lt.0) exit
			write(*,*) incident_id(valid_inc_num),incident_comp(incident_id(valid_inc_num))
			valid_inc_num=valid_inc_num+1
		enddo
		valid_inc_num=valid_inc_num-1
		close (fileid_rec)
		write(*,*) "inc num for recurrent delay is ", valid_inc_num
!***************************************read in total delay file ******************************************
		open (fileid_tot,file=filename_tot)
		total_incident_num=1
		do while(.true.)
			read(fileid_tot,*,iostat=status_1) tot_incident_id(total_incident_num),tot_incident_sta(total_incident_num),&
							tot_start_moy(total_incident_num),&
							tot_end_moy(total_incident_num),tot_delay(total_incident_num),&
							tot_priority(total_incident_num),&
							tot_severity(total_incident_num),tot_impact(total_incident_num)
			if(status_1.lt.0) exit
			total_incident_num=total_incident_num+1
		enddo
		total_incident_num=total_incident_num-1
		close (fileid_tot)
		write(*,*) "inc num for total delay is",total_incident_num
!************************************* matching total delay with recurrent delay***************************
		open (fileid_output,file=filename_output)
		write(fileid_output,*) "inc_id rec_delay tot_delay non_rec_delay pri sev imp rmse"
		open (fileid_check,file=filename_check)
		do i = 1, total_incident_num
			write(*,*) "start to process incident ",tot_incident_id(i)
!			read(*,*)
			found=0
			do j = 1, valid_inc_num
				!write(*,*) incident_id(j)
				if (tot_incident_id(i).eq.incident_id(j)) then
				found=1
				write(up_sta_id_str,"(I6)") station_id(tot_incident_sta(i))
				write(down_sta_id_str,"(I6)") station_id(tot_incident_sta(i)+1)
				line_num_start=(incident_comp(incident_id(j))-1)*288+mod(tot_start_moy(i),1440)/5+1
				line_num_end=(incident_comp(incident_id(j))-1)*288+mod(tot_end_moy(i),1440)/5+2
                line_flow_start=(incident_doy(incident_id(j))-1)*288+mod(tot_start_moy(i),1440)/5+1
                line_flow_end=(incident_doy(incident_id(j))-1)*288+mod(tot_end_moy(i),1440)/5+2
				if (line_num_end.lt.line_num_start) then
					line_num_end=line_num_end+288
                endif
                if (line_flow_end.lt.line_flow_start)then
                    line_flow_end=line_flow_end+288
                endif
				ref_line_num=68*288+24
				if (line_num_start.gt.ref_line_num) then
					line_num_start=line_num_start-12
				endif
				if (line_num_end.gt.ref_line_num) then
					line_num_end=line_num_end-12
                endif
                if(line_flow_start.gt.ref_line_num) then
                    line_flow_start=line_flow_start-12
                endif
                if (line_flow_end.gt.ref_line_num) then
                    line_flow_end=line_flow_end-12
                endif
!				write(*,*) line_num_start,line_num_end
				write(*,*) incident_comp(incident_id(j))
				open (fileid_delay,file="./output/N_"//trim(adjustl(up_sta_id_str))//"_DEL60.txt")
				open (fileid_delay2,file="./output/N_"//trim(adjustl(down_sta_id_str))//"_DEL60.txt")
		                open (fileid_spe,file="./output/N_"//trim(adjustl(up_sta_id_str))//"_Speed.txt")
		                open (fileid_spe2,file="./output/N_"//trim(adjustl(down_sta_id_str))//"_Speed.txt")
               			open (fileid_flow,file="./output/N_"//trim(adjustl(up_sta_id_str))//"_FlowPL.txt")
		                open (fileid_flow2,file="./output/N_"//trim(adjustl(down_sta_id_str))//"_FlowPL.txt")
				open (fileid_flowt,file="./output/N_"//trim(adjustl(up_sta_id_str))//"_Flow.txt")
				open (fileid_flowt2,file="./output/N_"//trim(adjustl(down_sta_id_str))//"_Flow.txt")

				do ii = 1, line_num_start-1
					read(fileid_delay,*)
					read(fileid_delay2,*)
				enddo
	            do ii = 1, line_flow_start-1
			                    read(fileid_spe,*)
			                    read(fileid_spe2,*)
			                    read(fileid_flow,*)
			                    read(fileid_flow2,*)
					    read(fileid_flowt,*)
					    read(fileid_flowt2,*)
                enddo
				delay_period_count=line_num_end-line_num_start+1
		                speed_count=1
               			speed=0
		                flow=0
				flowt=0
				do ii= line_num_start, line_num_end
					read(fileid_delay,*) up_delay_doy(ii-line_num_start+1),up_delay_hour(ii-line_num_start+1),&
					up_delay_minute(ii-line_num_start+1),up_delay_value(ii-line_num_start+1)
					
					read(fileid_delay2,*)down_delay_doy(ii-line_num_start+1),down_delay_hour(ii-line_num_start+1),&
					down_delay_minute(ii-line_num_start+1),down_delay_value(ii-line_num_start+1)
					write(fileid_check,*) tot_incident_id(i),incident_comp(incident_id(j)),&
					up_delay_doy(ii-line_num_start+1),up_delay_hour(ii-line_num_start+1),&
					up_delay_minute(ii-line_num_start+1),up_delay_value(ii-line_num_start+1),&
					down_delay_value(ii-line_num_start+1)
					read(fileid_spe,*) temp_day,temp_hour,temp_minute,temp_spe
			                read(fileid_spe2,*) temp_day,temp_hour,temp_minute,temp_spe2
			                read(fileid_flow,*) temp_day,temp_hour,temp_minute,temp_flow
			                read(fileid_flow2,*) temp_day,temp_hour,temp_minute,temp_flow2
					read(fileid_flowt,*) temp_day,temp_hour,temp_minute,temp_flowt
					read(fileid_flowt2,*) temp_day,temp_hour,temp_minute,temp_flowt2
			                speed=(speed_count-1.0)*speed/speed_count+(temp_spe*(1-station_coef(i))+temp_spe2*station_coef(i))/speed_count
			                flow=(speed_count-1.0)*flow/speed_count+(temp_flow*(1-station_coef(i))+temp_flow2*station_coef(i))/speed_count
					flowt=(speed_count-1.0)*flowt/speed_count+(temp_flowt*(1-station_coef(i))+temp_flowt2*station_coef(i))/speed_count
					write(*,*)temp_day,temp_hour,temp_minute,temp_spe,temp_spe2,temp_flow,temp_flow2
                    
		                enddo
                
                
                
				time_coef1(i)=(tot_start_moy(i)-tot_start_moy(i)/5*5)/5.0
				time_coef2(i)=(tot_end_moy(i)-tot_end_moy(i)/5*5)/5.0
				write(*,*) time_coef1(i),time_coef2(i)
				rec_delay(i)=((up_delay_value(1)+up_delay_value(2))*(1-station_coef(i))+(down_delay_value(1)+&
						down_delay_value(2))*station_coef(i))/2*(1-time_coef1(i))

				do ii= 2, delay_period_count-1
					rec_delay(i)=rec_delay(i)+(up_delay_value(ii)*(1-station_coef(i))+station_coef(i)*down_delay_value(ii))
				enddo
				rec_delay(i)=rec_delay(i)+((up_delay_value(ii)+up_delay_value(ii+1))*(1-station_coef(i))+station_coef(i)*&
							(down_delay_value(ii)+down_delay_value(ii+1)))/2*time_coef2(i)
				temp_rmse=incident_rmse(incident_id(j))
				temp_doy=incident_doy(incident_id(j))
				temp_comp=incident_comp(incident_id(j))
				close(fileid_delay)
				close(fileid_delay2)
				close(fileid_spe)
				close(fileid_spe2)
				close(fileid_flow)
				close(fileid_flow2)
				close(fileid_flowt)
				close(fileid_flowt2)

				exit				
				endif
				
			enddo
			if (found.eq.1) then
				write(fileid_output,*) tot_incident_id(i),incident_mp(tot_incident_id(i)),temp_doy,temp_comp,rec_delay(i),&
							tot_delay(i),tot_delay(i)-rec_delay(i),tot_priority(i),&
							tot_severity(i),tot_impact(i),temp_rmse,flow,flowt,speed, inc_period(tot_incident_id(i)),&
							tot_end_moy(i)-tot_start_moy(i)
			else
				write(*,*) "no total delay for incident",tot_incident_id(i)
!				read(*,*)
			endif
		enddo
	close (fileid_output)
	close (fileid_check)
	close (fileid_rec)
	deallocate(station_mp,station_id,station_coef,time_coef1,time_coef2,rec_delay)		
end program
