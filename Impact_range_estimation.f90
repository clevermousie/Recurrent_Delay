!CHECK!

module file_variables
    integer ::fileid_inc=11
    integer ::fileid_up=13
    integer ::fileid_down=15
    character(len=30) ::filename_inc="./log/output.txt"
    integer ::fileid_output=9
    integer ::fileid_collect=17
    character(len=30) ::filename_output="./log/incident_impact.txt"
    character(len=30) ::filename_collect="./log/impact_collect.txt"
    ! FORT 3,4 ARE OCCUPIED
    end module
module variables
    integer i,j,k
    integer incident_num    ! total number of incidents
    integer last_incident   ! for new incident detection
    integer last_down_station,last_up_station
    integer incident_count  ! count the number of compared days for each incident
    integer t_incident_id,t_incident_day,t_com_day,t_hour,t_minute,t_up_sta,t_down_sta
    real    t_station_coef,t_del_60    ! for incident input file
    real    station_coef    ! station_coef for each incident
    integer status_1        ! status for reading incident input file
    real    sum_delay       ! total delay for each incident
    real    sum_delay_sq
    real    :: inc_delay(60)! delay for each compared day 
    integer :: inc_doy(60),inc_hour(60),inc_minute(60)
    real    chi             ! parameter for distribution estimation
    real    :: chi_square(91)! chi-square table
    real    max_delay       ! delay range from the parameter estimation 95%
    character(len=30) ::up_station_str,down_station_str ! up station, down station ids
    real time_coef          ! time coefficient for the neighbour time period
    integer line_num        ! where to start reading available data
    integer ref_line        ! the reference line
    integer temp1,temp2,temp3 ! dummy for reading
    real del_up_bef,del_up_aft,del_down_bef,del_down_aft ! upstream, downstream, before incident, after incident delay
    real end_inc_del        ! delay when the incident ended
    integer period_count    ! count the number of period till when the delay vanished
    real    before_period,after_period !the delay before & after each selected segment
    integer before_doy,before_hour,before_minute,after_doy,after_hour,after_minute ! time for before and after period time points
    integer up_doy,up_hour,up_minute,down_doy,down_hour,down_minute! time for upstream and downstream station
    real    up_del,down_del! delay for up and down stream while looking for the last period
    real    temp_time_coef ! temporary time coefficient for interpolation in each time period
    real    temp_delay     ! temporary delay for test interpolation 
    integer ::station_id(100) ! store the station id information
    real    ::station_mp(100) ! store the station milepost information
    integer station_number    !the total number of stations
    integer test_int1,test_int2,test_int3
    integer test_real1
    
    integer	test_minute1,test_minute2,test_doy,test_dow ! test value for each minute
    real	test_delay	! calculated delay for select minute
    integer	up_found,down_found	! flags for upstream & downstream stations locating
    integer	status_3	! read flag for station_dis.txt
    integer	read_dow,read_minute,read_sta,read_day_count	! read data from station_dis.txt
    real	read_sum_del	! raed data form station_dis.txt
    real	read_sum_del_sq 
    Real	up_sum_del,down_sum_del ! upstream & downstream max delay
    real	up_sum_del_sq,down_sum_del_sq
    integer	up_day_count,down_day_count ! number of available days for upstream station and downstream station
    real	test_max_delay	! max delay for select minute
    integer	final_found	! flag for whether find the minute
    end module


    program distribution_estimator
    use file_variables
    use variables
    implicit none

    ! INITIAL INCIDENT NUMBER
    incident_num=1
    last_incident=0
    incident_count=0

    ! READ IN STATION INFORMATION
    open(fileid_collect,file=filename_collect)
    open (4,file="STA_ID.txt")
    read (4,*) station_number
    do i = 1, station_number
        read(4,*) station_mp(i),station_id(i)
    enddo
    close (4)

    ! READ IN CHI-SQUARE TABLE
    open (4,file="Chi_Square.txt")
    do i = 1, 91
        read (4,*) chi_square(i)
    enddo
    close(4)

    ! START TO PROCESS INCIDENT INFORMATION
    open (fileid_inc,file=filename_inc)
    open (fileid_output,file=filename_output)


    do while(.true.)
        read (fileid_inc,*,iostat=status_1) t_incident_id,t_incident_day,t_com_day,t_hour,&
		t_minute,t_up_sta,t_down_sta,t_station_coef,t_del_60

         ! THREE CONDITIONS: 1. FILE DOESN'T END, 2. NOT FIRST LINE 3. INCIDENT ID CHANGED
        

	 if (((t_incident_id.ne.last_incident).and.(last_incident.ne.0)).or.(status_1.lt.0)) then

	    ! CALCULATE TOTAL DELAY FOR LAST INCIDENT
            sum_delay=0
            sum_delay_sq=0 
            do i = 1, incident_count
                sum_delay=sum_delay+inc_delay(i)
		sum_delay_sq=sum_delay_sq+inc_delay(i)*inc_delay(i)
            enddo
        ! CALCULATE THE START MINUTE OF THE CRASH

	    ! DISTRIBUTION FILTER, IF TOO LITTLE DATA AVAILABLE, ASSUME THE DELAY IS ZERO

		max_delay=min(1.33*sum_delay/incident_count,1.14*sqrt(sum_delay_sq/incident_count))            
                !max_delay is available, locate incident end time
            
	    write(up_station_str,"(I6)") station_id(last_up_station)
            write(down_station_str,"(I6)") station_id(last_down_station)
	    write(*,*) up_station_str,down_station_str

            write(*,*) LAST_INCIDENT,"UP STATION IS", trim(UP_STATION_STR)," DOWN STATION IS ",trim(DOWN_STATION_STR)
            write(fileid_collect,*) LAST_INCIDENT,(inc_doy(incident_count)-1)*1440+&
		60*inc_hour(incident_count)+inc_minute(incident_count)

	! TWO CASES: 1. INTERPOLATION IN SPATIAL 2. NO SPATIAL INTERPOLATION

            if (last_up_station.ne.last_down_station) then

 
                open (fileid_up,file="./output/N_"//trim(adjustl(up_station_str))//"_DEL60.txt")
                open (fileid_down,file="./output/N_"//trim(adjustl(down_station_str))//"_DEL60.txt")
 
                time_coef=(inc_minute(incident_count)-inc_minute(incident_count)/5*5)/5.0
                line_num=(inc_doy(incident_count)-1)*288+12*inc_hour(incident_count)+inc_minute(incident_count)/5+1
                
                ref_line=68*288+12*2
                if (line_num.gt.ref_line) then
                    line_num=line_num-12
                endif

                do i = 1, line_num-1
                    read (fileid_up,*) 
                enddo

                do i = 1, line_num-1
                    read (fileid_down,*)
                enddo
                
                
                read (fileid_up,*) temp1,temp2,temp3,del_up_bef
                read (fileid_down,*) temp1,temp2,temp3,del_down_bef
                read (fileid_up,*) temp1,temp2,temp3,del_up_aft
                read (fileid_down,*) temp1,temp2,temp3,del_down_aft
             	
		! CALCULATE THE DELAY AT THE INCIDENT SPOT WHEN INCIDENT ENDED
                end_inc_del=(del_up_bef*(1-time_coef)+time_coef*del_up_aft)*(1-station_coef)+&
		station_coef*(del_down_bef*(1-time_coef)+time_coef*del_down_aft)
                
                if (end_inc_del.le.max_delay) then
		! IF THE DELAY WHEN THE INCIDENT ENDED IS IN THE ACCEPTABLE RANGE
                    
                    write(fileid_output,*) "1",last_incident,5*288*(inc_doy(incident_count)-1)+5*12*inc_hour(incident_count)&
			+inc_minute(incident_count), 5*288*(inc_doy(incident_count)-1)+5*12*inc_hour(incident_count)+&
			inc_minute(incident_count),last_up_station,last_down_station
                    close(fileid_up)
                    close(fileid_down)
                else
		! IF THE DELAY WHEN THE INCIDENT ENDED IS NOT IN THE RANGE 
		
                    period_count=0
                    before_period=end_inc_del
                    after_period=(1-station_coef)*del_up_aft+station_coef*del_down_aft
                    before_doy=inc_doy(incident_count)
                    before_hour=inc_hour(incident_count)
                    before_minute=inc_minute(incident_count)
                    after_doy=temp1
                    after_hour=temp2
                    after_minute=temp3
			
		
                    final_found=0                    
                    test_minute1=60*before_hour+before_minute+1
		! THE MINUTE OF DAY AT NEW PERIOD STARTING POINT
                    
                    if(after_hour.lt.before_hour) then
		! IF THE PERIOD ENDS IN NEXT DAY
                        test_doy=before_doy
                        test_dow=mod(before_doy+1,7)
                        test_minute2=1441    
                    else
                        test_doy=after_doy
                        test_dow=mod(after_doy+1,7)
                        test_minute2=60*after_hour+after_minute+1
                    endif
                ! TEST EVERY MINUTE FROM THE MINUTE CRASH ENDED AND NEXT LOOP DETECT TIME POINT
                    do i = test_minute1,test_minute2-1
                        time_coef=1.0*(i-test_minute1)/(test_minute2-test_minute1)
                        test_delay=(1-time_coef)*before_period+time_coef*after_period
                        open (3,file="./log/stations_dis.txt")
                        up_found=0
                        down_found=0
                        ! SEARCH THE DATABASE FILE FOR THE INCIDENT FREE DELAY DISTRIBUTION
                        do while (.true.)
                            read (3,*,iostat=status_3) read_dow,read_minute,read_sta,read_sum_del,read_sum_del_sq,read_day_count
                            if (status_3.lt.0) then
                                write(*,*) " ERROR CANNOT LOCATE THE MINUTE AND STATION",test_dow,i,station_id(last_up_station)
                                stop
                            endif
                            if ((read_dow.eq.test_dow).and.((read_minute+1).eq.i).and.&
							(station_id(last_up_station).eq.read_sta)) then
                                
                                up_sum_del=read_sum_del
                                up_day_count=read_day_count
                                up_found=1
				up_sum_del=read_sum_del_sq
                            endif
                            if ((read_dow.eq.test_dow).and.((read_minute+1).eq.i).and.&
							(station_id(last_down_station).eq.read_sta))then
                                down_sum_del=read_sum_del
                                down_day_count=read_day_count
                                down_found=1
				down_sum_del=read_sum_del_sq
                            endif
                            if((up_found.eq.1).and.(down_found.eq.1)) exit
                        enddo
                        close(3)
                        

			test_max_delay=1.33*(up_sum_del*(1-station_coef)+down_sum_del*station_coef)/up_day_count
			test_max_delay=min(test_max_delay,1.14/sqrt(1.0*up_day_count)*&
					sqrt(up_sum_del_sq*(1-station_coef)+down_sum_del_sq*station_coef))
                        WRITE(*,*) "1",TEST_MAX_DELAY, TEST_DELAY
                        WRITE(fileid_collect,*) "1",TEST_MAX_DELAY, TEST_DELAY
                        if ((test_delay.le.test_max_delay).or.(test_delay.le.1e-6)) then
                            final_found=1
                            write(fileid_output,*) "3",last_incident,5*288*(inc_doy(incident_count)-1)+&
				5*12*inc_hour(incident_count)+inc_minute(incident_count),&
				(test_doy-1)*288*5+i,last_up_station,last_down_station
                            exit
                        endif
                    enddo
                    write(*,*) "START TO LOCATE NEW PERIOD"
                    
                    do while ((final_found.eq.0).and.(period_count.le.7).and.((288*(up_doy-1)+12*up_hour+up_minute/5).lt.105107))
                        
                        period_count=period_count+1
                        read(fileid_up,*) up_doy,up_hour,up_minute,up_del
                        read(fileid_down,*) down_doy,down_hour,down_minute,down_del
                        
                        before_period=after_period
                        before_doy=after_doy
                        before_hour=after_hour
                        before_minute=after_minute
                        after_doy=down_doy
                        after_hour=down_hour
                        after_minute=down_minute
                        after_period=(1-station_coef)*up_del+station_coef*down_del
                        
                        test_minute1=60*before_hour+before_minute+1
                    
                        if(after_hour.lt.before_hour) then
                            test_doy=before_doy
                            test_dow=mod(before_doy+1,7)
                            test_minute2=1441    
                        else
                            test_doy=after_doy
                            test_dow=mod(after_doy+1,7)
                            test_minute2=60*after_hour+after_minute+1
                        endif
                        
                        do i = test_minute1,test_minute2-1
                            time_coef=1.0*(i-test_minute1)/(test_minute2-test_minute1)
                            test_delay=(1-time_coef)*before_period+time_coef*after_period
                            open (3,file="./log/stations_dis.txt")
                            up_found=0
                            down_found=0
                            do while (.true.)
                                read (3,*,iostat=status_3) read_dow,read_minute,read_sta,read_sum_del,read_sum_del_sq,read_day_count
                                if (status_3.lt.0) then
                                    write(*,*) " ERROR CANNOT LOCATE THE MINUTE AND STATION",test_dow,i,station_id(last_up_station)
                                    stop
                                endif
                                if ((read_dow.eq.test_dow).and.((read_minute+1).eq.i).and.&
						(station_id(last_up_station).eq.read_sta)) then
                                    
                                    up_sum_del=read_sum_del
                                    up_day_count=read_day_count
                                    up_found=1
                      		    up_sum_del_sq=read_sum_del_sq 
			         endif
                                if ((read_dow.eq.test_dow).and.((read_minute+1).eq.i).and.&
						(station_id(last_down_station).eq.read_sta))then
                                    down_sum_del=read_sum_del
                                    down_day_count=read_day_count
                                    down_found=1
				    down_sum_del_sq=read_sum_del_sq
                                endif
                                if((up_found.eq.1).and.(down_found.eq.1)) exit
                            enddo
                            close(3)

			    test_max_delay=1.3*(up_sum_del*(1-station_coef)+down_sum_del*station_coef)/up_day_count
			    test_max_delay=min(test_max_delay,1.14/sqrt(1.0*up_day_count)*&
				sqrt(up_sum_del_sq*(1-station_coef)+down_sum_del_sq&
						*station_coef))
                            WRITE(*,*) "2",TEST_MAX_DELAY,TEST_DELAY
                            WRITE(fileid_collect,*) "2",TEST_MAX_DELAY,TEST_DELAY
                            if ((test_delay.le.test_max_delay).or.(test_delay.le.1e-6)) then
                                final_found=1
                                write(fileid_output,*) "3",last_incident,5*288*(inc_doy(incident_count)-1)+&
				5*12*inc_hour(incident_count)+inc_minute(incident_count),&
					(test_doy-1)*288*5+i,last_up_station,last_down_station
                                exit
                            endif
                        enddo
                    enddo
                    if (((288*(up_doy-1)+12*up_hour+up_minute/5).ge.105107).or.(period_count.gt.7)) then
                        write(fileid_output,*) "2",last_incident,5*288*(inc_doy(incident_count)-1)+&
			5*12*inc_hour(incident_count)+inc_minute(incident_count),(after_doy-1)*288*5+&
			5*12*after_hour+after_minute,last_up_station,last_down_station
                        close(fileid_up)
                        close(fileid_down)
                    endif
                    
                    close(fileid_up)
                    close(fileid_down)
                    
                endif
                
            else
		write(*,*) trim(adjustl(up_station_str))
                open (fileid_up,file="./output/N_"//trim(adjustl(up_station_str))//"_DEL60.txt")
                time_coef=(inc_minute(incident_count)-inc_minute(incident_count)/5*5)/5.0
                line_num=(inc_doy(incident_count)-1)*288+12*inc_hour(incident_count)+inc_minute(incident_count)/5
                ref_line=68*288+12*2
                if (line_num.gt.ref_line) then
                    line_num=line_num-12
                endif
                do i = 1, line_num-1
                    read (fileid_up,*) 
                enddo
                read (fileid_up,*) temp1,temp2,temp3,del_up_bef
                read (fileid_up,*) after_doy,after_hour,after_minute,del_up_aft
                end_inc_del=(1-time_coef)*del_up_bef+time_coef*del_up_aft
                if (end_inc_del.le.max_delay) then
                    write(fileid_output,*) "1",last_incident,5*288*(inc_doy(incident_count)-1)+&
			5*12*inc_hour(incident_count)+inc_minute(incident_count),&
			 5*288*(inc_doy(incident_count)-1)+12*5*inc_hour(incident_count)+&
			inc_minute(incident_count),last_up_station,last_down_station
                    close(fileid_up)
                else
                       
                    period_count=0
                    before_period=end_inc_del
                    after_period=del_up_aft
                    before_doy=inc_doy(incident_count)
                    before_hour=inc_hour(incident_count)
                    before_minute=inc_minute(incident_count)

                    final_found=0                    
                    test_minute1=60*before_hour+before_minute+1
                    
                    if(after_hour.lt.before_hour) then
                        test_doy=before_doy
                        test_dow=mod(before_doy+1,7)
                        test_minute2=1441    
                    else
                        test_doy=after_doy
                        test_dow=mod(after_doy+1,7)
                        test_minute2=60*after_hour+after_minute+1
                    endif

                    do i = test_minute1,test_minute2-1
                        time_coef=1.0*(i-test_minute1)/(test_minute2-test_minute1)
                        test_delay=(1-time_coef)*before_period+time_coef*after_period
                        open (3,file="./log/stations_dis.txt")
                        up_found=0
                        down_found=0
                        do while (.true.)
                            read (3,*,iostat=status_3) read_dow,read_minute,read_sta,read_sum_del,read_sum_del_sq,read_day_count
                            if (status_3.lt.0) then
                                write(*,*) " ERROR CANNOT LOCATE THE MINUTE AND STATION",test_dow,i,station_id(last_up_station)
                                stop
                            endif
                            if ((read_dow.eq.test_dow).and.((read_minute+1).eq.i).and.&
							(station_id(last_up_station).eq.read_sta)) then
                                
                                up_sum_del=read_sum_del
                                up_day_count=read_day_count
                                up_found=1
				up_sum_del_sq=read_sum_del_sq
                            endif

                            if((up_found.eq.1)) exit
                        enddo
                        close(3)

			test_max_delay=1.3*up_sum_del/up_day_count
			test_max_delay=min(1.14*sqrt(up_sum_del_sq/up_day_count),test_max_delay)
                        WRITE(*,*) "3",TEST_MAX_DELAY,TEST_DELAY
                        WRITE(fileid_collect,*) "3",TEST_MAX_DELAY,TEST_DELAY
                        if ((test_delay.le.test_max_delay).or.(test_delay.le.1e-6)) then
                            final_found=1
                            write(fileid_output,*) "3",last_incident,5*288*(inc_doy(incident_count)-1)+&
				5*12*inc_hour(incident_count)+inc_minute(incident_count),(test_doy-1)*288*5+i,&
				last_up_station,last_down_station
                            exit
                        endif
                    enddo
                    
                    
                    write(fileid_collect,*) "START TO LOCATE NEW TIME PERIOD"
                    do while ((final_found.eq.0).and.(period_count.le.7).and.((288*(up_doy-1)+12*up_hour+up_minute/5).lt.105107))
                        period_count=period_count+1
                        read(fileid_up,*) up_doy,up_hour,up_minute,up_del
                        before_period=after_period
                        
                        after_period=(1-station_coef)*up_del+station_coef*down_del
                            
                        before_doy=after_doy
                        before_hour=after_hour
                        before_minute=after_minute
                        after_doy=up_doy
                        after_hour=up_hour
                        after_minute=up_minute
                        
                        test_minute1=60*before_hour+before_minute+1
                    
                        if(after_hour.lt.before_hour) then
                            test_doy=before_doy
                            test_dow=mod(before_doy+1,7)
                            test_minute2=1441    
                        else
                            test_doy=after_doy
                            test_dow=mod(after_doy+1,7)
                            test_minute2=60*after_hour+after_minute+1
                        endif
                        
                        do i = test_minute1,test_minute2-1
                            time_coef=1.0*(i-test_minute1)/(test_minute2-test_minute1)
                            test_delay=(1-time_coef)*before_period+time_coef*after_period
                            open (3,file="./log/stations_dis.txt")
                            up_found=0
                            down_found=0
                            do while (.true.)
                                read (3,*,iostat=status_3) read_dow,read_minute,read_sta,read_sum_del,read_sum_del_sq,read_day_count
                                if (status_3.lt.0) then
                                    write(*,*) " ERROR CANNOT LOCATE THE MINUTE AND STATION",test_dow,i,station_id(last_up_station)
                                    stop
                                endif
                                if ((read_dow.eq.test_dow).and.((read_minute+1).eq.i).and.&
							(station_id(last_up_station).eq.read_sta)) then
                                    
                                    up_sum_del=read_sum_del
                                    up_day_count=read_day_count
                                    up_found=1
				    up_sum_del_sq=read_sum_del_sq
                                endif
                                if(up_found.eq.1) exit
                            enddo
                            close(3)

			    test_max_delay=1.3*up_sum_del/up_day_count
			    test_max_delay=min(1.14*sqrt(up_sum_del_sq/up_day_count),test_max_delay)
                            WRITE(*,*) "4",TEST_MAX_DELAY,TEST_DELAY
                            WRITE(fileid_collect,*) "4",TEST_MAX_DELAY,TEST_DELAY
                            if ((test_delay.le.test_max_delay).or.(test_delay.le.1e-6)) then
                                final_found=1
                                write(fileid_output,*) "3",last_incident,5*288*(inc_doy(incident_count)-1)+&
				5*12*inc_hour(incident_count)+inc_minute(incident_count),&
				(test_doy-1)*288*5+i,last_up_station,last_down_station
                                exit
                            endif
                        enddo
                        
                        
                    enddo
                    if (((288*(up_doy-1)+12*up_hour+up_minute/5).ge.105107).or.(period_count.gt.7)) then
                        write(fileid_output,*) "2",last_incident,5*288*(inc_doy(incident_count)-1)+&
			5*12*inc_hour(incident_count)+inc_minute(incident_count),(after_doy-1)*288*5+&
			5*12*after_hour+after_minute,last_up_station,last_down_station
                        close(fileid_up)
                    endif
                endif
                
            endif
            if (status_1.lt.0) exit
            last_incident=t_incident_id
            last_up_station=t_up_sta
            last_down_station=t_down_sta
            incident_count=1
            inc_doy(incident_count)=t_incident_day
            inc_delay(incident_count)=t_del_60
            station_coef=t_station_coef
        else
           incident_count=incident_count+1
           inc_delay(incident_count)=t_del_60
           inc_hour(incident_count)=t_hour
           inc_minute(incident_count)=t_minute
           inc_doy(incident_count)=t_incident_day
           station_coef=t_station_coef
           last_up_station=t_up_sta
           last_down_station=t_down_sta
           last_incident=t_incident_id
        endif
    enddo
    close(fileid_output)
    close(fileid_inc)
    
    close(fileid_collect)
    end program distribution_estimator
