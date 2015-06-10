!  Total_delay.f90 
!
!  FUNCTIONS:
!  Total_delay - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Total_delay
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
module file_variables
    integer ::fileid_crash=11
    integer ::fileid_sta=13
    character(len=30) ::filename_crash="crash_input.txt"
    character(len=30) ::filename_sta="STA_ID.txt"
    integer ::fileid_out_day=9
    character(len=30) ::filename_out_day="./log/day_long.txt"
    integer ::fileid_out_fea=7
    character(len=30) ::filename_out_fea="./log/output.txt"
    end module
module variables
    integer i,j,k
    integer crash_num
    integer status_1,status_2
    character(len=120) ::dummy
    character(len=15),pointer, dimension(:) ::crash_name,start_date,end_date,crash_severity,start_time,end_time
    integer, pointer,dimension(:) ::crash_impact, crash_priority,crash_mp
    integer dummy_len,flag
    integer,pointer,dimension(:) ::crash_year,crash_month1,crash_month2,crash_day1,crash_day2,crash_hour1,&
		crash_hour2,crash_minute1,crash_minute2,crash_dow1,crash_doy1,crash_dow2,crash_doy2,overlap
    integer dow
    integer ::doy_avail(365)
    integer select_minute1,select_minute2,target_minute1,target_minute2
    integer,pointer,dimension(:) ::station_id
    real,pointer,dimension(:) ::station_mp
    integer station_flag,up_station,down_station,time_flag,up_time,down_time
    integer sta_num,line_num,ref_line_num
    real station_coef,time_coef
    integer temp1,temp2,temp3
    real temp4,temp5,temp6,temp7,flow_ub,flow_ua,flow_db,flow_da
    character(len=30) ::up_station_str,down_station_str
end module
    program Total_delay
    use file_variables
    use variables
    implicit none
    integer,external ::cal_day_of_year
    integer,external ::cal_day_of_week
!**************************************************READ CRASH DATA*****************************    
    open (fileid_crash,file=filename_crash)
    crash_num=0
    do while (.true.)
        crash_num=crash_num+1
        read(fileid_crash,*,iostat=status_1) dummy
        if (status_1.lt.0) exit
    enddo
    crash_num=crash_num-1
    write(*,*) crash_num
    close (fileid_crash)
    open (fileid_sta,file=filename_sta)
            read (fileid_sta,*) sta_num
            allocate (station_mp(sta_num))
            allocate (station_id(sta_num))
            do j = 1 ,sta_num
                read (fileid_sta,*) station_mp(j),station_id(j)
                write(*,*) j,station_mp(j),station_id(j)
            enddo
    close (fileid_sta)
    write(*,*) "STATION INFORMATION IS DONE"
    open (fileid_crash,file=filename_crash)
    call All_allocate
    do i = 1, crash_num
        read(fileid_crash,"(A100)") dummy
        
        dummy_len=len(trim(adjustl(dummy)))
        flag=scan(trim(dummy),' ')
        crash_name(i)=dummy(1:flag-1)
        dummy=dummy(flag+1:dummy_len)
        flag=scan(trim(dummy),' ')
        start_date(i)=dummy(1:flag-1)
        dummy_len=len(trim(adjustl(dummy)))
        dummy=dummy(flag+1:dummy_len)
        flag=scan(trim(dummy),' ')
        start_time(i)=dummy(1:flag-1)
        dummy_len=len(trim(adjustl(dummy)))
        dummy=dummy(flag+1:dummy_len)
        flag=scan(trim(dummy),' ')
        end_date(i)=dummy(1:flag-1)
        dummy_len=len(trim(adjustl(dummy)))
        dummy=dummy(flag+1:dummy_len)
        flag=scan(trim(dummy),' ')
        end_time(i)=dummy(1:flag-1)
        dummy_len=len(trim(adjustl(dummy)))
        dummy=dummy(flag+1:dummy_len)
        flag=scan(trim(dummy),' ')
        crash_severity(i)=dummy(1:flag-1)
        dummy_len=len(trim(adjustl(dummy)))
        dummy=dummy(flag+1:dummy_len)
        flag=scan(trim(dummy),' ')
        if (flag==2)then
        read(dummy(1:1),"(I1)") crash_priority(i)
        else
        write(dummy(1:2),"(I2)") crash_priority(i)
        end if
        dummy_len=len(trim(adjustl(dummy)))
        dummy=dummy(flag+1:dummy_len)
        flag=scan(trim(dummy),' ')
        if (flag==2) then
            read(dummy(1:1),"(I1)") crash_impact(i)
        else
            read(dummy(1:2),"(I2)") crash_impact(i)
        endif
        dummy=dummy(flag+1:dummy_len)
        read(dummy(1:3),"(I3)") crash_mp(i)
        
        dummy_len=len(trim(adjustl(dummy)))
        dummy=dummy(flag+1:dummy_len)
        flag=scan(trim(dummy),' ')
        read(dummy(flag+1:flag+1),"(I1)") overlap(i)

        !crash_name
        read(crash_name(i)(1:4),"(I4)") crash_year(i)
        flag=scan(start_date(i),'/')
        
            if (flag==2) then
                read(start_date(i)(1:1),"(I1)") crash_month1(i)
            else
        
                read(start_date(i)(1:2),"(I2)") crash_month1(i)
                
            endif
        
        dummy=start_date(i)(flag+1:len(trim(adjustl(start_date(i)))))
        flag=scan(dummy,'/')
            if (flag==2) then
                read(dummy(1:1),"(I1)") crash_day1(i)
            else
                read(dummy(1:2),"(I2)") crash_day1(i)
            endif
        
        flag=scan(start_time(i),':')
            if (flag==3) then
                read(start_time(i)(1:2),"(I2)") crash_hour1(i)
                read(start_time(i)(4:5),"(I2)") crash_minute1(i)
            else
                read(start_time(i)(1:1),"(I1)") crash_hour1(i)
                read(start_time(i)(3:4),"(I2)") crash_minute1(i)
            endif
            
        flag=scan(end_date(i),'/')
            if (flag==2) then
                read(end_date(i)(1:1),"(I1)") crash_month2(i)
            else
                read(end_date(i)(1:2),"(I2)") crash_month2(i)
            endif
        
        dummy=end_date(i)(flag+1:len(trim(adjustl(end_date(i)))))
        flag=scan(dummy,'/')
            if (flag==2) then
                read(dummy(1:1),"(I1)") crash_day2(i)
            else
                read(dummy(1:2),"(I2)") crash_day2(i)
            endif
        
        flag=scan(end_time(i),':')
            if (flag==3) then
                read(end_time(i)(1:2),"(I2)") crash_hour2(i)
                read(end_time(i)(4:5),"(I2)") crash_minute2(i)
            else
                read(end_time(i)(1:1),"(I1)") crash_hour2(i)
                read(end_time(i)(3:4),"(I2)") crash_minute2(i)
            endif
            
        
    enddo
    WRITE(*,*) "CRASH INFORMATION INPUT IS DONE: Input ",crash_num," incidents"
    
    !***************************************************READ CRASH DATA DONE**************************************
    !***************************************************INCIDENT FREE PERIOD**************************************
    do i = 1 ,crash_num
        crash_doy2(i)=cal_day_of_year(crash_month2(i),crash_day2(i))
        crash_doy1(i)=cal_day_of_year(crash_month1(i),crash_day1(i))
        crash_dow1(i)=cal_day_of_week(crash_year(i),crash_month1(i),crash_day1(i))
        crash_dow2(i)=cal_day_of_week(crash_year(i),crash_month2(i),crash_day2(i))
    enddo
    
    !**************************************************ADD NEW VARIABLES DOW,DOY*********************************
    open (fileid_out_day,file=filename_out_day)
    open (fileid_out_fea,file=filename_out_fea)
    do i = 1, crash_num
        write(*,*) overlap(i)
        if (overlap(i).eq.0) then
        write(*,*) "working on incident",i
     !do i =1, 10
        doy_avail=0
            do j = 1, 365
                dow=mod(j+1,7)+1
                if (dow.eq.crash_dow2(i)) then
                    doy_avail(j)=1
                    do k = 1, crash_num
                        ! IF MORE THAN ONE INCIDENT HAPPENS AT SAME DOW AND SAME TIME PERIOD, THIS DAY IS NOT AVAILABLE, VALUED AS K+100
                        ! THE VALUE OF DOY_AVAIL SHOWS THE REASON WHY THE DAY IS RULED OUT
                        ! DOY_AVAIL 0: NOT THE DOW, 1: RIGHT DAY, >100: MORE THAN ONE INCIDENT HAPPENED
                        if (crash_doy1(k)==j) then
                            target_minute1=crash_hour1(i)*60+crash_minute1(i)
                            target_minute2=crash_hour2(i)*60+crash_minute2(i)
                            select_minute1=crash_hour1(k)*60+crash_minute1(k)
                            select_minute2=(crash_hour2(k)+24*(crash_doy2(k)-crash_doy1(k)))*60+crash_minute2(k)
                            doy_avail(j)=k+100
                            
                            if ((select_minute2.lt.target_minute1).or.(select_minute1.gt.target_minute2)) then
                                doy_avail(j)=1
                            endif
                        endif
                    enddo    
                endif
            enddo
        doy_avail(crash_doy1(i))=i+100
        
        ! OUTPUT INCIDENT INFORMATION
        write(fileid_out_day,*)" incident",i,crash_month1(i),crash_day1(i),crash_doy1(i),crash_dow1(i)
        do j = 1, 365
            
            if (doy_avail(j)==1) then
                write(fileid_out_day,*) "INCIDENT #",I,"AVAILABLE DAY",J
            ELSE 
                WRITE(fileid_out_day,*) "NOT AVAILABLE",J,DOY_AVAIL(j),mod(j+1,7)+1
            endif
        end do
        write(*,*) I,"INCIDENT FREE PERIOD SELECTED"
                ! INCIDENT AND STATINO MATCHING
                station_flag=0
                do k = 1, sta_num
                    if (abs(station_mp(k)-crash_mp(i)).lt.1e-6) then
                        station_flag=1
                        up_station=k
                        down_station=k
                    endif
                enddo   
                
                if (station_flag==0) then
                    do k = 1, sta_num-1
                        if ((crash_mp(i).gt.station_mp(k)).and.(crash_mp(i).lt.station_mp(k+1))) then
                        station_flag=2
                        up_station=k
                        down_station=k+1
                        endif
                    enddo
                endif
                
                WRITE(*,*) I,"UP & DOWN STATIONS FOUND: UPSTREAM IS ",UP_STATION,"DOWNSTREAM IS ",&
		DOWN_STATION,"MP",crash_mp(i),"between",station_mp(up_station),station_mp(down_station)
                ! CALCULATE SPATIAL COEFFICIENT
                if (station_flag==1) then
                    station_coef=0
                else
                    station_coef=(crash_mp(i)-station_mp(up_station))/(station_mp(down_station)-station_mp(up_station))
                endif
                
                WRITE(*,*) I,"STATION COEFFICIENT CALCULATED: STATION COEFFICIENT IS ",STATION_COEF
                do k= 1, 365
                    if ((doy_avail(k)==1).AND.((k.ne.69).OR.(crash_hour2(i).lt.2).or.(crash_hour1(i).gt.3))) then
                            write(*,*) k,"IS AVAILABLE FOR INCIDENT ",I
                            ! CALCULATE TIME_COEF
                            time_flag=0
                            if (mod(crash_minute2(i),5).eq.0) then
                                time_flag=1
                            else 
                                time_flag=2
                            endif
                            line_num=(k-1)*288+12*crash_hour2(i)+crash_minute2(i)/5+1
                            ref_line_num=(cal_day_of_year(3,10)-1)*288+12*2
                            if (line_num.gt.ref_line_num) then
                                line_num=line_num-12
                            endif
                            if (time_flag==1) then
                                up_time=line_num
                                down_time=line_num
                                time_coef=0
                            else
                                up_time=line_num
                                down_time=line_num+1
                                time_coef=(crash_minute2(i)-(crash_minute2(i)/5)*5)/5.0
                            endif
                
                            write(up_station_str,"(I6)") station_id(up_station)
                            write(down_station_str,"(I6)") station_id(down_station)
                            open (15,file="./output/N_"//trim(adjustl(up_station_str))//"_DEL60.txt")
                            
                            do j =1, line_num
                                read(15,*) temp1,temp2,temp3,flow_ub
                            end do
                                read(15,*) temp1,temp2,temp3,flow_ua
                            close (15)
                            !if (station_flag.eq.2) then
                            open (17,file="./output/N_"//trim(adjustl(down_station_str))//"_DEL60.txt")
                            do j= 1, line_num
                                read(17,*) temp1,temp2,temp3,flow_db
                            end do
                                read (17,*) temp1, temp2, temp3, flow_da
                            close(17)
                            !else
                             !   flow_db=flow_ub
                             !   flow_da=flow_ua
                            !endif
                            write(fileid_out_fea,"(7(I6),2(F10.4))",advance='no') i,crash_doy2(i),k,crash_hour2(i),&
				crash_minute2(i),up_station, down_station, station_coef,&
				(flow_ub*(1-time_coef)+time_coef*flow_ua)*(1-station_coef)+&
					station_coef*(flow_db*(1-time_coef)+time_coef*flow_da)
                            write(fileid_out_fea,*)
                    endif
                enddo
     
    endif        
    enddo
    close (fileid_out_day)
    close (fileid_out_fea)
    deallocate(crash_impact, crash_priority,crash_mp,crash_name,start_date,end_date,crash_severity,start_time,end_time,overlap)
    deallocate(crash_year,crash_month1,crash_month2,crash_day1,crash_day2,crash_hour1,crash_hour2,&
		crash_minute1,crash_minute2,crash_dow1,crash_dow2,crash_doy1,crash_doy2)
    end program Total_delay

!**********************************************SUBROUTINE ALLOCATE*****************************
subroutine All_allocate
use file_variables
use variables
implicit none
    allocate(crash_name(crash_num))
    allocate(start_time(crash_num))
    allocate(start_date(crash_num))
    allocate(end_time(crash_num))
    allocate(end_date(crash_num))
    allocate (crash_severity(crash_num))
    allocate (crash_priority(crash_num))
    allocate (crash_impact(crash_num))
    allocate (crash_mp(crash_num))
    allocate (crash_year(crash_num))
    allocate (crash_month1(crash_num))
    allocate (crash_month2(crash_num))
    allocate (crash_day1(crash_num))
    allocate (crash_day2(crash_num))
    allocate (crash_hour1(crash_num))
    allocate (crash_hour2(crash_num))
    allocate (crash_minute1(crash_num))
    allocate (crash_minute2(crash_num))
    allocate (crash_dow1(crash_num))
    allocate (crash_doy1(crash_num))
    allocate (crash_dow2(crash_num))
    allocate (crash_doy2(crash_num))
    allocate (overlap(crash_num))
    return
    end subroutine
!***********************************************CALCULATE DAY OF YEAR**********************************************************
function cal_day_of_year(b,c)
implicit none
integer cal_day_of_year
integer a,b,c
integer i,total
integer ::day_month(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
    total=0
    if (b.gt.1) then
        do i=1, b-1
           total=total+day_month(i)
        enddo
    endif
    total=total+c
    cal_day_of_year=total
    end function
!***********************************************CALCULATE DAY OF WEEK**********************************************************
function cal_day_of_week(a,b,c)
implicit none
integer cal_day_of_week
integer func_dow
integer i,a,b,c,total_day,day_of_w
integer ::day_month(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
    total_day=0
    if (b.gt.1) then
        do i =1,b-1
            total_day=total_day+day_month(i)
        enddo
    endif
    total_day=total_day+c
    day_of_w=mod(total_day+1,7)+1
    cal_day_of_week=day_of_w
end function
    
        
