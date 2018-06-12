        program main
        use netcdf
        implicit none
        character(len=100) :: rawpath,filename,pathname
        character(len=10) :: initial_date,cur_date,pre_date
        integer(KIND=1) :: argnum
        character(len = *),parameter :: fileout = "ECOUT.nc"
        character(len=12),parameter :: longitude_units = "degrees_east"
        character(len=5),parameter :: longitude_unitsname = "units"
        logical alive
        integer :: ncid,status,xtype,ndims,nvars,DimID,VarID,NX,NY,NT
        integer :: VarNum,nGlobalAtts,unlimitedDimId,i0,j0,k0,k1
        integer :: tp_FillValue,tp_missing_value
        character(LEN=50) :: xname,yname,tname,vname,filename_4,Fname_3
        real,allocatable :: tpdata(:,:,:),tp(:,:,:)
        real,allocatable :: tpdata6h(:,:,:)
        real,allocatable :: m_tpdata6h(:,:,:)
        real,allocatable :: xpos(:),xpos_out(:)
        real,allocatable :: ypos(:),ypos_out(:)
        integer,allocatable :: tpos(:),DimNs(:),VarNs(:),dimids(:)
        integer,allocatable :: VarTypes(:),VarDims(:),AttTypes(:)
        integer,allocatable :: AttLens(:),AttNums(:),VarIds(:)
        character(LEN=50),allocatable :: DimNames(:),VarNames(:)
        real :: tp_scale_factor,tp_add_offset
!C  /* dimension ids */
       integer :: ncid_out
       integer :: longitude_dim,latitude_dim,time_dim
!C  /* variable ids */
       integer :: longitude_id,latitude_id,time_id,tp_id
!C  /* variable shapes */
       integer :: longitude_dims(1),latitude_dims(1)
       integer :: time_dims(1)
       integer :: tp_dims(3)
       integer,allocatable :: tpos_out(:)
!C   /* attribute vectors */
       real :: tp_scale_factor_out(1),tp_add_offset_out(1)
       integer :: tp_FillValue_out(1),tp_missing_value_out(1)
       character(len=100)    :: fngrd,fnctl,fngs,fngmf,mytime
       real,parameter :: finterval=2.0
       character(len=9)   :: a9_time,a9_time_pkt
       character(len=10)   :: a10_time,a9_to_yr_a10_time
       integer :: diamond_type
       character(len=100) :: comment
       integer :: year,month,day,hour,a,num_days0
       integer :: hour_in,year_out,month_out,day_out,hour_out
       logical,external :: isleap
       integer,external :: GetYearByDay,GetMonthByDay
       logical :: leap
       integer :: year_test,day_test,year_day,month_day
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 获取netcdf文件路径名字，判断是否存在，获取初始场时刻，并打开
!C ../rawdata/ECTHIN_2015091412_024_06_tp.nc
!C  get filename from args
!C        argnum = iargc()
!C          write(6,*)' argnum = ',argnum
!C       if(argnum .eq. 1)then
        call getarg(1,rawpath)
         write(6,*)' rawpath = ',rawpath
        call getarg(2,filename)
        if(len_trim(filename) .eq. 28)then
         write(6,*)' filename = ',filename
         write(*,*)"process CDF file!"
        else
         write(6,*)' filename = ',filename," not correct"
        go to 999
        endif
        initial_date=filename(8:18)
        write(6,*)' initial_date = ',initial_date
        read(initial_date(1:4),*)year
        read(initial_date(5:6),*)month
        read(initial_date(7:8),*)day
        read(initial_date(9:10),*)hour
        write(*,*)year,month,day,hour
!C        else
!C       write(6,*)' plese input correct filename!!'
!C      endif
!C    filename=trim(laps_data_root)//"/lapsprd/"//trim(fnvar2d(in))
!C    1   //"/"//a9_time//"."//trim(fnvar2d(in))
!C  open file for input and output
        pathname=trim(rawpath)//"/"//filename
        inquire(file=trim(pathname),exist=alive)
        if(alive)then
         write(*,*)'pathname=',trim(pathname),' exist'
         write(*,*), nf90_inq_libvers()
         status = nf90_open(trim(pathname),nf90_nowrite,ncid)
         call check(status)
!C 获取netcdf文件变量维数
         call check(nf90_inquire(ncid,ndims,nvars,nGlobalAtts,
     1   unlimitedDimId))
         write(*,*)'ndims=',ndims,'nvars=',nvars,'nGlob=',nGlobalAtts
         allocate(DimNames(ndims))
         allocate(DimNs(ndims))
         allocate(DimIds(ndims))
         allocate(VarNames(nvars))
         allocate(VarTypes(nvars))
         allocate(VarDims(nvars))
         allocate(VarIds(nvars))
         allocate(AttTypes(nvars))
         allocate(AttLens(nvars))
         allocate(AttNums(nvars))
!C  get the dimensions names and number
         do DimID = 1,ndims
         status=nf90_inquire_dimension(ncid,DimID,DimNames(DimID),
     1    DimNs(DimID))
         call check(status)
         write(*,*)DimID,'=',DimNames(DimID),DimNs(DimID)
         enddo
         write(*,*)DimNs
         NX = DimNs(1)
         NY = DimNs(2)
         NT = DimNs(3)
         k1 = DimNs(3)-1
         write(*,*)'k1=',k1,'DimNs(3)=',DimNs(3)
         allocate(xpos(DimNs(1)))
         allocate(ypos(DimNs(2)))
         allocate(tpos(DimNs(3)))
         allocate(tpos_out(k1))
         allocate(tpdata(DimNs(1),DimNs(2),DimNs(3)))
         allocate(tp(DimNs(1),DimNs(2),DimNs(3)))
         allocate(tpdata6h(DimNs(1),DimNs(2),k1))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
!C get the variables names Types number and IDs
         do VarNum = 1,nvars
         call check(nf90_inquire_variable(ncid,VarNum,VarNames(VarNum),
     1   VarTypes(VarNum),VarDims(VarNum),dimids))
         write(*,*)'VarNum=',VarNum
         write(*,*)'VName=',VarNames(VarNum),'Types=',VarTypes(VarNum)
         write(*,*)'VarDims=',VarDims(VarNum)
         write(*,*)dimids
         call check(nf90_inq_varid(ncid,VarNames(VarNum),
     1    VarIds(VarNum)))
         write(*,*)'VarIds=',VarIds(VarNum)
          call check(nf90_inquire_attribute(ncid,VarIds(VarNum),
     1     "units",AttTypes(VarNum),AttLens(VarNum),
     1     AttNums(VarNum)))
          write(*,*)'AttType=',AttTypes(VarNum)
          write(*,*)'Attlen=',AttLens(VarNum)
          write(*,*)'AttNum=',AttNums(VarNum)
         enddo
         call check(nf90_get_var(ncid,VarIds(1),xpos))
!C         write(*,*)xpos
         CALL check(nf90_get_var(ncid,VarIds(2),ypos))
!C         write(*,*)ypos
         CALL check(nf90_get_var(ncid,VarIds(3),tpos))
         write(*,*)tpos
         CALL check(nf90_get_att(ncid,VarIds(4),"scale_factor",
     1    tp_scale_factor))
         write(*,*)'tp_scale_factor =',tp_scale_factor
         CALL check(nf90_get_att(ncid,VarIds(4),"add_offset",
     1    tp_add_offset))
         write(*,*)'tp_add_offset =',tp_add_offset
         CALL check(nf90_get_att(ncid,VarIds(4),"_FillValue",
     1    tp_FillValue))
         write(*,*)'tp_FillValue =',tp_FillValue
         CALL check(nf90_get_att(ncid,VarIds(4),"missing_value",
     1    tp_missing_value))
         write(*,*)'tp_missing_value =',tp_missing_value
         CALL check(nf90_get_var(ncid,VarIds(4),tpdata))
         call check(nf90_close(ncid))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C get the grid preciption data
         do k0=1,DimNs(3)
             tpos_out(k0)=tpos(k0+1)
          do j0=1,DimNs(2)
             do i0=1,DimNs(1)
                 tp(i0,j0,k0)=(tpdata(i0,j0,k0)*tp_scale_factor
     1           +tp_add_offset)*1000
             enddo
           enddo
          enddo
         write(*,*)'tpos_out =',tpos_out
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C get the 间隔 grid preciption data
         do k0=1,DimNs(3)-1
          do j0=1,DimNs(2)
             do i0=1,DimNs(1)
             tpdata6h(i0,j0,k0)=tp(i0,j0,k0+1)-tp(i0,j0,k0)
             if (tpdata6h(i0,j0,k0) < 0) then
                write(*,*)i0,j0,k0,tpdata6h(i0,j0,k0)
             endif
             enddo
           enddo
          enddo
         write(*,*)'tpos_out =',tpos_out
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 给定范围 (120-100)/0.125=160,(40-25)/0.125=120,100/0.125=800,(90-40)/0.125=400
         allocate(m_tpdata6h(161,121,k1))
         allocate(xpos_out(161))
         do i0=1,161
         xpos_out(i0)=xpos(800+i0)
         enddo
         allocate(ypos_out(121))
         do j0=1,121
         ypos_out(j0)=ypos(400+j0)
         enddo
         do k0=1,k1
          do j0=1,121
             do i0=1,161
             m_tpdata6h(i0,j0,k0)=tpdata6h(800+i0,400+j0,k0)
             if (m_tpdata6h(i0,j0,k0) < 0) then
                write(*,*)i0,j0,k0,m_tpdata6h(i0,j0,k0)
             endif
             enddo
           enddo
          enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成micaps 3类数据
            diamond_type = 3
140         format(i5.5,f8.3,f7.3,i2,f5.1) 
        do k0=1,k1
        call GetDateByHour(tpos_out(k0),year_out,month_out,
     1 day_out,hour_out)
         write(*,*)tpos_out(k0),year_out,month_out,day_out,hour_out
         write(cur_date,300)year_out,month_out,day_out,hour_out
         call adddatetime(year_out,month_out,day_out,hour_out,-6,"hr")
         write(*,*)"pre_date(-6)=",year_out,month_out,day_out,hour_out
         write(pre_date,300)year_out,month_out,day_out,hour_out
         write(comment,400)'Diamond ',diamond_type,initial_date,
     1     "对",pre_date,"到",cur_date,"ECMFthin_6小时降水"
         write(*,*)comment
         Fname_3=trim(rawpath)//"/"//cur_date(3:10)//".000"
         open(10,file=trim(Fname_3),iostat=status)
         write(10,*)comment(:len_trim(comment))
         write(10,150)year_out,month_out,day_out,hour_out,-1
150         format(i4,3i3.2,i4)
        write(10,'(a)')'2 0.1 2.5 0 2.5'
        write(10,'(a)')'0 '
        write(10,'(2i8)')1,121*161
           do j0=1,121
              do i0=1,161
          write(10,140)i0*j0,xpos_out(i0),ypos_out(j0),1,
     1    m_tpdata6h(i0,j0,k0)
              enddo
           enddo
C           do j0=401,521
C              do i0=801,961
C          write(10,140)(i0-800)*(j0-400),xpos(i0),ypos(j0),1,
C      1    tpdata6h(i0,j0,k0)
C              enddo
C           enddo
         print *, "*** SUCCESS writing micaps 3 data!*"//Fname_3
          close(10)
         enddo
        end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成micaps 4类数据 
         diamond_type = 4
        do k0=1,k1
          write(*,*)'k0=',k0
        call GetDateByHour(tpos_out(k0),year_out,month_out,
     1 day_out,hour_out)
         write(*,*)tpos_out(k0),year_out,month_out,day_out,hour_out
         write(cur_date,300)year_out,month_out,day_out,hour_out
         write(*,*)'cur_date=',cur_date
         call adddatetime(year_out,month_out,day_out,hour_out,-6,"hr")
         write(*,*)"pre_date(-6)=",year_out,month_out,day_out,hour_out
         write(pre_date,300)year_out,month_out,day_out,hour_out
300      format(i4,3i2.2)
         write(comment,400)'Diamond ',diamond_type,initial_date,
     1     "对",pre_date,"到",cur_date,"ECMFthin_6小时降水"
400      format(a8,i1,1x,a10,a,a10,a,a10,a)
         write(*,*)comment
         filename_4=trim(rawpath)//"/"//cur_date(1:10)//".000"
         write(6,*)"write to file : ",filename_4
c write data to file
            open(30,file=filename_4,iostat=status)
            write(30,*)comment(:len_trim(comment))
            write(30,500)year,month,day,hour,0,9999
500         format(i4.2,3i3.2,i4,i6)
550         format(10f8.1)
            write(30,'(a)'),"0.125 -0.125 100 120 40 25 161 121 2.5 -100
     1  100 0 0"
            write(30,550)((m_tpdata6h(i0,j0,k0),i0=1,161),j0=1,121)
            close(30)
        enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 创建netCDF文件，返回文件对应的ID，如果存在则覆盖
       status = nf90_create(trim(fileout),NF90_CLOBBER,ncid_out)
       call check(status)
       write(*,*)'ncid_out=',ncid_out
!C       call check(nf90_redef(ncid_out))
!C  定义维数，返回一个对应的ID,取100-120E,25-40N,110,40--120,25
       call check(nf90_def_dim(ncid_out,DimNames(1),961-801+1,
     1     longitude_dim))
       call check(nf90_def_dim(ncid_out,DimNames(2),521-401+1,
     1  latitude_dim))
       call check(nf90_def_dim(ncid_out,DimNames(3),k1,
     1  time_dim))
       write(*,*)'longitude_dim=',longitude_dim
       write(*,*)'latitude_dim=',latitude_dim
       write(*,*)'time_dim=',time_dim
!C 把上面得到的ID写到一个存放ID的数组里，注意，在fortran中，数组是以列为主存放数据的
       longitude_dims(1)=longitude_dim
       latitude_dims(1)=latitude_dim
       time_dims(1)=time_dim
       write(*,*)'longitude_dims=',longitude_dims
       write(*,*)'latitude_dims=',latitude_dims
       write(*,*)'time_dims=',time_dims
       tp_dims(1)=longitude_dim
       tp_dims(2)=latitude_dim
       tp_dims(3)=time_dim
       write(*,*)'tp_dims=',tp_dims
!C 定义变量，返回一个对应的ID
       call check(nf90_def_var(ncid_out,VarNames(1),NF90_FLOAT,
     1    longitude_dims,longitude_id))
       write(*,*)'longitude_id=',longitude_id
       call check(nf90_def_var(ncid_out,VarNames(2),NF90_FLOAT,
     1    latitude_dims,latitude_id))
       write(*,*)'latitude_id=',latitude_id
       call check(nf90_def_var(ncid_out,VarNames(3),NF90_INT,time_dims,
     1   time_id))
       write(*,*)'time_id=',time_id
       call check(nf90_def_var(ncid_out,VarNames(4),NF90_FLOAT,tp_dims,
     1  tp_id))
       write(*,*)'tp_id=',tp_id
!C    /* assign attributes */ 
        CALL check(nf90_put_att(ncid_out,longitude_id,
     1   longitude_unitsname,longitude_units))
        CALL check(nf90_put_att(ncid_out,longitude_id,"long_name",
     1   "longitude"))
        CALL check(nf90_put_att(ncid_out,latitude_id,"units",
     1   "degrees_north"))
        CALL check(nf90_put_att(ncid_out,latitude_id,"long_name",
     1   "latitude"))
        CALL check(nf90_put_att(ncid_out,time_id,"units",
     1   "hours since 1900-01-01 00:00:0.0")) 
        CALL check(nf90_put_att(ncid_out,time_id,"long_name","time"))
        CALL check(nf90_put_att(ncid_out,time_id,"calendar",
     1  "gregorian"))
C           tp_scale_factor_out(1)=6.28811220944982e-06
C         CALL check(nf90_put_att(ncid_out,tp_id,"scale_factor",
C      1   tp_scale_factor_out))
C           tp_add_offset_out(1)=0.206036284654833
C         CALL check(nf90_put_att(ncid_out,tp_id,"add_offset",
C      1   tp_add_offset_out))
C          tp_FillValue_out(1)=-32767
C          CALL check(nf90_put_att(ncid_out,tp_id,"FillValue",
C      1   tp_FillValue_out))
C           tp_missing_value_out(1)=-32767
C         CALL check(nf90_put_att(ncid_out,tp_id,"missing_value",
C      1   tp_missing_value_out))
        CALL check(nf90_put_att(ncid_out,tp_id,"units","mm"))
        CALL check(nf90_put_att(ncid_out,tp_id,"long_name",
     1    "6 hours precipitation"))
       CALL check(nf90_put_att(ncid_out,NF90_GLOBAL,"Conventions",
     1   "zhang weimin"))
       CALL check(nf90_put_att(ncid_out,NF90_GLOBAL,"history",
     1   "by zhang weimin"))
!C 定义完成，关闭定义模式
       call check(nf90_enddef(ncid_out))
! 写入数据
       call check(nf90_put_var(ncid_out,longitude_id,xpos_out))
       call check(nf90_put_var(ncid_out,latitude_id,ypos_out))
       call check(nf90_put_var(ncid_out,time_id,tpos_out))
       write(*,*)tpos_out,k0
!C       write(*,*)'ffffffffffffffcccffff'
       write(*,550)m_tpdata6h
       call check(nf90_put_var(ncid_out,tp_id,m_tpdata6h))
! 关闭文件
      call check(nf90_close(ncid_out))
!提示写文件成功ll
      print *, "*** SUCCESS writing file! ****"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
!C 写grads二进制文件
!C        fngrd=trim(datadir)//"laps"//trim(lapstime)//".grd"
        fngrd="ec.grd"
        open(15,file=trim(fngrd),form='binary')
        write(15)m_tpdata6h
        close(15)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
        deallocate(tpos)
        deallocate(tpos_out)
        deallocate(xpos)
        deallocate(ypos)
        deallocate(tp)
        deallocate(dimids)
        deallocate(tpdata)
        deallocate(tpdata6h)
        deallocate(m_tpdata6h)
999     call exit()
      end 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
         subroutine check(status)
        use netcdf
        integer, intent (in) :: status
         if(status /= nf90_noerr) then
        print *, trim(nf90_strerror(status))
        stop 2
        end if
        end subroutine check
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!距离1900年1月1日的天数(0起始)，返回年，并且置天数为其在本年的索引(0起始)
         integer FUNCTION GetYearByDay(day,year_D) 
         IMPLICIT NONE
         INTEGER :: year,year_D,day
         year=day/365
         year_D=MOD(day,365)
         GetYearByDay=year
         year_D=year_D - int(GetYearByDay/4)
         year_D=year_D + int(GetYearByDay/100)
         year_D=year_D - int((GetYearByDay+300)/400)
         if (year_D .lt. 0) then
         GetYearByDay=GetYearByDay-1
         endif
         END FUNCTION GetYearByDay
!C根据一年中的第几天(0起始)，返回月份(1起始)，并且置天数为其在本月的索引(0起始)    
         integer FUNCTION GetMonthByDay(day,leap,month_day) 
          IMPLICIT NONE
          INTEGER :: day,month,add_day,n,i0,month_day
          integer :: days_in_month(12)
          logical :: leap
          DATA days_in_month /31,28,31,30,31,30,31, 31, 30, 31, 30, 31/
          if (leap .eq. .true.) then
            if (day .lt. 366) then
              days_in_month(2) = 29
              n = day
              do i0 =1,12
                if (n .ne. 0) then
                  n = day - add_day
                  add_day = add_day + days_in_month(i0)
                  month = month +1
                end if
              enddo
            endif
          else if (day .lt. 365) then
              days_in_month(2) = 28
              n = day
              do i0 =1,12
                if (n .ne. 0) then
                  n = day - add_day
                  add_day = add_day + days_in_month(i0)
                  month = month +1
                end if
              enddo
          endif
          month_day = n
          GetMonthByDay = month
          END FUNCTION GetMonthByDay

!C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         SUBROUTINE GetFebDay(year,num_days0) 
!C Compute the number of days in February for the given year
         IMPLICIT NONE
         INTEGER :: year
         INTEGER :: num_days0
         num_days0=28 ! By default, February has 28 days ...
         IF (MOD(year,4) .eq. 0) THEN  
            num_days0=29  ! But every four years, it has 29 days ...
            IF (MOD(year,100).eq.0) THEN
               num_days0=28  ! Except every 100 years, when it has 28 days ...
               IF (MOD(year,400).eq.0) THEN
                 num_days0 = 29  ! Except every 400 years, when it has 29 days.
               END IF
            END IF
         END IF
         END SUBROUTINE GetFebDay
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         SUBROUTINE split_date_char0 (date0,century_year,month,day 
     1         , hour , minute)
         IMPLICIT NONE
!  Input data.
         CHARACTER(LEN=12) :: date0
!  Output data.
         INTEGER :: century_year , month , day,hour, minute
         write(*,*)date0
         READ(date0(1:4),FMT='(I4.4)') century_year
         write(*,*)century_year
         READ(date0(5:6),FMT='(I2.2)') month
         write(*,*)month
         READ(date0(7:8),FMT='(I2.2)') day
         write(*,*)day
         READ(date0(9:10),FMT='(I2.2)') hour
         write(*,*)hour
         READ(date0(11:12),FMT='(I2.2)') minute
         write(*,*)minute  
         END SUBROUTINE split_date_char0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine day_of_year0(century_year,month,day,day2)
!get day2 of year by gived year,month,day
         IMPLICIT NONE
         INTEGER               :: century_year, month, day,day2
         INTEGER               :: m
         INTEGER :: days_in_month(12)
         DATA days_in_month /31,28,31,30,31,30,31, 31, 30, 31, 30, 31/
         call GetFebDay(century_year,days_in_month(2))           
         IF ( month .eq. 1) THEN
         day2 = day
         ELSE  
         day2 = 0
         DO m = 1, month-1
         day2 = day2 + days_in_month(m)
         ENDDO
         day2 = day2 + day
            ENDIF
         return
        END subroutine day_of_year0
         subroutine adddatetime(year,month,day,hour,dtime,dunit)
       implicit none
       integer,intent(inout)       :: year,month,day,hour
       integer,intent(in)          :: dtime
       character(len=2),intent(in) :: dunit
       integer                     :: days_of_month(12)
       integer                     :: i,n
       integer                     :: lyear,lmonth,lday,lhour,ldtime
       character(len=2)            :: ldunit   
       logical                     :: isleap      
       lyear   = year
       lmonth  = month
       lday    = day
       lhour   = hour
       ldtime  = dtime
       ldunit  = dunit  
       if (ldunit .eq. 'yr') then
          year = lyear + ldtime
       else if (ldunit .eq. 'mn') then
          ldtime = ldtime + lmonth - 1
          if (ldtime .lt. 0) then
            year   = lyear - 1 + ldtime / 12
            month  = 13 + mod(ldtime, 12)
          else
            year   = lyear + ldtime / 12
            month  = 1 + mod(ldtime, 12)
          endif
       else if (ldunit .eq. 'hr') then
          ldtime  = ldtime + lhour
          if (ldtime .lt. 0) then
            hour    = 24+mod(ldtime, 24)
            ldtime  = -1+ldtime / 24
          else
            hour    = mod(ldtime, 24)
            ldtime  = ldtime / 24
          endif
          ldunit = 'dy'
       endif
       if (ldunit .eq. 'dy') then   
       if (isleap(lyear)) then
        days_of_month=(/31,29,31,30,31,30,31,31,30,31,30,31/)
       else
        days_of_month=(/31,28,31,30,31,30,31,31,30,31,30,31/)
       endif
       n=abs(ldtime)
       if (ldtime .gt. 0) then
        do i=1,n
            lday=lday+1
            if (lday .gt. days_of_month(lmonth)) then
                lmonth=lmonth+1
            if (lmonth .gt. 12) then
              lmonth=1
              lyear=lyear+1
              if (isleap(lyear)) then
                days_of_month(2)=29
              else
                days_of_month(2)=28
              end if 
            endif
            lday=1
          endif
        enddo
       else if (ldtime .lt. 0) then
        do i=1,n
            lday=lday-1
            if (lday .lt. 1) then
                lmonth=lmonth-1
            if (lmonth .lt. 1) then
              lmonth=12
              lyear=lyear-1
              if (isleap(lyear)) then
                days_of_month(2)=29
              else
                days_of_month(2)=28
              endif 
            endif
            lday=days_of_month(lmonth)
          endif
        enddo
       endif
       year  = lyear
       month = lmonth
       day   = lday
       endif
       end subroutine adddatetime
!!距离1900年1月1日的小时数(0起始)，计算这一天的日期(年，月，日，时)(1起始)。
         SUBROUTINE GetDateByHour(hour_in,year_out,month_out,
     1        day_out,hour_out)
        IMPLICIT NONE
        INTEGER :: year,D,day,i0,n,day_add
        INTEGER :: hour_in,year_out,month_out,day_out,hour_out
        integer :: days_in_month(12)
        logical :: isleap
        DATA days_in_month /31,28,31,30,31,30,31, 31, 30, 31, 30, 31/
        year_out=0
        month_out=0
        day_out=0
        hour_out=0
        day = int(hour_in/24)
!C       write(*,*)'day=',day
        hour_out = MOD(hour_in,24)
!C        write(*,*)'hour_out=',hour_out
        year=int(day/365)
!C        write(*,*)'year=',year
        D=MOD(day,365)
!C        write(*,*)'D=',D
        year_out=year+1900
!C        write(*,*)'year_out=',year_out
        D=D-int(year/4)
!C       write(*,*)'D=',D
        D=D+int(year/100)
!C        write(*,*)'D=',D
        D=D-int((year+300)/400)
!C        write(*,*)'D=',D
        
        if (D < 0) then
          year_out=year-1
           if (isleap(year_out) .eq. .true.) then
           D=365+D
           else
           D=366+D
           endif
        endif
        if (isleap(year_out) .eq. .true.) then
          days_in_month(2) = 29
        endif
!C        write(*,*)'days_in_month(2)=',days_in_month(2)
        day_add = 0
        do while (D >= day_add)
           n = D - day_add
!C           write(*,*)'n=',n
           month_out = month_out + 1
!C           write(*,*)'month_out=',month_out
           day_add = sum(days_in_month(1:month_out))
!C           write(*,*)'day_add=',day_add
           
        end do
        day_out=n+1
        END SUBROUTINE GetDateByHour
         function isleap(year)
         implicit none
         integer,intent(in) :: year
         logical :: isleap
           if (mod(year, 4) .ne. 0) then
           isleap = .FALSE.
           else if (mod(year, 100) .ne. 0) then
              isleap = .TRUE.
           else if (mod(year, 400) .eq. 0) then
              isleap = .TRUE.
           else
              isleap = .FALSE.
         endif
         end function isleap