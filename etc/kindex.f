        program main
        use netcdf
        implicit none
        character(len=100) :: rawpath,filename,pathname,ncfileout
        character(len=100) :: purename
        character(len=100) :: reference_date,gsctl,uuuuu
        character(len=3) :: mhz,monthen
        character(len=2) :: mnow
        character(len=10) :: initial_date,cur_date,pre_date
        integer(KIND=1) :: argnum
        character(len=12),parameter :: longitude_units = "degrees_east"
        character(len=5),parameter :: longitude_unitsname = "units"
        integer(KIND=4),parameter :: corner_E=108
        integer(KIND=4),parameter :: corner_W=116
        integer(KIND=4),parameter :: corner_N=42
        integer(KIND=4),parameter :: corner_S=33
        real,parameter            :: gd_f=0.5
        real,parameter            :: gd_l=0.125
        integer(KIND=4)  :: gdnf_x,gdnf_y,gdnl_x,gdnl_y,gdni_x,gdni_y
        logical alive
        integer :: ncid,status,xtype,ndims,nvars,DimID,VarID,NX,NY,NT
        integer :: VarNum,nGlobalAtts,unlimitedDimId,i0,j0,k0,k1
        integer :: tp_FillValue,tp_missing_value,m,n
        character(LEN=50) :: xname,yname,tname,vname,argstr
        real,allocatable,dimension(:,:,:) :: kx,k_kx,kx_out,c_kx
        real,allocatable,dimension(:) :: xpos,xpos_out,k_xpos,czh_xpos
        real,allocatable,dimension(:) :: ypos,ypos_out,k_ypos,czh_ypos
        integer,allocatable :: tpos(:)
        real :: tw,te,tn,ts,t1,t2,ns,we,x1,x2,x3,x4,y1,y2
        integer,allocatable :: DimNs(:),VarNs(:),dimids(:)
        integer,allocatable :: VarTypes(:),VarDims(:),AttTypes(:)
        integer,allocatable :: AttLens(:),AttNums(:),VarIds(:)
        character(LEN=50),allocatable :: DimNames(:),VarNames(:)
        real :: tp_scale_factor,tp_add_offset
!C  /* dimension ids */
       integer :: ncid_out,reference_time
       integer :: longitude_dim,latitude_dim,time_dim
!C  /* variable ids */
       integer :: longitude_id,latitude_id,time_id,kx_id
!C  /* variable shapes */
       integer :: longitude_dims(1),latitude_dims(1)
       integer :: time_dims(1)
       integer :: kx_dims(3)
!C   /* attribute vectors */
       real :: tp_scale_factor_out(1),tp_add_offset_out(1)
       integer :: tp_FillValue_out(1),tp_missing_value_out(1)
       character(len=100) :: fngrd,fnctl,fngs,fngmf,mytime
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
        !C  get args
        argnum = command_argument_count() 
        write(6,*)' argnum = ',argnum
        if(argnum .eq. 2)then
            call getarg(1,rawpath)
            write(6,*)'argstr 1 = ',rawpath
            call getarg(2,purename)
            write(6,*)'argstr 2 = ',purename
            purename=trim(purename)
            initial_date=purename(3:12)
            if(len_trim(initial_date) .eq. 10)then
                write(*,*)"process CDF file!"
            else
                write(6,*)'initial_date = ',initial_date," not correct"
                go to 999
            endif
            write(6,*)'initial_date = ',initial_date
            read(initial_date(1:4),*)year
            read(initial_date(5:6),*)month
            read(initial_date(7:8),*)day
            read(initial_date(9:10),*)hour
            write(*,*)year,month,day,hour
        else
            write(6,*)' plese input correct path and file name !!'
        endif
        pathname=trim(rawpath)//"/"//purename
        inquire(file=trim(pathname),exist=alive)
        if (alive .eq. .false.)then
            write(*,*)'pathname=',pathname,' not exist'
            go to 999
        endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
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
            status=nf90_inquire_dimension(ncid,DimID,
     1      DimNames(DimID),DimNs(DimID))
            call check(status)
            write(*,*)DimID,'=',DimNames(DimID),DimNs(DimID)
        enddo
        write(*,*)DimNs
         NY=DimNs(1)
         NX=DimNs(2)
         NT=DimNs(3)
        write(*,*)'NY=',NY,'NX=',NX
        allocate(xpos(NX))
        allocate(ypos(NY))
        allocate(tpos(NT))
        allocate(kx(NX,NY,NT))
        !C get the variables names Types number and IDs
        do VarNum = 1,nvars
            call check(nf90_inquire_variable(ncid,VarNum,
     1      VarNames(VarNum),VarTypes(VarNum),VarDims(VarNum),dimids))
            write(*,*)'VarNum=',VarNum
            write(*,*)'VName=',VarNames(VarNum),'Types=',
     1      VarTypes(VarNum)
            write(*,*)'VarDims=',VarDims(VarNum)
            write(*,*)dimids
            call check(nf90_inq_varid(ncid,VarNames(VarNum),
     1      VarIds(VarNum)))
            write(*,*)'VarIds=',VarIds(VarNum)
            call check(nf90_inquire_attribute(ncid,VarIds(VarNum),
     1      "units",AttTypes(VarNum),AttLens(VarNum),AttNums(VarNum)))
            write(*,*)'AttType=',AttTypes(VarNum)
            write(*,*)'Attlen=',AttLens(VarNum)
            write(*,*)'AttNum=',AttNums(VarNum)
        enddo
        call check(nf90_get_var(ncid,VarIds(1),ypos))
!C        write(*,*)"ypos=",ypos
        CALL check(nf90_get_var(ncid,VarIds(2),xpos))
!C        write(*,*)"xpos=",xpos
        CALL check(nf90_get_var(ncid,VarIds(3),tpos))
        write(*,*)"tpos=",tpos
        CALL check(nf90_get_att(ncid,VarIds(3),"units",
     1    uuuuu))
        write(*,*)"units=",uuuuu
        CALL check(nf90_get_att(ncid,VarIds(3),"reference_time",
     1    reference_time))
        write(*,*)"reference_time=",reference_time
        do k0=1,NT
            write(*,*)"time dert =",(tpos(k0)-reference_time)/3600
        enddo
        CALL check(nf90_get_att(ncid,VarIds(3),"reference_date",
     1    reference_date))
        write(*,*)"reference_date=",reference_date
!C         CALL check(nf90_get_att(ncid,VarIds(4),"_FillValue",
!C      1    tp_FillValue))
        write(*,*)'tp_FillValue =',tp_FillValue
        CALL check(nf90_get_var(ncid,VarIds(4),kx))
        call check(nf90_close(ncid))
!C        write(*,*)"kx=",kx
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
!C 给定范围 (120-100)/0.5=40,(40-25)/0.5=30,(100-40)/0.5=120,(25-(-10))/0.5=70
        gdnf_x=(corner_W-corner_E)/gd_f
        write(*,*)'gdnf_x=',gdnf_x
        gdnf_y=(corner_N-corner_S)/gd_f
        write(*,*)'gdnf_y=',gdnf_y
        gdni_x=(corner_E-40)/gd_f
        write(*,*)'gdni_x=',gdni_x
        gdni_y=(corner_S-(-10))/gd_f
        write(*,*)'gdni_y=',gdni_y
        allocate(k_xpos(gdnf_x+1))
        allocate(k_ypos(gdnf_y+1))
        allocate(k_kx(gdnf_x+1,gdnf_y+1,NT))
        k_xpos(:)=xpos(gdni_x+1:gdni_x+gdnf_x+1)
        write(*,*)"k_xpos=",k_xpos
        k_ypos(:)=ypos(gdni_y+1:gdni_y+1)
        write(*,*)"k_ypos=",k_ypos
        k_kx(:,:,:)=kx(gdni_x+1:gdni_x+gdnf_x+1,gdni_y+1:
     1  gdni_y+gdnf_y+1,:)
!C        write(*,*)"k_kx=",k_kx
        deallocate(xpos)
        deallocate(ypos)
        deallocate(kx)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
!C 给定范围 y方向从25--40反转40---25
        allocate(kx_out(gdnf_x+1,gdnf_y+1,NT))
        allocate(xpos_out(gdnf_x+1))
        allocate(ypos_out(gdnf_y+1))
        do k0=1,NT
            do j0=1,gdnf_y+1
                ypos_out(j0)=k_ypos(gdnf_y+1-j0+1)
                do i0=1,gdnf_x+1
                    kx_out(i0,j0,k0)=k_kx(i0,gdnf_y+1-j0+1,k0)
                enddo
!C        write(*,*)"k_kx(:,",j0,",",k0,")=",k_kx(:,j0,k0)
!C        write(*,*)"kx_out(:,",j0,",",k0,")=",kx_out(:,gdnf_y+1-j0+1,k0)
            enddo
        enddo
            xpos_out(:)=k_xpos(:)
        write(*,*)"ypos_out=",ypos_out
        write(*,*)"xpos_out=",xpos_out
        deallocate(k_xpos)
        deallocate(k_ypos)
        deallocate(k_kx)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C! 给定范围 (120-100)/0.125=160,(40-25)/0.125=120 插值
!C 给定范围 (116-108)/0.125=64, (42-33)/0.125=72 插值
        gdnl_x=(corner_W-corner_E)/gd_l
        write(*,*)'gdnl_x=',gdnl_x
        gdnl_y=(corner_N-corner_S)/gd_l
        write(*,*)'gdnl_y=',gdnl_y
        
        allocate(c_kx(gdnl_x+1,gdnl_y+1,NT))
        allocate(czh_xpos(gdnl_x+1))
        allocate(czh_ypos(gdnl_y+1))
!C        write(*,*)"kx_out=",kx_out
        do i0=1,gdnl_x+1
            czh_xpos(i0)=gd_l*(i0-1)+corner_E
        enddo
        do j0=1,gdnl_y+1
            czh_ypos(j0)=gd_l*(j0-1)+corner_S
        enddo
!C        write(*,*)"czh_xpos=",czh_xpos
!C        write(*,*)"czh_ypos=",czh_ypos
120     format(2i4,i2,4f8.3,2f6.1) 
125     format(2i4,f8.3,i4,f8.3,i4,f8.3,i4,f8.3) 
130     format(3f8.1) 
        do k0=1,NT
        do j0=1,gdnl_y+1
        do i0=1,gdnl_x+1
            m=int((czh_xpos(i0)-corner_E)/gd_f)+1
            n=int((czh_ypos(j0)-corner_S)/gd_f)+1
!C            write(*,*)"mmmm=",m,"n=",n
!C            write(*,120)i0,j0,k0,czh_xpos(i0),czh_ypos(j0),xpos_out(m),
!C     1      ypos_out(n),czh_tp(i0,j0,n0,k0),m_tpdata6h(m,n,n0,k0)
!C            write(*,125)n0,i0,czh_xpos(i0),j0,czh_ypos(j0),
!C    1                       m,xpos_out(m),n,ypos_out(n)
!C             write(*,'(4f8.1)')m_tpdata6h(m,n,n0,k0),m_tpdata6h(m+1,n,n0,k0),
!C      1      m_tpdata6h(m,n+1,n0,k0),m_tpdata6h(m+1,n+1,n0,k0)
            tw=ABS(czh_xpos(i0)-xpos_out(m))
            te=ABS(xpos_out(m+1)-czh_xpos(i0))
            tn=ABS(czh_ypos(j0)-ypos_out(n))
            ts=ABS(ypos_out(n+1)-czh_ypos(j0))
            we=ABS(xpos_out(m+1)-xpos_out(m))
            ns=ABS(ypos_out(n+1)-ypos_out(n))
            x1=1-(tw/we)
            x2=1-(te/we)
            x3=1-(tw/we)
            x4=1-(te/we)
            y1=1-(tn/ns)
            y2=1-(ts/ns)
         t1=(x1*kx_out(m,n,k0))  +(x2*kx_out(m+1,n,k0))
         t2=(x3*kx_out(m,n+1,k0))+(x4*kx_out(m+1,n+1,k0))
            c_kx(i0,j0,k0)=(y1*t1) +(y2*t2)
!C             if (c_q(i0,j0,n0,k0) < -0.1) then
!C                 write(*,*)i0,j0,k0,czh_xpos(i0),czh_ypos(j0),
!C      1          c_q(i0,j0,n0,k0)
!C             endif
        enddo
        enddo
        enddo
!C        write(*,*)"c_kx=",c_kx
        print *, "*** SUCCESS chazhi data!***" 
        deallocate(kx_out)
        deallocate(xpos_out)
        deallocate(ypos_out)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
!C 创建插值后netCDF文件 ECENS_2015100912_24_6_tp_INS.NC
        !C返回文件对应的ID，如果存在则覆盖
        ncfileout=trim(pathname(:(len_trim(pathname)-3)))//"_INS.NC"
        write(*,*)'ncfileout=',ncfileout
        status = nf90_create(trim(ncfileout),NF90_CLOBBER,ncid_out)
        call check(status)
        write(*,*)'ncid_out=',ncid_out
        !C  定义维数，返回一个对应的ID,取100-120E,25-40N,110,40--120,25
        call check(nf90_def_dim(ncid_out,DimNames(2),gdnl_x+1,
     1     longitude_dim))
        call check(nf90_def_dim(ncid_out,DimNames(1),gdnl_y+1,
     1  latitude_dim))
        write(*,*)"NT=",NT
        call check(nf90_def_dim(ncid_out,DimNames(3),NT,
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
        kx_dims(1)=longitude_dim
        kx_dims(2)=latitude_dim
        kx_dims(3)=time_dim
        write(*,*)'kx_dims=',kx_dims
        !C 定义变量，返回一个对应的ID
        call check(nf90_def_var(ncid_out,VarNames(2),NF90_FLOAT,
     1  longitude_dims,longitude_id))
        write(*,*)'longitude_id=',longitude_id
        call check(nf90_def_var(ncid_out,VarNames(1),NF90_FLOAT,
     1    latitude_dims,latitude_id))
        write(*,*)'latitude_id=',latitude_id
        call check(nf90_def_var(ncid_out,VarNames(3),NF90_INT,time_dims,
     1  time_id))
        write(*,*)'time_id=',time_id
        call check(nf90_def_var(ncid_out,VarNames(4),NF90_FLOAT,kx_dims,
     1  kx_id))
        write(*,*)'kx_id=',kx_id
        !C    /* assign attributes */ 
        CALL check(nf90_put_att(ncid_out,longitude_id,
     1   longitude_unitsname,longitude_units))
        write(*,*)'longitude_unitsname=',longitude_unitsname
        CALL check(nf90_put_att(ncid_out,longitude_id,"long_name",
     1   "longitude"))
        CALL check(nf90_put_att(ncid_out,latitude_id,"units",
     1   "degrees_north"))
        CALL check(nf90_put_att(ncid_out,latitude_id,"long_name",
     1   "latitude"))
        CALL check(nf90_put_att(ncid_out,time_id,"units",
     1   uuuuu)) 
        CALL check(nf90_put_att(ncid_out,time_id,"long_name","time"))
        CALL check(nf90_put_att(ncid_out,time_id,"reference_time",
     1  reference_time))
        CALL check(nf90_put_att(ncid_out,time_id,"reference_date",
     1  reference_date))
        CALL check(nf90_put_att(ncid_out,kx_id,"units","K"))
        CALL check(nf90_put_att(ncid_out,NF90_GLOBAL,"Conventions",
     1   "zhang weimin"))
        CALL check(nf90_put_att(ncid_out,NF90_GLOBAL,"history",
     1   "by zhang weimin"))
        !C 定义完成，关闭定义模式
        call check(nf90_enddef(ncid_out))
        ! 写入数据
        call check(nf90_put_var(ncid_out,longitude_id,czh_xpos))
        call check(nf90_put_var(ncid_out,latitude_id,czh_ypos))
        call check(nf90_put_var(ncid_out,time_id,tpos))
        write(*,*)'put_att succes' 
        call check(nf90_put_var(ncid_out,kx_id,c_kx))
        call check(nf90_close(ncid_out))
        print *, "SUCCESS writing nc file ",ncfileout
!C        write(*,*)"c_kx=",c_kx
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        deallocate(DimNames)
        deallocate(DimNs)       
        deallocate(DimIds)
        deallocate(VarNames)       
        deallocate(VarTypes)
        deallocate(VarDims)       
        deallocate(VarIds)
        deallocate(AttTypes)       
        deallocate(AttLens)
        deallocate(AttNums)       
        deallocate(tpos)
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
          year_out=year_out-1
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