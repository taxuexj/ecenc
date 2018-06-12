        program main
        use netcdf
        implicit none
        character(len=100) :: rawpath,filename,pathname,ncfileout
        character(len=100) :: purename
        character(len=100) :: grdfileout,filename_4,Fname_3,gsctl
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
        real,parameter            :: gd_l=0.125
        integer(KIND=4)  :: gdnf_x,gdnf_y,gdnl_x,gdnl_y,gdni_x,gdni_y
        logical alive
        integer :: ncid,status,xtype,ndims,nvars,DimID,VarID,NX,NY,NT
        integer :: VarNum,nGlobalAtts,unlimitedDimId,i0,j0,k0,k1
        integer :: tp_FillValue,tp_missing_value
        character(LEN=50) :: xname,yname,tname,vname,argstr
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
        !C  open file for input and output
        pathname=trim(rawpath)//"/"//purename
        inquire(file=trim(pathname),exist=alive)
        if (alive .eq. .false.)then
            write(*,*)'pathname=',pathname,' not exist'
            go to 999
        endif
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
C          NX = DimNs(1)
C          NY = DimNs(2)
C          NT = DimNs(3)
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
     1              +tp_add_offset)*1000
                enddo
            enddo
        enddo
        print *, "*** SUCCESS getting rain tp data!***"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C get the 间隔 grid preciption data
        do k0=1,DimNs(3)-1
            do j0=1,DimNs(2)
                do i0=1,DimNs(1)
                    tpdata6h(i0,j0,k0)=tp(i0,j0,k0+1)-tp(i0,j0,k0)
                    if (tpdata6h(i0,j0,k0) < -0.1) then
       write(*,'(i4.4,1x,i4.4,i2.2,f5.1)')i0,j0,k0,tpdata6h(i0,j0,k0)
                    endif
                enddo
            enddo
        enddo
        print *, "*** SUCCESS getting betewn time tp data!***"
C! 给定范围 (116-108)/0.125=64,(42-33)/0.125=72 插值
        gdnl_x=(corner_W-corner_E)/gd_l
        write(*,*)'gdnl_x=',gdnl_x
        gdnl_y=(corner_N-corner_S)/gd_l
        write(*,*)'gdnl_y=',gdnl_y
        gdni_x=(corner_E-0)/gd_l
        write(*,*)'gdni_x=',gdni_x
        gdni_y=(90-corner_N)/gd_l
        write(*,*)'gdni_y=',gdni_y
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 给定范围 (120-100)/0.125=160,(40-25)/0.125=120,100/0.125=800,(90-40)/0.125=400
        allocate(m_tpdata6h(gdnl_x+1,gdnl_y+1,k1))
        allocate(xpos_out(gdnl_x+1))
        do i0=1,gdnl_x+1
            xpos_out(i0)=xpos(gdni_x+i0)
        enddo
        allocate(ypos_out(gdnl_y+1))
        do j0=1,gdnl_y+1
            ypos_out(j0)=ypos(gdni_y+j0)
        enddo
        do k0=1,k1
            do j0=1,gdnl_y+1
                do i0=1,gdnl_x+1
                   m_tpdata6h(i0,j0,k0)=tpdata6h(gdni_x+i0,gdni_y+j0,k0)
                    if (m_tpdata6h(i0,j0,k0) < -0.1) then
            write(*,'(2i5.5,i2.2,f8.1)')i0,j0,k0,m_tpdata6h(i0,j0,k0)
                    endif
                enddo
            enddo
        enddo
        print *, "*** SUCCESS cutting data!***"  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成micaps 3类数据
        diamond_type = 3
140     format(i5.5,f8.3,f7.3,i2,f5.1)
150     format(i4,3i3.2,i4) 
        do k0=1,k1
            call GetDateByHour(tpos_out(k0),year_out,month_out,
     1      day_out,hour_out)
            write(*,*)tpos_out(k0),year_out,month_out,day_out,hour_out
            write(cur_date,300)year_out,month_out,day_out,hour_out
            call adddatetime(year_out,month_out,day_out,hour_out,-24,
     1      "hr")
            write(*,*)"pre_date=",year_out,month_out,day_out,
     1      hour_out
            write(pre_date,300)year_out,month_out,day_out,hour_out
            write(comment,400)'diamond ',diamond_type,initial_date,
     1      "—",pre_date,"-",cur_date,"thin24HRain"
            write(*,*)comment
            write(mhz,'(i3.3)')(k0+1)*24-12
            write(*,*)mhz
            Fname_3=trim(rawpath)//"/T_"//pre_date//"-"//
     1      cur_date(5:10)//"_M3."//mhz
            open(10,file=trim(Fname_3),iostat=status)
            write(10,*)comment(:len_trim(comment))
            write(10,150)year_out,month_out,day_out,hour_out,-2
            write(10,'(a)')'0'
            write(10,'(a)')'1 2.5 0'
            write(10,'(i5,i6)')1,(gdnl_y+1)*(gdnl_x+1)
            do j0=1,gdnl_y+1
                do i0=1,gdnl_x+1
                    write(10,140)i0*j0,xpos_out(i0),ypos_out(j0),1,
     1              m_tpdata6h(i0,j0,k0)
                enddo
            enddo
            print *, "*** SUCCESS writing micaps 3 data!*"//Fname_3
            close(10)
        enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成micaps 4类数据 
        diamond_type = 4
300     format(i4,3i2.2)
400     format(a8,i1,1x,a10,a,a10,a,a10,a)
        do k0=1,k1
            write(*,*)'k0=',k0
            call GetDateByHour(tpos_out(k0),year_out,month_out,
     1      day_out,hour_out)
            write(*,*)tpos_out(k0),year_out,month_out,day_out,hour_out
            write(cur_date,300)year_out,month_out,day_out,hour_out
            write(*,*)'cur_date=',cur_date
            call adddatetime(year_out,month_out,day_out,hour_out,-24,
     1      "hr")
            write(*,*)"pre_date=",year_out,month_out,day_out
     1      ,hour_out 
            write(pre_date,300)year_out,month_out,day_out,hour_out
            write(comment,400)'diamond ',diamond_type,initial_date,
     1      "—",pre_date,"-",cur_date,"Thin24HRain"
            write(*,*)comment
            write(mhz,'(i3.3)')(k0+1)*24-12
            filename_4=trim(rawpath)//"/T_"//pre_date//"-"//
     1      cur_date(5:10)//"_M4."//mhz
            write(6,*)"write to file : ",filename_4
!c           write data to file
            open(30,file=filename_4,iostat=status)
            write(30,*)comment(:len_trim(comment))
            write(30,500)year,month,day,hour,0,9999
500         format(i4.2,3i3.2,i4,i6)
550         format(10f8.1)
            write(30,'(a)'),"0.125 -0.125 108 116 42 33 65 
     1      73 2.5 -100 100 0 0"
            write(30,550)m_tpdata6h(:,:,k0)
            close(30)
        enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 创建netCDF文件，返回文件对应的ID，如果存在则覆盖
!C        write(*,*)'ncfileout=',ncfileout
        write(*,*)'pathname=',pathname(1:(len_trim(pathname)-3))
        ncfileout=trim(pathname(1:(len_trim(pathname)-3)))//"_OUT.NC"
        write(*,*)'ncfileout=',ncfileout
        status = nf90_create(trim(ncfileout),NF90_CLOBBER,ncid_out)
        call check(status)
        write(*,*)'ncid_out=',ncid_out
!C       call check(nf90_redef(ncid_out))
!C  定义维数，返回一个对应的ID,取100-120E,25-40N,110,40--120,25
        call check(nf90_def_dim(ncid_out,DimNames(1),gdnl_x+1,
     1  longitude_dim))
        call check(nf90_def_dim(ncid_out,DimNames(2),gdnl_y+1,
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
     1  longitude_dims,longitude_id))
        write(*,*)'longitude_id=',longitude_id
        call check(nf90_def_var(ncid_out,VarNames(2),NF90_FLOAT,
     1  latitude_dims,latitude_id))
        write(*,*)'latitude_id=',latitude_id
        call check(nf90_def_var(ncid_out,VarNames(3),NF90_INT,time_dims,
     1  time_id))
        write(*,*)'time_id=',time_id
        call check(nf90_def_var(ncid_out,VarNames(4),NF90_FLOAT,tp_dims,
     1  tp_id))
        write(*,*)'tp_id=',tp_id
!C    /* assign attributes */ 
        CALL check(nf90_put_att(ncid_out,longitude_id,
     1  longitude_unitsname,longitude_units))
        CALL check(nf90_put_att(ncid_out,longitude_id,"long_name",
     1  "longitude"))
        CALL check(nf90_put_att(ncid_out,latitude_id,"units",
     1  "degrees_north"))
        CALL check(nf90_put_att(ncid_out,latitude_id,"long_name",
     1  "latitude"))
        CALL check(nf90_put_att(ncid_out,time_id,"units",
     1  "hours since 1900-01-01 00:00:0.0")) 
        CALL check(nf90_put_att(ncid_out,time_id,"long_name","time"))
        CALL check(nf90_put_att(ncid_out,time_id,"calendar",
     1  "gregorian"))
!C           tp_scale_factor_out(1)=6.28811220944982e-06
!C         CALL check(nf90_put_att(ncid_out,tp_id,"scale_factor",
!C      1   tp_scale_factor_out))
!C           tp_add_offset_out(1)=0.206036284654833
!C         CALL check(nf90_put_att(ncid_out,tp_id,"add_offset",
!C      1   tp_add_offset_out))
!C          tp_FillValue_out(1)=-32767
!C          CALL check(nf90_put_att(ncid_out,tp_id,"FillValue",
!C      1   tp_FillValue_out))
!C           tp_missing_value_out(1)=-32767
!C         CALL check(nf90_put_att(ncid_out,tp_id,"missing_value",
!C      1   tp_missing_value_out))
        CALL check(nf90_put_att(ncid_out,tp_id,"units","mm"))
        CALL check(nf90_put_att(ncid_out,tp_id,"long_name",
     1  "6 hours precipitation"))
        CALL check(nf90_put_att(ncid_out,NF90_GLOBAL,"Conventions",
     1  "zhang weimin"))
        CALL check(nf90_put_att(ncid_out,NF90_GLOBAL,"history",
     1  "by zhang weimin"))
!C 定义完成，关闭定义模式
        call check(nf90_enddef(ncid_out))
! 写入数据
        call check(nf90_put_var(ncid_out,longitude_id,xpos_out))
        call check(nf90_put_var(ncid_out,latitude_id,ypos_out))
        call check(nf90_put_var(ncid_out,time_id,tpos_out))
        write(*,*)tpos_out,k0
!C        write(*,550)m_tpdata6h
        call check(nf90_put_var(ncid_out,tp_id,m_tpdata6h))
! 关闭文件
        call check(nf90_close(ncid_out))
!提示写文件成功ll
        print *, "*** SUCCESS writing file ",ncfileout
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
!C 写grads二进制文件
!C        write(*,*)'grdfileout=',grdfileout
        write(*,*)'pathname=',pathname(1:(len_trim(pathname)-3))
        grdfileout=trim(pathname(1:(len_trim(pathname)-3)))//".GRD"
        write(*,*)'grdfileout=',grdfileout
        open(15,file=trim(grdfileout),form='binary')
        write(15)m_tpdata6h
        close(15)
        print *, "*** SUCCESS bin file ",grdfileout
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
!C 写gradsCTL文件
        call adddatetime(year,month,day,hour,+36,"hr")
        write(mnow,'(i2.2)')month       
        mnow=initial_date(5:6)
        if(mnow=="01")monthen="JAN"
        if(mnow=="02")monthen="FEB"
        if(mnow=="03")monthen="MAR"
        if(mnow=="04")monthen="APR"
        if(mnow=="05")monthen="MAY"
        if(mnow=="06")monthen="JUN"
        if(mnow=="07")monthen="JUL"
        if(mnow=="08")monthen="AUG"
        if(mnow=="09")monthen="SEP"
        if(mnow=="10")monthen="OCT"
        if(mnow=="11")monthen="NOV"
        if(mnow=="12")monthen="DEC"
600     format(i2.2,a1,i2.2,a3,i4.4,a4) 
        write(mytime,600)hour,"Z",day,monthen,year," 1dy"
        write(*,*)"mytime=",mytime
        fnctl=trim(pathname(:(len_trim(pathname)-3)))//"_INS.ctl"
        open(100,file=trim(fnctl))
        write(100,'(a)')"DSET "//trim(grdfileout)
        write(100,'(a)')"undef  -999.9"
        write(100,'(a)')"options byteswapped"
        write(100,'(a)')"title EC thin precipitation of 24 hour"
        write(100,'(a)')"options yrev"
        write(100,'(a)')"xdef 65 linear 108 0.125"
        write(100,'(a)')"ydef 73 linear 33 0.125"
        write(100,'(a)')"zdef 1 linear 1 1"
        write(100,'(a)')"tdef 4 linear "//trim(mytime)
        write(100,'(a)')"vars 1"
        write(100,'(a)')"TPsfc 0 228,1,0 * surface precipitation [mm]"
        write(100,'(a)')"endvars"
        close(100)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
!C 写grads GS文件 
        gsctl=trim(pathname(:(len_trim(pathname)-3)))//"_BIN.gs"
        open(110,file=trim(gsctl))
        write(110,'(a)')"'reinit'"
        write(110,'(a)')"'open "//trim(fnctl)//"'" 
        write(110,'(a)')"j=1"
        write(110,'(a)')"while(j<5)"
        write(110,'(a)')"'c'"
        write(110,'(a)')"'set display color white'"        
        write(110,'(a)')"'set map 4 1 10'"
        write(110,'(a)')"'set mpdset cnworld cnriver shanxi shanxi_q'"        
        write(110,'(a)')"'set parea 1 10 1 7.7'"
        write(110,'(a)')"'set csmooth on'"
        write(110,'(a)')"'set t 'j"
        write(110,'(a)')"'q time'"        
        write(110,'(a)')"say j' 'result"
        write(110,'(a)')"in_time1= subwrd(result,3)"        
        write(110,'(a)')"in_time_hour= substr(in_time1,1,2)"
        write(110,'(a)')"in_time_day= substr(in_time1,4,2)"        
        write(110,'(a)')"in_time_mon= substr(in_time1,6,3)"
        write(110,'(a)')"in_time_year= substr(in_time1,9,4)"        
        write(110,'(a)')"year=subwrd(in_time_year,1)"
        write(110,'(a)')"day=subwrd(in_time_day,1)" 
        write(110,'(a)')"hour=subwrd(in_time_hour,1)" 
        write(110,'(a)')" if(in_time_mon='JAN')"
        write(110,'(a)')"  month=01" 
        write(110,'(a)')" endif"
        write(110,'(a)')" if(in_time_mon='FEB')"        
        write(110,'(a)')"  month=02"
        write(110,'(a)')" endif" 
        write(110,'(a)')" if(in_time_mon='MAR')"
        write(110,'(a)')"  month=03" 
        write(110,'(a)')" endif"
        write(110,'(a)')" if(in_time_mon='APR')"        
        write(110,'(a)')"  month=04"
        write(110,'(a)')" endif"        
        write(110,'(a)')" if(in_time_mon='MAY')"
        write(110,'(a)')"  month=05" 
        write(110,'(a)')" endif"
        write(110,'(a)')" if(in_time_mon='JUN')"        
        write(110,'(a)')"  month=06"
        write(110,'(a)')" endif"        
        write(110,'(a)')" if(in_time_mon='JUL')"
        write(110,'(a)')"  month=07" 
        write(110,'(a)')" endif"
        write(110,'(a)')" if(in_time_mon='AUG')"        
        write(110,'(a)')"  month=08"
        write(110,'(a)')" endif"        
        write(110,'(a)')" if(in_time_mon='SEP')"
        write(110,'(a)')"  month=09" 
        write(110,'(a)')" endif"
        write(110,'(a)')" if(in_time_mon='OCT')"        
        write(110,'(a)')"  month=10"
        write(110,'(a)')" endif"        
        write(110,'(a)')" if(in_time_mon='NOV')"
        write(110,'(a)')"  month=11" 
        write(110,'(a)')" endif"
        write(110,'(a)')" if(in_time_mon='DEC')"        
        write(110,'(a)')"  month=12"
        write(110,'(a)')" endif"        
        write(110,'(a)')"say year month day hour"        
        write(110,'(a)')"'set grads off'"
        write(110,'(a)')"'set grid off'"        
        write(110,'(a)')"'set gxout shaded'"
        write(110,'(a)')"'set clevs 1 10 25 50 100 250'"        
        write(110,'(a)')"'set rgb 100 253 253 253'"
        write(110,'(a)')"'set rgb 21  166   242  143'"        
        write(110,'(a)')"'set rgb 22  61   186   61'"
        write(110,'(a)')"'set rgb 23  97  184   255'"        
        write(110,'(a)')"'set rgb 24  0  0   225'"
        write(110,'(a)')"'set rgb 25  250  0   250'"        
        write(110,'(a)')"'set rgb 26  128 0   64'"
        write(110,'(a)')"'set ccols 100 21 22 23 24 25 26 '"        
        write(110,'(a)')"'d TPsfc'"
        write(110,'(a)')"'run cbar.gs'"        
        write(110,'(a)')"'set gxout contour'"
        write(110,'(a)')"'set cint 5'"        
        write(110,'(a)')"'set clab on'"
        write(110,'(a)')"'d TPsfc'"
        write(110,'(a)')"'draw title ECthin 24rain 'year month day hour"
        write(110,'(a)')"'draw string 6.0 0.2 "//initial_date//"'"
        write(110,'(a)')"'printim ECthin_"//initial_date//
     1  "-'year month day hour'_BIN.gif gif'"        
        write(110,'(a)')"j=j+1"
        write(110,'(a)')"endwhile" 
        write(110,'(a)')"'quit'"
        close(110)
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
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
          !!add by zwm at 20170331
!C          lday = lday + int(ldtime/24)
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