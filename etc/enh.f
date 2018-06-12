        program main
        use netcdf
        implicit none
        character(len=100) :: rawpath,outpath,filename,pathname
        character(len=100) :: purename
        character(len=10) :: initial_date,cur_date,pre_date
        character(len=3) :: mhz,levstr
        integer(KIND=1) :: argnum
!C        real :: tw,te,tn,ts,t1,t2,ns,we,x1,x2,x3,x4,y1,y2
        character(len=100) :: ncfileout,grdfileout,filename_4,Fname_3
        character(len=12),parameter :: longitude_units = "degrees_east"
        character(len=5),parameter :: longitude_unitsname = "units"
        integer(KIND=4),parameter :: corner_E=108
        integer(KIND=4),parameter :: corner_W=116
        integer(KIND=4),parameter :: corner_N=42
        integer(KIND=4),parameter :: corner_S=33
        real,parameter            :: gd_f=0.25
        real,parameter            :: gd_l=0.125
        integer(KIND=4)  :: gdnf_x,gdnf_y,gdnl_x,gdnl_y,gdni_x,gdni_y
        logical alive
        integer :: ncid,status,xtype,ndims,nvars,DimID,VarID
        integer :: VarNum,nGlobalAtts,unlimitedDimId
        integer :: m,n,NX,NY,NL,NT,i0,j0,n0,k0
        character(LEN=50) :: xname,yname,tname,vname,argstr
        real,allocatable,dimension(:,:,:,:) :: q,pv,d,t,gh,v,u,w,r
        real,allocatable,dimension(:,:,:,:) :: m_q,m_pv,m_d,m_t,m_gh
        real,allocatable,dimension(:,:,:,:) :: m_v,m_u,m_w,m_r
        real,allocatable,dimension(:,:,:,:) :: c_q,c_pv,c_d,c_t,c_gh
        real,allocatable,dimension(:,:,:,:) :: c_v,c_u,c_w,c_r,sj
        real,allocatable,dimension(:) :: xpos,xpos_out,czh_xpos
        real,allocatable,dimension(:) :: ypos,ypos_out,czh_ypos
        integer(KIND=4),allocatable,dimension(:) :: lev,lev_out
        integer,allocatable,dimension(:) :: tpos,DimNs,VarNs,dimids
        integer,allocatable,dimension(:) :: VarTypes,VarDims,AttTypes
        integer,allocatable,dimension(:) :: AttLens,AttNums,VarIds
        character(LEN=50),allocatable,dimension(:) :: DimNames,VarNames
!C        real :: tp_scale_factor,tp_add_offset,ling
        !C  /* dimension ids */
        integer :: ncid_out
        integer :: longitude_dim,latitude_dim,lev_dim,time_dim
        !C  /* variable ids */
        integer :: longitude_id,latitude_id,lev_id,time_id,q_id
        integer :: pv_id,d_id,t_id,gh_id,v_id,u_id,w_id,r_id
        !C  /* variable shapes */
        integer :: longitude_dims(1),latitude_dims(1),lev_dims(1)
        integer :: time_dims(1),v_dims(4),u_dims(4),w_dims(4)
        integer :: q_dims(4),pv_dims(4),d_dims(4),t_dims(4),gh_dims(4)
        integer,allocatable :: tpos_out(:)
        !C   /* attribute vectors */
        real :: scale_factor(13),add_offset(13),r_dims(4)
        integer :: FillValue(13),missing_value(13)
        character(LEN=50) :: units(13),long_name(13)
        character(len=100)    :: fngrd,fnctl,fngs,fngmf,mytime
        integer :: diamond_type
        character(len=100) :: comment,m4parameter
        integer :: year,month,day,hour,a,num_days0
        integer :: hour_in,year_out,month_out,day_out,hour_out
        logical,external :: isleap
        integer,external :: GetYearByDay,GetMonthByDay
        logical :: leap
        integer :: year_test,day_test,year_day,month_day
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 获取netcdf文件路径名字，判断是否存在，获取初始场时刻，并打开
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
        if(alive)then
            write(*,*)'pathname=',trim(pathname),' exist'
        else
            write(*,*)'pathname=',trim(pathname),'not exist'
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
            status=nf90_inquire_dimension(ncid,DimID,DimNames(DimID),
     1      DimNs(DimID))
            call check(status)
            write(*,*)DimID,'=',DimNames(DimID),DimNs(DimID)
        enddo
        write(*,*)DimNs
        NX = DimNs(1)
        NY = DimNs(2)
        NL = DimNs(3)
        NT = DimNs(4)
        allocate(xpos(NX))
        allocate(ypos(NY))
        allocate(lev(NL))
        allocate(tpos(NT))
        allocate(sj(NX,NY,NL,NT))
        allocate(q(NX,NY,NL,NT))
        allocate(pv(NX,NY,NL,NT))
        allocate(d(NX,NY,NL,NT))
        allocate(t(NX,NY,NL,NT))
        allocate(gh(NX,NY,NL,NT))
        allocate(v(NX,NY,NL,NT))
        allocate(u(NX,NY,NL,NT))
        allocate(w(NX,NY,NL,NT))
        allocate(r(NX,NY,NL,NT))
        !C 给定范围 (120-100)/0.25=80,(40-25)/0.25=60,(100-60)/0.25=160,(60-40)/0.25=80
        gdnf_x=(corner_W-corner_E)/gd_f
        write(*,*)'gdnf_x=',gdnf_x
        gdnf_y=(corner_N-corner_S)/gd_f
        write(*,*)'gdnf_y=',gdnf_y
        gdni_x=(corner_E-60)/gd_f
        write(*,*)'gdni_x=',gdni_x
        gdni_y=(60-corner_N)/gd_f
        write(*,*)'gdni_y=',gdni_y
        
        allocate(m_q(gdnf_x+1,gdnf_y+1,NL,NT))
        allocate(m_pv(gdnf_x+1,gdnf_y+1,NL,NT))
        allocate(m_d(gdnf_x+1,gdnf_y+1,NL,NT))
        allocate(m_t(gdnf_x+1,gdnf_y+1,NL,NT))
        allocate(m_gh(gdnf_x+1,gdnf_y+1,NL,NT))
        allocate(m_v(gdnf_x+1,gdnf_y+1,NL,NT))
        allocate(m_u(gdnf_x+1,gdnf_y+1,NL,NT))
        allocate(m_w(gdnf_x+1,gdnf_y+1,NL,NT))
        allocate(m_r(gdnf_x+1,gdnf_y+1,NL,NT))
        allocate(xpos_out(gdnf_x+1))
        allocate(ypos_out(gdnf_y+1))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
!C get the variables names Types number and IDs
        do VarNum=1,nvars
          call check(nf90_inquire_variable(ncid,VarNum,VarNames(VarNum),
     1      VarTypes(VarNum),VarDims(VarNum),dimids))
            write(*,*)'VarNum = ',VarNum
          write(*,*)'VName=',VarNames(VarNum),'Types=',VarTypes(VarNum)
            write(*,*)'VarDims=',VarDims(VarNum)
            write(*,*)'dimids=',dimids
            call check(nf90_inq_varid(ncid,VarNames(VarNum),
     1      VarIds(VarNum)))
            write(*,*)'VarIds=',VarIds(VarNum)
        enddo 
        call check(nf90_get_var(ncid,VarIds(1),xpos))
        CALL check(nf90_get_var(ncid,VarIds(2),ypos))
        CALL check(nf90_get_var(ncid,VarIds(3),lev))
        CALL check(nf90_get_var(ncid,VarIds(4),tpos))
        xpos_out(:)=xpos(gdni_x+1:gdni_x+1+gdnf_x)
        write(*,*)"xpos_out=",xpos_out
        ypos_out(:)=ypos(gdni_y+1:gdni_y+1+gdnf_y)
        write(*,*)"ypos_out=",ypos_out
        write(*,*)"lev=",lev
        write(*,*)"tpos=",tpos
        do VarNum=5,nvars
            write(*,*)"VarNum=",VarNum
            CALL check(nf90_get_att(ncid,VarIds(VarNum),"scale_factor",
     1      scale_factor(VarNum)))
            write(*,*)'scale_factor=',scale_factor(VarNum)
            CALL check(nf90_get_att(ncid,VarIds(VarNum),"add_offset",
     1      add_offset(VarNum)))
            write(*,*)'add_offset=',add_offset(VarNum)
            CALL check(nf90_get_att(ncid,VarIds(VarNum),"_FillValue",
     1      FillValue(VarNum)))
            write(*,*)'FillValue=',FillValue(VarNum)
            CALL check(nf90_get_att(ncid,VarIds(VarNum),"missing_value",
     1      missing_value(VarNum)))
            write(*,*)'missing_value=',missing_value(VarNum)
            CALL check(nf90_get_att(ncid,VarIds(VarNum),"units",
     1      units(VarNum)))
            write(*,*)'units=',units(VarNum)
            CALL check(nf90_get_att(ncid,VarIds(VarNum),"long_name",
     1      long_name(VarNum)))
            write(*,*)'long_name=',long_name(VarNum)
            CALL check(nf90_get_var(ncid,VarIds(VarNum),sj))
            if (VarNames(VarNum) .eq. 'q') then
                q=(sj*scale_factor(VarNum)+add_offset(VarNum))*1000
                m_q(:,:,:,:)=q(gdni_x+1:gdni_x+1+gdnf_x,
     1          gdni_y+1:gdni_y+1+gdnf_y,:,:)
                call write_to_m3(xpos_out,ypos_out,tpos,lev,m_q,
     1          initial_date,"Q",rawpath,gdnf_x+1,gdnf_y+1,NL,NT)
        write(m4parameter,100)gd_f,gd_f*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnf_x+1,gdnf_y+1," 2 0 20 0 0" 
        write(*,*)"m4parameter=",m4parameter     
                call write_to_m4(tpos,lev,m_q,initial_date,"Q",
     1          rawpath,gdnf_x+1,gdnf_y+1,NL,NT,m4parameter)
            elseif (VarNames(VarNum) .eq. 'pv') then
             pv=(sj*scale_factor(VarNum)+add_offset(VarNum))*100000000
                m_pv(:,:,:,:)=pv(gdni_x+1:gdni_x+1+gdnf_x,
     1          gdni_y+1:gdni_y+1+gdnf_y,:,:)
                call write_to_m3(xpos_out,ypos_out,tpos,lev,m_pv,
     1          initial_date,"PV",rawpath,gdnf_x+1,gdnf_y+1,NL,NT)
        write(m4parameter,100)gd_f,gd_f*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnf_x+1,gdnf_y+1," 500 -3000 3000 0 0" 
        write(*,*)"m4parameter=",m4parameter          
                call write_to_m4(tpos,lev,m_pv,initial_date,"PV",
     1          rawpath,gdnf_x+1,gdnf_y+1,NL,NT,m4parameter)
            elseif (VarNames(VarNum) .eq. 'd') then
                CALL check(nf90_get_var(ncid,VarIds(VarNum),d))
                d=(sj*scale_factor(VarNum)+add_offset(VarNum))*1000000
                m_d(:,:,:,:)=d(gdni_x+1:gdni_x+1+gdnf_x,
     1          gdni_y+1:gdni_y+1+gdnf_y,:,:)
                call write_to_m3(xpos_out,ypos_out,tpos,lev,m_d,
     1          initial_date,"D",rawpath,gdnf_x+1,gdnf_y+1,NL,NT)
        write(m4parameter,100)gd_f,gd_f*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnf_x+1,gdnf_y+1," 200 -1000 1000 0 0"      
                call write_to_m4(tpos,lev,m_d,initial_date,"D",
     1          rawpath,gdnf_x+1,gdnf_y+1,NL,NT,m4parameter)
            elseif (VarNames(VarNum) .eq. 't') then
                CALL check(nf90_get_var(ncid,VarIds(VarNum),t))
                t=(sj*scale_factor(VarNum)+add_offset(VarNum))-273.15
                m_t(:,:,:,:)=t(gdni_x+1:gdni_x+1+gdnf_x,
     1          gdni_y+1:gdni_y+1+gdnf_y,:,:)
                call write_to_m3(xpos_out,ypos_out,tpos,lev,m_t,
     1          initial_date,"T",rawpath,gdnf_x+1,gdnf_y+1,NL,NT)
        write(m4parameter,100)gd_f,gd_f*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnf_x+1,gdnf_y+1," 4 -50 50 0 0"           
                call write_to_m4(tpos,lev,m_t,initial_date,"T",
     1          rawpath,gdnf_x+1,gdnf_y+1,NL,NT,m4parameter)
            elseif (VarNames(VarNum) .eq. 'gh') then
                CALL check(nf90_get_var(ncid,VarIds(VarNum),gh))
                gh=(sj*scale_factor(VarNum)+add_offset(VarNum))/9.8
                m_gh(:,:,:,:)=gh(gdni_x+1:gdni_x+1+gdnf_x,
     1          gdni_y+1:gdni_y+1+gdnf_y,:,:)
                call write_to_m3(xpos_out,ypos_out,tpos,lev,m_gh,
     1          initial_date,"GH",rawpath,gdnf_x+1,gdnf_y+1,NL,NT)
        write(m4parameter,100)gd_f,gd_f*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnf_x+1,gdnf_y+1," 4 100 800 0 0"                
                call write_to_m4(tpos,lev,m_gh,initial_date,"GH",
     1          rawpath,gdnf_x+1,gdnf_y+1,NL,NT,m4parameter)
            elseif (VarNames(VarNum) .eq. 'v') then
                CALL check(nf90_get_var(ncid,VarIds(VarNum),v))
                v=sj*scale_factor(VarNum)+add_offset(VarNum)
                m_v(:,:,:,:)=v(gdni_x+1:gdni_x+1+gdnf_x,
     1          gdni_y+1:gdni_y+1+gdnf_y,:,:)
                call write_to_m3(xpos_out,ypos_out,tpos,lev,m_v,
     1          initial_date,"V",rawpath,gdnf_x+1,gdnf_y+1,NL,NT)
        write(m4parameter,100)gd_f,gd_f*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnf_x+1,gdnf_y+1," 2 -40 40 0 0"                     
                call write_to_m4(tpos,lev,m_v,initial_date,"V",
     1          rawpath,gdnf_x+1,gdnf_y+1,NL,NT,m4parameter)
            elseif (VarNames(VarNum) .eq. 'u') then
                CALL check(nf90_get_var(ncid,VarIds(VarNum),u))
                u=sj*scale_factor(VarNum)+add_offset(VarNum)
                m_u(:,:,:,:)=u(gdni_x+1:gdni_x+1+gdnf_x,
     1          gdni_y+1:gdni_y+1+gdnf_y,:,:)
                call write_to_m3(xpos_out,ypos_out,tpos,lev,m_u,
     1          initial_date,"U",rawpath,gdnf_x+1,gdnf_y+1,NL,NT)
        write(m4parameter,100)gd_f,gd_f*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnf_x+1,gdnf_y+1," 2 -40 40 0 0"                          
                call write_to_m4(tpos,lev,m_u,initial_date,"U",
     1          rawpath,gdnf_x+1,gdnf_y+1,NL,NT,m4parameter)
            elseif (VarNames(VarNum) .eq. 'w') then
                CALL check(nf90_get_var(ncid,VarIds(VarNum),w))
                w=sj*scale_factor(VarNum)+add_offset(VarNum)
                m_w(:,:,:,:)=w(gdni_x+1:gdni_x+1+gdnf_x,
     1          gdni_y+1:gdni_y+1+gdnf_y,:,:)
                call write_to_m3(xpos_out,ypos_out,tpos,lev,m_w,
     1          initial_date,"W",rawpath,gdnf_x+1,gdnf_y+1,NL,NT)
        write(m4parameter,100)gd_f,gd_f*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnf_x+1,gdnf_y+1," 2 -20 20 0 0"                          
                call write_to_m4(tpos,lev,m_w,initial_date,"W",
     1          rawpath,gdnf_x+1,gdnf_y+1,NL,NT,m4parameter)
            elseif (VarNames(VarNum) .eq. 'r') then
                CALL check(nf90_get_var(ncid,VarIds(VarNum),r))
                r=sj*scale_factor(VarNum)+add_offset(VarNum)
                m_r(:,:,:,:)=r(gdni_x+1:gdni_x+1+gdnf_x,
     1          gdni_y+1:gdni_y+1+gdnf_y,:,:)
                call write_to_m3(xpos_out,ypos_out,tpos,lev,m_r,
     1          initial_date,"R",rawpath,gdnf_x+1,gdnf_y+1,NL,NT)
        write(m4parameter,100)gd_f,gd_f*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnf_x+1,gdnf_y+1," 10 0 100 0 0"                          
                call write_to_m4(tpos,lev,m_r,initial_date,"R",
     1          rawpath,gdnf_x+1,gdnf_y+1,NL,NT,m4parameter)
            endif
        enddo
        call check(nf90_close(ncid))
        write(*,*)'*** SUCCESS getting nc data ***'
        deallocate(DimNs)
        deallocate(DimIds)
        deallocate(VarTypes)
        deallocate(VarDims)
        deallocate(VarIds)
        deallocate(AttTypes)
        deallocate(AttLens)
        deallocate(AttNums)
        deallocate(q)
        deallocate(pv)
        deallocate(d)
        deallocate(t)
        deallocate(gh)
        deallocate(v)
        deallocate(u)
        deallocate(w)
        deallocate(r)
        deallocate(sj)
        deallocate(xpos)
        deallocate(ypos)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
C! 给定范围 (116-108)/0.125=160,(42-33)/0.125=72 插值
        gdnl_x=(corner_W-corner_E)/gd_l
        write(*,*)'gdnl_x=',gdnl_x
        gdnl_y=(corner_N-corner_S)/gd_l
        write(*,*)'gdnl_y=',gdnl_y
        allocate(c_q(gdnl_x+1,gdnl_y+1,NL,NT))
        allocate(c_pv(gdnl_x+1,gdnl_y+1,NL,NT))
        allocate(c_d(gdnl_x+1,gdnl_y+1,NL,NT))
        allocate(c_t(gdnl_x+1,gdnl_y+1,NL,NT))
        allocate(c_gh(gdnl_x+1,gdnl_y+1,NL,NT))
        allocate(c_v(gdnl_x+1,gdnl_y+1,NL,NT))
        allocate(c_u(gdnl_x+1,gdnl_y+1,NL,NT))
        allocate(c_w(gdnl_x+1,gdnl_y+1,NL,NT))
        allocate(c_r(gdnl_x+1,gdnl_y+1,NL,NT))
        allocate(czh_xpos(gdnl_x+1))
        allocate(czh_ypos(gdnl_y+1))
!C----------------------------------------------------------------------
        do i0=1,gdnl_x+1
            czh_xpos(i0)=gd_l*(i0-1)+corner_E
        enddo
        do j0=1,gdnl_y+1
            czh_ypos(j0)=corner_N-gd_l*(j0-1)
        enddo
        write(*,*)"xpos_out=",xpos_out
        write(*,*)"ypos_out=",ypos_out
        write(*,*)"czh_xpos=",czh_xpos
        write(*,*)"czh_ypos=",czh_ypos
        write(*,*)"NL=",NL
        write(*,*)"NT=",NT
        write(*,*)"gdnf_x=",gdnf_x
        write(*,*)"gdnf_y=",gdnf_y
        write(*,*)"gdnl_x=",gdnl_x
        write(*,*)"gdnl_y=",gdnl_y
c       write(*,'(15f5.1)')((m_q(i0,j0,1,1),i0=1,gdnf_x+1),j0=1,gdnf_y+1)
        call insertdata(xpos_out,ypos_out,m_q,czh_xpos,czh_ypos,
     1  corner_E,corner_N,gd_f,gdnf_x,gdnf_y,gdnl_x,gdnl_y,NL,NT,c_q)
c       write(*,'(15f5.1)')((c_q(i0,j0,1,1),i0=1,gdnl_x+1),j0=1,gdnl_y+1)
        call insertdata(xpos_out,ypos_out,m_pv,czh_xpos,czh_ypos,
     1  corner_E,corner_N,gd_f,gdnf_x,gdnf_y,gdnl_x,gdnl_y,NL,NT,c_pv)
        
c        write(*,'(15f5.1)')m_d(:,:,1,1)
        call insertdata(xpos_out,ypos_out,m_d,czh_xpos,czh_ypos,
     1  corner_E,corner_N,gd_f,gdnf_x,gdnf_y,gdnl_x,gdnl_y,NL,NT,c_d)
c        write(*,'(15f5.1)')c_d(:,:,1,1)

        call insertdata(xpos_out,ypos_out,m_t,czh_xpos,czh_ypos,
     1  corner_E,corner_N,gd_f,gdnf_x,gdnf_y,gdnl_x,gdnl_y,NL,NT,c_t)

        call insertdata(xpos_out,ypos_out,m_gh,czh_xpos,czh_ypos,
     1  corner_E,corner_N,gd_f,gdnf_x,gdnf_y,gdnl_x,gdnl_y,NL,NT,c_gh)

        call insertdata(xpos_out,ypos_out,m_v,czh_xpos,czh_ypos,
     1  corner_E,corner_N,gd_f,gdnf_x,gdnf_y,gdnl_x,gdnl_y,NL,NT,c_v)

        call insertdata(xpos_out,ypos_out,m_u,czh_xpos,czh_ypos,
     1  corner_E,corner_N,gd_f,gdnf_x,gdnf_y,gdnl_x,gdnl_y,NL,NT,c_u)

        call insertdata(xpos_out,ypos_out,m_w,czh_xpos,czh_ypos,
     1  corner_E,corner_N,gd_f,gdnf_x,gdnf_y,gdnl_x,gdnl_y,NL,NT,c_w)

        call insertdata(xpos_out,ypos_out,m_r,czh_xpos,czh_ypos,
     1  corner_E,corner_N,gd_f,gdnf_x,gdnf_y,gdnl_x,gdnl_y,NL,NT,c_r)

        deallocate(m_q)
        deallocate(m_pv)
        deallocate(m_d)
        deallocate(m_t)
        deallocate(m_gh)
        deallocate(m_v)
        deallocate(m_u)
        deallocate(m_w)
        deallocate(m_r)
        deallocate(xpos_out)
        deallocate(ypos_out)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成micaps 3类 插值后数据
        call write_to_m3(czh_xpos,czh_ypos,tpos,lev,c_q,initial_date,
     1  "Q",rawpath,gdnl_x+1,gdnl_y+1,NL,NT)
        call write_to_m3(czh_xpos,czh_ypos,tpos,lev,c_pv,initial_date,
     1  "PV",rawpath,gdnl_x+1,gdnl_y+1,NL,NT)
        call write_to_m3(czh_xpos,czh_ypos,tpos,lev,c_d,initial_date,
     1  "D",rawpath,gdnl_x+1,gdnl_y+1,NL,NT)
        call write_to_m3(czh_xpos,czh_ypos,tpos,lev,c_t,initial_date,
     1  "T",rawpath,gdnl_x+1,gdnl_y+1,NL,NT)
        call write_to_m3(czh_xpos,czh_ypos,tpos,lev,c_gh,initial_date,
     1  "GH",rawpath,gdnl_x+1,gdnl_y+1,NL,NT)
        call write_to_m3(czh_xpos,czh_ypos,tpos,lev,c_v,initial_date,
     1  "V",rawpath,gdnl_x+1,gdnl_y+1,NL,NT)
        call write_to_m3(czh_xpos,czh_ypos,tpos,lev,c_u,initial_date,
     1  "U",rawpath,gdnl_x+1,gdnl_y+1,NL,NT)
        call write_to_m3(czh_xpos,czh_ypos,tpos,lev,c_w,initial_date,
     1  "W",rawpath,gdnl_x+1,gdnl_y+1,NL,NT)
        call write_to_m3(czh_xpos,czh_ypos,tpos,lev,c_r,initial_date,
     1  "R",rawpath,gdnl_x+1,gdnl_y+1,NL,NT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成micaps 4类 插值 后 数据
100     format(2f7.3,6i4,a) 

        write(m4parameter,100)gd_l,gd_l*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnl_x+1,gdnl_y+1," 2 0 20 0 0" 
        write(*,*)"m4parameter=",m4parameter
        call write_to_m4(tpos,lev,c_q,initial_date,"Q",rawpath,
     1  gdnl_x+1,gdnl_y+1,NL,NT,m4parameter)
     
        write(m4parameter,100)gd_l,gd_l*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnl_x+1,gdnl_y+1," 500 -3000 3000 0 0" 
        call write_to_m4(tpos,lev,c_pv,initial_date,"PV",rawpath,
     1  gdnl_x+1,gdnl_y+1,NL,NT,m4parameter)
     
        write(m4parameter,100)gd_l,gd_l*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnl_x+1,gdnl_y+1," 200 -1000 1000 0 0"      
        call write_to_m4(tpos,lev,c_d,initial_date,"D",rawpath,
     1  gdnl_x+1,gdnl_y+1,NL,NT,m4parameter)
     
        write(m4parameter,100)gd_l,gd_l*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnl_x+1,gdnl_y+1," 4 -40 40 0 0"      
        call write_to_m4(tpos,lev,c_t,initial_date,"T",rawpath,
     1  gdnl_x+1,gdnl_y+1,NL,NT,m4parameter)
     
        write(m4parameter,100)gd_l,gd_l*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnl_x+1,gdnl_y+1," 4 100 800 0 0"      
        call write_to_m4(tpos,lev,c_gh,initial_date,"GH",rawpath,
     1  gdnl_x+1,gdnl_y+1,NL,NT,m4parameter)
     
        write(m4parameter,100)gd_l,gd_l*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnl_x+1,gdnl_y+1," 2 -40 40 0 0"      
        call write_to_m4(tpos,lev,c_v,initial_date,"V",rawpath,
     1  gdnl_x+1,gdnl_y+1,NL,NT,m4parameter)
     
        write(m4parameter,100)gd_l,gd_l*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnl_x+1,gdnl_y+1," 2 40 40 0 0"      
        call write_to_m4(tpos,lev,c_u,initial_date,"U",rawpath,
     1  gdnl_x+1,gdnl_y+1,NL,NT,m4parameter)
     
        write(m4parameter,100)gd_l,gd_l*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnl_x+1,gdnl_y+1," 2 -20 20 0 0"      
        call write_to_m4(tpos,lev,c_w,initial_date,"W",rawpath,
     1  gdnl_x+1,gdnl_y+1,NL,NT,m4parameter)
     
        write(m4parameter,100)gd_l,gd_l*(-1),corner_E,corner_W,corner_N,
     1  corner_S,gdnl_x+1,gdnl_y+1," 10 0 100 0 0" 
        call write_to_m4(tpos,lev,c_r,initial_date,"R",rawpath,
     1  gdnl_x+1,gdnl_y+1,NL,NT,m4parameter)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 创建插值后netCDF文件 
        !C返回文件对应的ID，如果存在则覆盖
        write(*,*)'DimNames=',DimNames
        ncfileout=trim(pathname(:(len_trim(pathname)-3)))//"_INS.NC"
        write(*,*)'ncfileout=',ncfileout
        status = nf90_create(trim(ncfileout),NF90_CLOBBER,ncid_out)
        call check(status)
        write(*,*)'ncid_out=',ncid_out
        !C  定义维数，返回一个对应的ID,取100-120E,25-40N,110,40--120,25
        call check(nf90_def_dim(ncid_out,DimNames(1),gdnl_x+1,
     1     longitude_dim))
        call check(nf90_def_dim(ncid_out,DimNames(2),gdnl_y+1,
     1  latitude_dim))
        call check(nf90_def_dim(ncid_out,DimNames(3),NL,
     1  lev_dim))
        call check(nf90_def_dim(ncid_out,DimNames(4),NT,
     1  time_dim))
        write(*,*)'longitude_dim=',longitude_dim
        write(*,*)'latitude_dim=',latitude_dim
        write(*,*)'lev_dim=',lev_dim
        write(*,*)'time_dim=',time_dim
        !C 把上面得到的ID写到一个存放ID的数组里，注意，在fortran中，数组是以列为主存放数据的
        longitude_dims(1)=longitude_dim
        latitude_dims(1)=latitude_dim
        lev_dims(1)=lev_dim
        time_dims(1)=time_dim
        write(*,*)'longitude_dims=',longitude_dims
        write(*,*)'latitude_dims=',latitude_dims
        write(*,*)'lev_dims=',lev_dims
        write(*,*)'time_dims=',time_dims
        q_dims(1)=longitude_dim
        q_dims(2)=latitude_dim
        q_dims(3)=lev_dim
        q_dims(4)=time_dim
        write(*,*)'q_dims=',q_dims
        !C 定义变量，返回一个对应的ID
        call check(nf90_def_var(ncid_out,VarNames(1),NF90_FLOAT,
     1  longitude_dims,longitude_id))
        write(*,*)'longitude_id=',longitude_id
        call check(nf90_def_var(ncid_out,VarNames(2),NF90_FLOAT,
     1    latitude_dims,latitude_id))
        write(*,*)'latitude_id=',latitude_id
        call check(nf90_def_var(ncid_out,VarNames(3),NF90_INT,
     1  lev_dims,lev_id))
        write(*,*)'lev_id=',lev_id
        call check(nf90_def_var(ncid_out,VarNames(4),NF90_INT,
     1  time_dims,time_id))
        write(*,*)'time_id=',time_id        
        call check(nf90_def_var(ncid_out,"q",NF90_FLOAT,q_dims,q_id))
        call check(nf90_def_var(ncid_out,"pv",NF90_FLOAT,q_dims,pv_id))
        call check(nf90_def_var(ncid_out,"d",NF90_FLOAT,q_dims,d_id))
        call check(nf90_def_var(ncid_out,"t",NF90_FLOAT,q_dims,t_id))
        call check(nf90_def_var(ncid_out,"gh",NF90_FLOAT,q_dims,gh_id))
        call check(nf90_def_var(ncid_out,"v",NF90_FLOAT,q_dims,v_id))
        call check(nf90_def_var(ncid_out,"u",NF90_FLOAT,q_dims,u_id))
        call check(nf90_def_var(ncid_out,"w",NF90_FLOAT,q_dims,w_id))
        call check(nf90_def_var(ncid_out,"r",NF90_FLOAT,q_dims,r_id))
        write(*,*)'q_id=',q_id,'pv_id=',pv_id,'d_id=',d_id,'t_id=',t_id
        write(*,*)'gh_id=',gh_id,'v_id=',v_id,'u_id=',u_id,'w_id=',w_id
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
        CALL check(nf90_put_att(ncid_out,lev_id,"units",
     1   "millibars"))
        CALL check(nf90_put_att(ncid_out,time_id,"units",
     1   "hours since 1900-01-01 00:00:0.0")) 
        CALL check(nf90_put_att(ncid_out,q_id,"units","g kg**-1"))
        CALL check(nf90_put_att(ncid_out,q_id,"long_name",
     1    "Specific humidity"))
        CALL check(nf90_put_att(ncid_out,pv_id,"units",
     1  "K m**2 kg**-1 s**-1 e-08"))
        CALL check(nf90_put_att(ncid_out,d_id,"units","s**-1 e-06"))
        CALL check(nf90_put_att(ncid_out,t_id,"units","k-273.15"))
        CALL check(nf90_put_att(ncid_out,gh_id,"units","gpm/9.8"))
        CALL check(nf90_put_att(ncid_out,v_id,"units","m s**-1"))
        CALL check(nf90_put_att(ncid_out,u_id,"units","m s**-1"))
        CALL check(nf90_put_att(ncid_out,w_id,"units","Pa s**-1"))
        CALL check(nf90_put_att(ncid_out,r_id,"units","%"))
        CALL check(nf90_put_att(ncid_out,NF90_GLOBAL,"Conventions",
     1   "zhang weimin"))
        CALL check(nf90_put_att(ncid_out,NF90_GLOBAL,"history",
     1   "by zhang weimin"))
        !C 定义完成，关闭定义模式
        call check(nf90_enddef(ncid_out))
        ! 写入数据
        write(*,*)'czh_xpos=',czh_xpos
        call check(nf90_put_var(ncid_out,longitude_id,czh_xpos))
        write(*,*)'czh_ypos=',czh_ypos
        call check(nf90_put_var(ncid_out,latitude_id,czh_ypos))
        write(*,*)'lev=',lev
        call check(nf90_put_var(ncid_out,lev_id,lev))
        write(*,*)'tpos=',tpos
        call check(nf90_put_var(ncid_out,time_id,tpos))
        write(*,*)'put_att succes'         
        call check(nf90_put_var(ncid_out,q_id,c_q))
        call check(nf90_put_var(ncid_out,pv_id,c_pv))
        call check(nf90_put_var(ncid_out,t_id,c_t))
        call check(nf90_put_var(ncid_out,d_id,c_d))
        call check(nf90_put_var(ncid_out,gh_id,c_gh))
        call check(nf90_put_var(ncid_out,v_id,c_v))
        call check(nf90_put_var(ncid_out,u_id,c_u))
        call check(nf90_put_var(ncid_out,w_id,c_w))
        call check(nf90_put_var(ncid_out,r_id,c_r))
        call check(nf90_close(ncid_out))
        print *, "SUCCESS writing nc file ",ncfileout
        deallocate(DimNames)
        deallocate(VarNames)
        deallocate(czh_xpos)
        deallocate(czh_ypos)
        deallocate(lev)
        deallocate(tpos)
        deallocate(c_q)
        deallocate(c_pv)
        deallocate(c_d)
        deallocate(c_t)
        deallocate(c_gh)
        deallocate(c_v)
        deallocate(c_u)
        deallocate(c_w)
        deallocate(c_r)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
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
        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 给定范围 (120-100)/0.125=160,(40-25)/0.125=120 插值
        subroutine insertdata(xpos_out,ypos_out,m_q,czh_xpos,czh_ypos,
     1  corner_E,corner_N,gd_f,gdnf_x,gdnf_y,gdnl_x,gdnl_y,NL,NT,c_q)
        IMPLICIT NONE
        integer(KIND=4) :: corner_E,corner_N,gdnf_x,gdnf_y,gdnl_x,gdnl_y
        INTEGER :: i0,j0,n0,k0,m,n,NL,NT
        INTEGER :: year_out,month_out,day_out,hour_out
        real :: tw,te,tn,ts,t1,t2,ns,we,x1,x2,x3,x4,y1,y2,gd_f
        real :: xpos_out(gdnf_x+1)
        real :: ypos_out(gdnf_y+1)
        real :: m_q(gdnf_x+1,gdnf_y+1,NL,NT)
        real :: czh_xpos(gdnl_x+1)
        real :: czh_ypos(gdnl_y+1)
        real :: c_q(gdnl_x+1,gdnl_y+1,NL,NT)
!C----------------------------------------------------------------------
120     format(2i4,i2,4f8.3,2f6.1) 
125     format(2i4,f8.3,i4,f8.3,i4,f8.3,i4,f8.3) 
130     format(3f8.1) 
        do k0=1,NT
        do n0=1,NL
        do j0=1,gdnl_y+1
        do i0=1,gdnl_x+1
            m=int((czh_xpos(i0)-corner_E)/gd_f)+1
            n=int((corner_N-czh_ypos(j0))/gd_f)+1
!C            write(*,120)i0,j0,k0,czh_xpos(i0),czh_ypos(j0),xpos_out(m),
!C     1      ypos_out(n)
!C     ,czh_tp(i0,j0,n0,k0),m_q(m,n,n0,k0)
!C            write(*,125)n0,i0,czh_xpos(i0),j0,czh_ypos(j0),
!C     1                       m,xpos_out(m),n,ypos_out(n)
!C        write(*,'(4f8.1)')m_tpdata6h(m,n,n0,k0),m_q(m+1,n,n0,k0),
!C     1      m_tpdata6h(m,n+1,n0,k0),m_tpdata6h(m+1,n+1,n0,k0)
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
         t1=(x1*m_q(m,n,n0,k0))  +(x2*m_q(m+1,n,n0,k0))
         t2=(x3*m_q(m,n+1,n0,k0))+(x4*m_q(m+1,n+1,n0,k0))
            c_q(i0,j0,n0,k0)=(y1*t1) +(y2*t2)
C             if (c_q(i0,j0,n0,k0) < -0.1) then
C                 write(*,*)i0,j0,k0,czh_xpos(i0),czh_ypos(j0),
C      1          c_q(i0,j0,n0,k0)
C             endif
        enddo
        enddo
        enddo
        enddo
        print *, "*** SUCCESS chazhi data!***" 
        end subroutine insertdata
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成micaps 3 类数据文件
        subroutine write_to_m3(xpos_out,ypos_out,tpos,lev,m_q,
     1   initial_date,yaosu,rawpath,NX,NY,NL,NT)
        IMPLICIT NONE
        INTEGER :: i0,j0,n0,k0,NX,NY,NL,NT
        INTEGER :: status,lev(NL)
        INTEGER :: year_out,month_out,day_out,hour_out
        real :: xpos_out(NX),ypos_out(NY),tpos(NT)
        real :: m_q(NX,NY,NL,NT)
        character(len=10) :: initial_date,cur_date
        character(len=*) :: yaosu
        character(len=3) :: mhz,levstr,SNX,SNY
        character(len=100) :: comment,Fname_3,rawpath
140     format(i5.5,f8.3,f7.3,i2,1x,f8.3) 
150     format(i4,3i3.2,i5)
200     format(a8,i1,1x,a10,a)
300     format(i4,3i2.2)
        do k0=1,NT
            call GetDateByHour(tpos(k0),year_out,month_out,
     1      day_out,hour_out)
            write(*,*)tpos(k0),year_out,month_out,day_out,hour_out
            write(cur_date,300)year_out,month_out,day_out,hour_out
            do n0=1,NL   
                write(*,*)"lev=",lev
                write(levstr,'(i3.3)')lev(n0)
            write(comment,200)'diamond ',3,cur_date,"_"//trim(yaosu)
                write(*,*)comment
                write(mhz,'(i3.3)')k0*24-12
                write(SNX,'(i3.3)')NX
                write(SNY,'(i3.3)')NY
                write(*,*)mhz
                Fname_3=trim(rawpath)//"/H_"//trim(yaosu)//"_"//levstr//
     1          "_"//initial_date//"_"//cur_date(5:10)//"_"//SNX//SNY//
     1          "_M3."//mhz
                open(10,file=trim(Fname_3),iostat=status)
                write(10,*)comment(:len_trim(comment))
                write(10,150)year_out,month_out,day_out,hour_out,lev(n0)
                write(10,'(a)')'0'
                write(10,'(a)')'1 2.5 0 '
                write(10,'(i5,i6)')1,NX*NY
                do j0=1,NY
                do i0=1,NX
                    write(10,140)i0*j0,xpos_out(i0),ypos_out(j0),1,
     1              m_q(i0,j0,n0,k0)
                enddo
                enddo
                close(10)
                print *, "SUCCESS writing micaps 3 data "//Fname_3
            enddo
        enddo
        end subroutine write_to_m3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成micaps 4 类数据文件 
        subroutine write_to_m4(tpos,lev,m_yaosu,initial_date,
     1  yaosu,rawpath,NX,NY,NL,NT,m4parameter)
        IMPLICIT NONE
        INTEGER :: i0,j0,n0,k0,status,NX,NY,NL,NT
        INTEGER :: year_out,month_out,day_out,hour_out
        INTEGER :: lev(NL)
        real :: tpos(NT)
        real :: m_yaosu(NX,NY,NL,NT)
        character(len=10) :: initial_date,cur_date
        character(len=*) :: yaosu
        character(len=3) :: mhz,levstr,SNX,SNY
        character(len=100) :: comment,Fname_4,rawpath,m4parameter
300     format(i4,3i2.2)
400     format(a8,i1,1x,a10,a,a10,a,a10,a)
410     format(a8,i1,1x,a10,a,a10,a,a3,a)
500     format(i4.2,3i3.2,i4,i6)
550     format(10f8.1)
        do k0=1,NT
            call GetDateByHour(tpos(k0),year_out,month_out,
     1      day_out,hour_out)
            write(*,*)tpos(k0),year_out,month_out,day_out,hour_out
            write(cur_date,300)year_out,month_out,day_out,hour_out
            write(*,*)'cur_date=',cur_date
            do n0=1,NL
                write(levstr,'(i3.3)')lev(n0)
                write(SNX,'(i3.3)')NX
                write(SNY,'(i3.3)')NY
                write(comment,410)'diamond ',4,initial_date,
     1          "ini",cur_date,"_",levstr,"_"//yaosu
                write(*,*)comment
                write(mhz,'(i3.3)')(k0)*24-12
                write(*,*)mhz
                Fname_4=trim(rawpath)//"/H_"//yaosu//"_"//levstr//
     1          "_"//initial_date//"-"//cur_date(5:10)//"_"//
     1          SNX//SNY//"_M4."//mhz
                write(6,*)"write to file : ",Fname_4
                open(30,file=Fname_4,iostat=status)
                write(30,*)comment(:len_trim(comment))
                write(30,500)year_out,month_out,day_out,hour_out,
     1          (k0)*24,lev(n0)
C 600             format(a27,3i4,2i6,a) 
C                write(m4parameter,600)'0.125 -0.125 108 116 42 33',NX,NY,
C      1 nint((maxval(m_yaosu(:,:,n0,k0))-minval(m_yaosu(:,:,n0,k0)))/10),
C      1          nint(minval(m_yaosu(:,:,n0,k0))), 
C      1          nint(maxval(m_yaosu(:,:,n0,k0)))," 0 0" 
C                 write(*,*)"m4parameter=",m4parameter
                write(30,*)m4parameter(:len_trim(m4parameter)) 
                write(30,550)((m_yaosu(i0,j0,n0,k0),i0=1,NX),j0=1,NY)
                close(30)
            enddo
            print *, "SUCCESS writing micaps 4 data "
        enddo
       end subroutine write_to_m4
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
