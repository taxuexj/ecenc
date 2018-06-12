        program main
        use netcdf
        implicit none
        character(len=100) :: rawpath,filename,pathname,gsctl
        character(len=100) :: purename
        character(len=100) :: toutncf,poutncf,coutncf,houtncf,koutncf
        character(len=10) :: initial_date,cur_date,pre_date
        character(len=3) :: mhz,monthen
        integer(KIND=1) :: argnum
        character(len=100) :: ncfileout,filename_4,Fname_3
        character(len=100) :: zhgrd,JCgrd,jcfnctl,zhfnctl
        character(len=12),parameter :: longitude_units="degrees_east"
        character(len=5),parameter :: longitude_unitsname="units"
                integer(KIND=4),parameter :: corner_E=108
        integer(KIND=4),parameter :: corner_W=116
        integer(KIND=4),parameter :: corner_N=42
        integer(KIND=4),parameter :: corner_S=33
        real,parameter            :: gd_l=0.125
        integer(KIND=4)  :: gdnf_x,gdnf_y,gdnl_x,gdnl_y,gdni_x,gdni_y
        logical alive
        integer :: status,xtype,ndims,nvars,DimID,VarID,NX,NY,NN,NT
        integer :: nGlobalAtts,unlimitedDimId,i0,j0,n0,k0,k1,lx,ln
        integer :: tp_FillValue,tp_missing_value,m,n,VarNum
        integer :: ncid_p,ncid_c,ncid_t,ncid_h,NX_h,NY_h,NT_h,NL_h
        integer :: ncid_k,NX_k,NY_k,NT_k
         real   :: temp
        character(LEN=50) :: xname,yname,tname,vname,argstr
        real,allocatable,dimension(:,:,:,:) :: q,pv,d,t,gh,v,u,w,r,p_tp
        real,allocatable,dimension(:)    :: t_xpos,c_xpos,p_xpos,h_xpos
        real,allocatable,dimension(:)    :: t_ypos,c_ypos,p_ypos,h_ypos
        real,allocatable,dimension(:)       :: k_xpos,k_ypos
        real,allocatable,dimension(:,:,:)   :: p_Avg,p_Max,p_Ninety,zh
        real,allocatable,dimension(:,:,:)   :: p_Min,ens,c_tp,t_tp,kx
      integer(KIND=1),allocatable,dimension(:,:,:) :: tq,tw,td,tk,tj,dt
        real :: scale_factor(13),add_offset(13)
        integer :: FillValue(13),missing_value(13)
        character(LEN=50) :: units(13),long_name(13)
        character(LEN=50) :: units_k,long_name_k,reference_date_k
        integer :: reference_time_k
        integer,allocatable,dimension(:) :: t_tpos,c_tpos,p_tpos
        integer,allocatable,dimension(:) :: h_tpos,k_tpos
        integer(KIND=2),allocatable,dimension(:) :: p_npos
        integer(KIND=4),allocatable,dimension(:) :: lev
        integer,allocatable,dimension(:) :: DimNs,VarNs,dimids
        integer,allocatable,dimension(:) :: VarTypes,VarDims,AttTypes
        integer,allocatable,dimension(:) :: AttLens,AttNums,VarIds
        character(LEN=50),allocatable,dimension(:) :: DimNames,VarNames
!C  /* dimension ids */
        integer :: longitude_dim,latitude_dim,time_dim,number_dim
!C  /* variable ids */
        integer :: longitude_id,latitude_id,number_id,time_id,tp_id
!C  /* variable shapes */
        integer :: longitude_dims(1),latitude_dims(1)
        integer :: time_dims(1),number_dims(1)
        integer :: tp_dims(4)
!C   /* attribute vectors */
       character(len=100)    :: fngrd,fnctl,fngs,mytime
       integer :: diamond_type
       character(len=100) :: comment
       integer :: year,month,day,hour,a,num_days0
       integer :: hour_in,year_out,month_out,day_out,hour_out
       logical,external :: isleap
       integer,external :: GetYearByDay,GetMonthByDay
       logical :: leap
       integer :: year_test,day_test,year_day,month_day
       character(len=2) :: numberstr,mnow
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
            initial_date=purename(1:10)
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
        
        toutncf=trim(rawpath)//"/T_"//purename
        toutncf=trim(toutncf)//"_OUT.NC"
        
        coutncf=trim(rawpath)//"/C_"//purename
        coutncf=trim(coutncf)//"_INS.NC"
        
        poutncf=trim(rawpath)//"/P_"//purename
        poutncf=trim(poutncf)//"_INS.NC"
        
        houtncf=trim(rawpath)//"/H_"//purename
        houtncf=trim(houtncf)//"_INS.NC"
        
        koutncf=trim(rawpath)//"/K_"//purename
        koutncf=trim(koutncf)//"_INS.NC"
        
        inquire(file=trim(toutncf),exist=alive)
        if(alive)then
            write(*,*)'toutncf=',trim(toutncf),' exist'
        else
            write(*,*)'toutncf=',trim(toutncf),' not exist'
            go to 999
        endif
        inquire(file=trim(coutncf),exist=alive)
        if(alive)then
            write(*,*)'coutncf=',trim(coutncf),' exist'
        else
            write(*,*)'coutncf=',trim(coutncf),' not exist'
            go to 999
        endif
        inquire(file=trim(poutncf),exist=alive)
        if(alive)then
            write(*,*)'poutncf=',trim(poutncf),' exist'
        else
            write(*,*)'poutncf=',trim(poutncf),' not exist'
            go to 999
        endif
        inquire(file=trim(houtncf),exist=alive)
        if(alive)then
            write(*,*)'houtncf=',trim(houtncf),' exist'
        else
            write(*,*)'houtncf=',trim(houtncf),' not exist'
            go to 999
        endif
        inquire(file=trim(koutncf),exist=alive)
        if(alive)then
            write(*,*)'koutncf=',trim(koutncf),' exist'
        else
            write(*,*)'koutncf=',trim(koutncf),' not exist'
            go to 999
        endif
        write(*,*), nf90_inq_libvers()
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
        !C 打开Perturbed文件
        status = nf90_open(trim(poutncf),nf90_nowrite,ncid_p)
        call check(status)
        !C 获取netcdf文件变量维数
        call check(nf90_inquire(ncid_p,ndims,nvars,nGlobalAtts,
     1  unlimitedDimId))
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
            status=nf90_inquire_dimension(ncid_p,DimID,DimNames(DimID),
     1      DimNs(DimID))
            call check(status)
            write(*,*)DimID,'=',DimNames(DimID),DimNs(DimID)
            write(*,*)"----------------------------------------------"
        enddo
        write(*,*)DimNs
        NX = DimNs(1)
        NY = DimNs(2)
        NN = DimNs(3)
        NT = DimNs(4)
        write(*,*)'NX=',NX,'NY=',NY,'NN=',NN
        write(*,*)'k1=',k1,'DimNs(4)=',DimNs(4)
        allocate(p_xpos(NX))
        allocate(p_ypos(NY))
        allocate(p_npos(NN))
        allocate(p_tpos(NT))
        allocate(p_tp(NX,NY,NN,NT))
        !C get the variables names Types number and IDs
        do VarNum = 1,nvars
            call check(nf90_inquire_variable(ncid_p,VarNum,
     1      VarNames(VarNum),VarTypes(VarNum),VarDims(VarNum),dimids))
            write(*,*)'VarNum = ',VarNum
!C            write(*,*)'VName=',VarNames(VarNum),'Types=',
!C     1      VarTypes(VarNum)
!C            write(*,*)'VarDims=',VarDims(VarNum)
!C            write(*,*)'dimids=',dimids
            call check(nf90_inq_varid(ncid_p,VarNames(VarNum),
     1      VarIds(VarNum)))
!C           write(*,*)'VarIds=',VarIds(VarNum)
        enddo    
        call check(nf90_get_var(ncid_p,VarIds(1),p_xpos))
        CALL check(nf90_get_var(ncid_p,VarIds(2),p_ypos))
        CALL check(nf90_get_var(ncid_p,VarIds(3),p_npos))
        CALL check(nf90_get_var(ncid_p,VarIds(4),p_tpos))
!C        write(*,*)"p_npos=",p_npos
!C        write(*,*)"p_tpos=",p_tpos
        CALL check(nf90_get_var(ncid_p,VarIds(5),p_tp))
        call check(nf90_close(ncid_p))
        write(*,*)'*** SUCCESS getting Perturbed netcdf data ***'
        deallocate(DimNames)
        deallocate(DimNs)
        deallocate(dimIds)
        deallocate(VarNames)
        deallocate(VarTypes)  
        deallocate(VarDims)
        deallocate(VarIds)
        deallocate(AttTypes)
        deallocate(AttLens)
        deallocate(AttNums) 
        deallocate(p_xpos)
        deallocate(p_ypos)
        deallocate(p_tpos)
        deallocate(p_npos)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 打开控制预报netcdf文件
        status = nf90_open(trim(coutncf),nf90_nowrite,ncid_c)
        call check(status)
        !C 获取netcdf文件变量维数
        call check(nf90_inquire(ncid_c,ndims,nvars,nGlobalAtts,
     1  unlimitedDimId))
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
            status=nf90_inquire_dimension(ncid_c,DimID,DimNames(DimID),
     1      DimNs(DimID))
            call check(status)
            write(*,*)DimID,'=',DimNames(DimID),DimNs(DimID)
            write(*,*)"----------------------------------------------"
        enddo
        write(*,*)DimNs
        NX = DimNs(1)
        NY = DimNs(2)
        NT = DimNs(3)
        write(*,*)'NX=',NX,'NY=',NY,'NT=',NT
        allocate(c_xpos(NX))
        allocate(c_ypos(NY))
        allocate(c_tpos(NT))
        allocate(c_tp(NX,NY,NT))
        !C get the variables names Types number and IDs
        do VarNum = 1,nvars
            call check(nf90_inquire_variable(ncid_c,VarNum,
     1      VarNames(VarNum),VarTypes(VarNum),VarDims(VarNum),dimids))
            write(*,*)'VarNum = ',VarNum
!C            write(*,*)'VName=',VarNames(VarNum),'Types=',
!C     1      VarTypes(VarNum)
!C            write(*,*)'VarDims=',VarDims(VarNum)
!C            write(*,*)'dimids=',dimids
            call check(nf90_inq_varid(ncid_c,VarNames(VarNum),
     1      VarIds(VarNum)))
!C            write(*,*)'VarIds=',VarIds(VarNum)
            write(*,*)"----------------------------------------------"
        enddo    
        call check(nf90_get_var(ncid_c,VarIds(1),c_xpos))
        CALL check(nf90_get_var(ncid_c,VarIds(2),c_ypos))
        CALL check(nf90_get_var(ncid_c,VarIds(3),c_tpos))
        write(*,*)"c_tpos=",c_tpos
        CALL check(nf90_get_var(ncid_c,VarIds(4),c_tp))
        call check(nf90_close(ncid_c))
        write(*,*)'*** SUCCESS getting contral netcdf data ***'
        deallocate(DimNames)
        deallocate(DimNs)
        deallocate(dimIds)
        deallocate(VarNames)
        deallocate(VarTypes)  
        deallocate(VarDims)
        deallocate(VarIds)
        deallocate(AttTypes)
        deallocate(AttLens)
        deallocate(AttNums)
        deallocate(c_xpos)
        deallocate(c_ypos)
        deallocate(c_tpos)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
C !C 打开细网格预报netcdf文件
!C        write(*,*)"toutncf=",toutncf
        status = nf90_open(trim(toutncf),nf90_nowrite,ncid_t)
        call check(status)
        call check(nf90_inquire(ncid_t,ndims,nvars,nGlobalAtts,
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
            status=nf90_inquire_dimension(ncid_t,DimID,
     1      DimNames(DimID),DimNs(DimID))
            call check(status)
            write(*,*)DimID,'=',DimNames(DimID),DimNs(DimID)
            write(*,*)"----------------------------------------------"
        enddo
!C        write(*,*)"DimNs=",DimNs
          NX = DimNs(1)
          NY = DimNs(2)
          NT = DimNs(3)
        allocate(t_xpos(NX))
        allocate(t_ypos(NY))
        allocate(t_tpos(NT))
        allocate(t_tp(NX,NY,NT))
        !C get the variables names Types number and IDs
        do VarNum = 1,nvars
            call check(nf90_inquire_variable(ncid_t,VarNum,
     1      VarNames(VarNum),VarTypes(VarNum),VarDims(VarNum),dimids))
            write(*,*)'VarNum=',VarNum
            write(*,*)'VName=',VarNames(VarNum),'Types=',
     1      VarTypes(VarNum)
            write(*,*)'VarDims=',VarDims(VarNum)
            write(*,*)dimids
            call check(nf90_inq_varid(ncid_t,VarNames(VarNum),
     1      VarIds(VarNum)))
            write(*,*)'VarIds=',VarIds(VarNum)
            call check(nf90_inquire_attribute(ncid_t,VarIds(VarNum),
     1      "units",AttTypes(VarNum),AttLens(VarNum),AttNums(VarNum)))
            write(*,*)'AttType=',AttTypes(VarNum)
            write(*,*)'Attlen=',AttLens(VarNum)
            write(*,*)'AttNum=',AttNums(VarNum)
            write(*,*)"----------------------------------------------"
        enddo
        call check(nf90_get_var(ncid_t,VarIds(1),t_xpos))
!C         write(*,*)xpos
        CALL check(nf90_get_var(ncid_t,VarIds(2),t_ypos))
!C         write(*,*)ypos
        CALL check(nf90_get_var(ncid_t,VarIds(3),t_tpos))
        write(*,*)t_tpos
        CALL check(nf90_get_var(ncid_t,VarIds(4),t_tp))
        call check(nf90_close(ncid_t))
        deallocate(DimNames)
        deallocate(DimNs)
        deallocate(dimIds)
        deallocate(VarNames)
        deallocate(VarTypes)  
        deallocate(VarDims)
        deallocate(VarIds)
        deallocate(AttTypes)
        deallocate(AttLens)
        deallocate(AttNums)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!C 打开细网格 高空预报 netcdf文件
         status = nf90_open(trim(houtncf),nf90_nowrite,ncid_h)
         call check(status)
        !C 获取netcdf文件变量维数
         call check(nf90_inquire(ncid_h,ndims,nvars,nGlobalAtts,
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
            status=nf90_inquire_dimension(ncid_h,DimID,DimNames(DimID),
     1      DimNs(DimID))
            call check(status)
            write(*,*)DimID,'=',DimNames(DimID),DimNs(DimID)
            write(*,*)"----------------------------------------------"
        enddo
!C        write(*,*)DimNs
        NX_h=DimNs(1)
        NY_h=DimNs(2)
        NL_h=DimNs(3)
        NT_h=DimNs(4)
        
        allocate(h_xpos(NX_h))
        allocate(h_ypos(NY_h))
        allocate(lev(NL_h))
        allocate(h_tpos(NT_h))
        allocate(q(NX_h,NY_h,NL_h,NT_h))
        allocate(pv(NX_h,NY_h,NL_h,NT_h))
        allocate(d(NX_h,NY_h,NL_h,NT_h))
        allocate(t(NX_h,NY_h,NL_h,NT_h))
        allocate(gh(NX_h,NY_h,NL_h,NT_h))
        allocate(v(NX_h,NY_h,NL_h,NT_h))
        allocate(u(NX_h,NY_h,NL_h,NT_h))
        allocate(w(NX_h,NY_h,NL_h,NT_h))
        allocate(r(NX_h,NY_h,NL_h,NT_h))
        !C get the variables names Types number and IDs
        do VarNum=1,nvars
            call check(nf90_inquire_variable(ncid_h,VarNum,
     1      VarNames(VarNum),VarTypes(VarNum),VarDims(VarNum),dimids))
            write(*,*)'VarNum = ',VarNum
          write(*,*)'VName=',VarNames(VarNum),'Types=',VarTypes(VarNum)
            write(*,*)'VarDims=',VarDims(VarNum)
            write(*,*)'dimids=',dimids
            call check(nf90_inq_varid(ncid_h,VarNames(VarNum),
     1      VarIds(VarNum)))
            write(*,*)'VarIds=',VarIds(VarNum)
            write(*,*)"----------------------------------------------"
        enddo 
        call check(nf90_get_var(ncid_h,VarIds(1),h_xpos))
        CALL check(nf90_get_var(ncid_h,VarIds(2),h_ypos))
        CALL check(nf90_get_var(ncid_h,VarIds(3),lev))
        CALL check(nf90_get_var(ncid_h,VarIds(4),h_tpos))
        write(*,*)"lev=",lev
        write(*,*)"h_tpos=",h_tpos
        do VarNum=5,nvars
            write(*,*)"VarNum=",VarNum
            CALL check(nf90_get_att(ncid_h,VarIds(VarNum),"units",
     1      units(VarNum)))
            write(*,*)'units=',units(VarNum)
            if (VarNames(VarNum) .eq. 'q') then
                CALL check(nf90_get_var(ncid_h,VarIds(VarNum),q))
            elseif (VarNames(VarNum) .eq. 'pv') then
                CALL check(nf90_get_var(ncid_h,VarIds(VarNum),pv))
            elseif (VarNames(VarNum) .eq. 'd') then
                CALL check(nf90_get_var(ncid_h,VarIds(VarNum),d))
            elseif (VarNames(VarNum) .eq. 't') then
                CALL check(nf90_get_var(ncid_h,VarIds(VarNum),t))
            elseif (VarNames(VarNum) .eq. 'gh') then
                CALL check(nf90_get_var(ncid_h,VarIds(VarNum),gh))
            elseif (VarNames(VarNum) .eq. 'v') then
                CALL check(nf90_get_var(ncid_h,VarIds(VarNum),v))
            elseif (VarNames(VarNum) .eq. 'u') then
                CALL check(nf90_get_var(ncid_h,VarIds(VarNum),u))
            elseif (VarNames(VarNum) .eq. 'w') then
                CALL check(nf90_get_var(ncid_h,VarIds(VarNum),w))
            elseif (VarNames(VarNum) .eq. 'r') then
                CALL check(nf90_get_var(ncid_h,VarIds(VarNum),r))
            endif
            write(*,*)"----------------------------------------------"
        enddo
        call check(nf90_close(ncid_h))
        write(*,*)'*** SUCCESS getting high nc data ***'
        deallocate(DimNs)
        deallocate(DimIds)
        deallocate(VarTypes)
        deallocate(VarDims)
        deallocate(VarIds)
        deallocate(AttTypes)
        deallocate(AttLens)
        deallocate(AttNums)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C !C 打开K指数netcdf文件
!C        write(*,*)"koutncf=",koutncf
        status = nf90_open(trim(koutncf),nf90_nowrite,ncid_k)
        call check(status)
        call check(nf90_inquire(ncid_k,ndims,nvars,nGlobalAtts,
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
            status=nf90_inquire_dimension(ncid_k,DimID,
     1      DimNames(DimID),DimNs(DimID))
            call check(status)
            write(*,*)DimID,'=',DimNames(DimID),DimNs(DimID)
            write(*,*)"----------------------------------------------"
        enddo
        write(*,*)"DimNs=",DimNs
          NX_k = DimNs(1)
          NY_k = DimNs(2)
          NT_k = DimNs(3)
        allocate(k_xpos(NX_k))
        allocate(k_ypos(NY_k))
        allocate(k_tpos(NT_k))
        allocate(kx(NX_k,NY_k,NT_k))
        !C get the variables names Types number and IDs
        do VarNum = 1,nvars
            call check(nf90_inquire_variable(ncid_k,VarNum,
     1      VarNames(VarNum),VarTypes(VarNum),VarDims(VarNum),dimids))
            write(*,*)'VarNum=',VarNum
            write(*,*)'VName=',VarNames(VarNum),'Types=',
     1      VarTypes(VarNum)
            write(*,*)'VarDims=',VarDims(VarNum)
            write(*,*)dimids
            call check(nf90_inq_varid(ncid_k,VarNames(VarNum),
     1      VarIds(VarNum)))
            write(*,*)'VarIds=',VarIds(VarNum)
            call check(nf90_inquire_attribute(ncid_k,VarIds(VarNum),
     1      "units",AttTypes(VarNum),AttLens(VarNum),AttNums(VarNum)))
            write(*,*)'AttType=',AttTypes(VarNum)
            write(*,*)'Attlen=',AttLens(VarNum)
            write(*,*)'AttNum=',AttNums(VarNum)
            write(*,*)"----------------------------------------------"
        enddo
        call check(nf90_get_var(ncid_k,VarIds(1),k_xpos))
        write(*,*)"k_xpos=",k_xpos
        CALL check(nf90_get_var(ncid_k,VarIds(2),k_ypos))
        write(*,*)"k_ypos=",k_ypos
        CALL check(nf90_get_var(ncid_k,VarIds(3),k_tpos))
        write(*,*)"k_tpos=",k_tpos
        
        CALL check(nf90_get_att(ncid_k,VarIds(3),"units",
     1  units_k))
        write(*,*)'units_k =',units_k
        
        CALL check(nf90_get_att(ncid_k,VarIds(3),"long_name",
     1  long_name_k))
        write(*,*)'long_name_k =',long_name_k
        
        CALL check(nf90_get_att(ncid_k,VarIds(3),"reference_time",
     1  reference_time_k))
        write(*,*)'reference_time_k =',reference_time_k
        
        CALL check(nf90_get_att(ncid_k,VarIds(3),"reference_date",
     1  reference_date_k))
        write(*,*)'reference_date_k =',reference_date_k
        
        CALL check(nf90_get_var(ncid_k,VarIds(4),kx))
        call check(nf90_close(ncid_k))
        deallocate(DimNames)
        deallocate(DimNs)
        deallocate(dimIds)
        deallocate(VarNames)
        deallocate(VarTypes)  
        deallocate(VarDims)
        deallocate(VarIds)
        deallocate(AttTypes)
        deallocate(AttLens)
        deallocate(AttNums)
		deallocate(k_xpos(NX_k))
        deallocate(k_ypos(NY_k))
        write(*,*)'*** SUCCESS getting Kx nc data ***'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 叠套方法：
        !C get the 条件1 水汽 data
        allocate(tq(NX_h,NY_h,NT_h))
        do k0=1,NT_h
        do j0=1,NY_h
        do i0=1,NX_h 
            if (q(i0,j0,2,k0)>=7 .and. q(i0,j0,3,k0)>=10) then
            tq(i0,j0,k0)=1
            else
            tq(i0,j0,k0)=0
            endif
        enddo
        enddo
        enddo
!C        write(*,*)"tq=",tq
        !C get the 条件2 垂直速度 data t_xpos
        write(*,*)"TJ1 h_tpos=",h_tpos
        call write_to_m3(h_xpos,h_ypos,h_tpos,tq,initial_date,"TJ1",
     1  rawpath,NX_h,NY_h,NT_h)
        call write_to_m4(h_tpos,tq,initial_date,"TJ1",rawpath,
     1  NX_h,NY_h,NT_h)
        allocate(tw(NX_h,NY_h,NT_h))
        do k0=1,NT_h
        do j0=1,NY_h
        do i0=1,NX_h 
            if (w(i0,j0,1,k0)<=-50 .and. w(i0,j0,2,k0)<=-50
     1       .and. w(i0,j0,3,k0)<=-50) then
            tw(i0,j0,k0)=1
            else
            tw(i0,j0,k0)=0
            endif
        enddo
        enddo
        enddo
        call write_to_m3(h_xpos,h_ypos,h_tpos,tw,initial_date,"TJ2",
     1  rawpath,NX_h,NY_h,NT_h)
        call write_to_m4(h_tpos,tw,initial_date,"TJ2",rawpath,
     1  NX_h,NY_h,NT_h)
!C        write(*,*)"tw=",tw
        !C get the 条件3 data
        allocate(td(NX_h,NY_h,NT_h))
        do k0=1,NT_h
        do j0=1,NY_h
        do i0=1,NX_h 
            if (d(i0,j0,2,k0)<=-10 .or. d(i0,j0,3,k0) <=-10) then
            td(i0,j0,k0)=1
            else
            td(i0,j0,k0)=0
            endif
        enddo
        enddo
        enddo
        
        call write_to_m3(h_xpos,h_ypos,h_tpos,td,initial_date,"TJ3",
     1  rawpath,NX_h,NY_h,NT_h)
        call write_to_m4(h_tpos,td,initial_date,"TJ3",rawpath,
     1  NX_h,NY_h,NT_h)
!C        write(*,*)"td=",td
        !C get the 条件4 data
        allocate(tk(NX_k,NY_k,NT_k))
        do k0=1,NT_k
        do j0=1,NY_k
        do i0=1,NX_k 
            if ( kx(i0,j0,k0)>=30 ) then
            tk(i0,j0,k0)=1
            else
            tk(i0,j0,k0)=0
            endif
        enddo
        enddo
        enddo
        call write_to_m3(h_xpos,h_ypos,h_tpos,tk,initial_date,"TJ4",
     1  rawpath,NX_h,NY_h,NT_h)
        call write_to_m4(h_tpos,tk,initial_date,"TJ4",rawpath,
     1  NX_h,NY_h,NT_h)
!C        write(*,*)"tk=",tk
        !C get the 叠套结果 data
        allocate(tj(NX_h,NY_h,NT_h))
        do k0=1,NT_h
        do j0=1,NY_h
        do i0=1,NX_h
       tj(i0,j0,k0)=tq(i0,j0,k0)+td(i0,j0,k0)+tw(i0,j0,k0)+tk(i0,j0,k0)
        enddo
        enddo
        enddo        
!C        write(*,*)"tj=",tj
        allocate(dt(NX_h,NY_h,NT_h))
        do k0=1,NT_h
        do j0=1,NY_h
        do i0=1,NX_h
        if ( tj(i0,j0,k0) >= 3 .and. (tq(i0,j0,k0) .eq. 1)
     1  .and. tw(i0,j0,k0) .eq. 1) then
            dt(i0,j0,k0)=1
            else
            dt(i0,j0,k0)=0
            endif
        enddo
        enddo
        enddo
        call write_to_m3(h_xpos,h_ypos,h_tpos,dt,initial_date,"DT",
     1  rawpath,NX_h,NY_h,NT_h)
        call write_to_m4(h_tpos,dt,initial_date,"DT",rawpath,
     1  NX_h,NY_h,NT_h)
!C        write(*,*)"dt=",dt
        deallocate(h_xpos)
        deallocate(h_ypos)
        deallocate(tq)
        deallocate(td)
        deallocate(tw)
        deallocate(tk)
        deallocate(tj)
        write(*,*)'*** SUCCESS getting tiao jian shuzhu data ***'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C get the 集合预报 计算值 grid preciption data
        allocate(p_Avg(NX,NY,NT))
        allocate(p_Max(NX,NY,NT))
        allocate(p_Min(NX,NY,NT))
        allocate(p_Ninety(NX,NY,NT))
        allocate(ens(NX,NY,NT))
        do k0=1,NT
        do j0=1,NY
        do i0=1,NX
!C            write(*,'(10f6.1)')p_tp(i0,j0,:,k0)
            p_Max(i0,j0,k0)=MaxVal(p_tp(i0,j0,:,k0))
            p_Min(i0,j0,k0)=MinVal(p_tp(i0,j0,:,k0))
!C            write(*,*)MaxLoc(p_tp(i0,j0,:,k0)),MinLoc(p_tp(i0,j0,:,k0))
            p_Avg(i0,j0,k0)=SUM(p_tp(i0,j0,:,k0))/50
!C            write(*,650)"Max=",p_Max(i0,j0,k0),"Min=",p_Min(i0,j0,k0),
!C    1      "Avg=",p_Avg(i0,j0,k0)
            do m=1,NN-1
            do n=m+1,NN
                if (p_tp(i0,j0,m,k0) > p_tp(i0,j0,n,k0) ) then
                    temp=p_tp(i0,j0,m,k0)
                    p_tp(i0,j0,m,k0)=p_tp(i0,j0,n,k0)
                    p_tp(i0,j0,n,k0)=temp
                endif
            enddo
            enddo
!C            write(*,'(10f6.1)')p_tp(i0,j0,:,k0)
C             p_Max(i0,j0,k0)=MaxVal(p_tp(i0,j0,:,k0))
C             p_Min(i0,j0,k0)=MinVal(p_tp(i0,j0,:,k0))
C             write(*,*)MaxLoc(p_tp(i0,j0,:,k0)),MinLoc(p_tp(i0,j0,:,k0))
C             p_Avg(i0,j0,k0)=SUM(p_tp(i0,j0,:,k0))/50
C             write(*,650)"Max=",p_Max(i0,j0,k0),"Min=",p_Min(i0,j0,k0),
C      1      "Avg=",p_Avg(i0,j0,k0)
            p_Ninety(i0,j0,k0)=p_tp(i0,j0,45,k0)
!C            write(*,*)"p_Ninety(i0,j0,k0)=",p_Ninety(i0,j0,k0)
            ens(i0,j0,k0)=t_tp(i0,j0,k0)*0.18+p_Avg(i0,j0,k0)*0.25+
     1                p_Ninety(i0,j0,k0)*0.25+p_Max(i0,j0,k0)*0.16+
     1                    c_tp(i0,j0,k0)*0.16
        enddo
        enddo
        enddo
        write(*,*)'*** SUCCESS getting jisuan data '
        deallocate(c_tp)
        deallocate(p_tp)
        deallocate(t_tp)
        deallocate(p_Avg)
        deallocate(p_Max)
        deallocate(p_Min)
        deallocate(p_Ninety)
        deallocate(q)
        deallocate(pv)
        deallocate(d)
        deallocate(t)
        deallocate(gh)
        deallocate(v)
        deallocate(u)
        deallocate(w)
        deallocate(r)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        allocate(zh(NX,NY,NT))
        do k0=1,NT
        do j0=1,NY
        do i0=1,NX
        if ( ens(i0,j0,k0) >= 25 ) then
!C        if ( ens(i0,j0,k0) >= 25 .and. (dt(i0,j0,k0) .eq. 1)) then
            zh(i0,j0,k0)=ens(i0,j0,k0)
            else
            zh(i0,j0,k0)=0
            endif
        enddo
        enddo
        enddo
!C        write(*,*)"zh=",zh
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成多模式集成micaps 3类数据 
140     format(i5.5,f8.3,f7.3,i2,f5.1) 
150     format(i4,3i3.2,i4)
200     format(a8,i1,1x,a10,a,a10,a,a10,a)
        diamond_type=3
        write(*,*)'t_tpos=',t_tpos
        write(*,*)'k_tpos=',k_tpos
        write(*,*)"reference_time_k=",reference_time_k
        do k0=1,NT
            call GetDateByHour(t_tpos(k0),year_out,month_out,
     1      day_out,hour_out)
            write(*,*)t_tpos(k0),year_out,month_out,day_out,hour_out
            !c reload initial date        
            read(reference_date_k(1:4),*)year
            read(reference_date_k(6:7),*)month
            read(reference_date_k(9:10),*)day
            read(reference_date_k(12:13),*)hour
             call adddatetime(year,month,day,hour,
     1      (k_tpos(k0)-reference_time_k)/3600,"hr")
            write(pre_date,300)year,month,day,hour
            write(*,*)"pre_date=",pre_date
            !c reload initial date
            read(reference_date_k(1:4),*)year
            read(reference_date_k(6:7),*)month
            read(reference_date_k(9:10),*)day
            read(reference_date_k(12:13),*)hour
            call adddatetime(year,month,day,hour,
     1      (k_tpos(k0+1)-reference_time_k)/3600,"hr")
            write(cur_date,300)year,month,day,hour
            write(*,*)"cur_date=",cur_date
            write(comment,600)'diamond ',diamond_type,initial_date,
     1     "_",pre_date,"-",cur_date,"Duo_JC_24Hrain"
            write(*,*)"comment=",comment
            write(*,*)"k_tpos(",k0+1,")=",k_tpos(k0+1)
            write(mhz,'(i3.3)')(k_tpos(k0+1)-reference_time_k)/3600
            write(*,*)"mhz=",mhz
            Fname_3=trim(rawpath)//"/JC_"//pre_date//"-"//
     1      cur_date(5:10)//"_M3."//mhz
             open(80,file=trim(Fname_3),iostat=status)
             write(80,*)comment(:len_trim(comment))
             write(80,150)year,month,day,hour,-2
             write(80,'(a)')'0'
             write(80,'(a)')'1 2.5 0 '
             write(80,'(i5,i6')1,(gdnl_x+1)*(gdnl_y+1)
             do j0=1,gdnl_y+1
             do i0=1,gdnl_x+1
                 write(80,140)i0*j0,t_xpos(i0),t_ypos(j0),1,
     1          ens(i0,j0,k0)
             enddo
             enddo
             close(80)
             write(*,*)"SUCCESS writing insert m3 data ",Fname_3
         enddo 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       
!C 生成综合预报micaps 3 类数据 
        do k0=1,NT
             call GetDateByHour(t_tpos(k0),year_out,month_out,
     1      day_out,hour_out)
             write(*,*)t_tpos(k0),year_out,month_out,day_out,hour_out
            !c reload initial date        
            read(reference_date_k(1:4),*)year
            read(reference_date_k(6:7),*)month
            read(reference_date_k(9:10),*)day
            read(reference_date_k(12:13),*)hour
             call adddatetime(year,month,day,hour,
     1      (k_tpos(k0)-reference_time_k)/3600,"hr")
            write(pre_date,300)year,month,day,hour
            write(*,*)"pre_date=",pre_date
			!c reload initial date
            read(reference_date_k(1:4),*)year
            read(reference_date_k(6:7),*)month
            read(reference_date_k(9:10),*)day
            read(reference_date_k(12:13),*)hour
            call adddatetime(year,month,day,hour,
     1      (k_tpos(k0+1)-reference_time_k)/3600,"hr")
            write(cur_date,300)year,month,day,hour
            write(*,*)"cur_date=",cur_date
            write(comment,600)'diamond ',diamond_type,initial_date,
     1     "_",pre_date,"-",cur_date,"ZhongHe_24Hrain"
            write(*,*)"comment_zh_m3=",comment
            write(mhz,'(i3.3)')(k_tpos(k0+1)-reference_time_k)/3600
            write(*,*)"mhz_zh_m3=",mhz
            Fname_3=trim(rawpath)//"/ZH_"//pre_date//"-"//
     1      cur_date(5:10)//"_M3."//mhz
            write(*,*)"zh_Fname_3=",Fname_3
             open(90,file=trim(Fname_3),iostat=status)
             write(90,*)comment(:len_trim(comment))
             write(90,150)year,month,day,hour,-2
             write(90,'(a)')'0'
             write(90,'(a)')'1 2.5 0 '
             write(90,'(i5,i6')1,(gdnl_x+1)*(gdnl_y+1)
             do j0=1,gdnl_y+1
             do i0=1,gdnl_x+1
                 write(90,140)i0*j0,t_xpos(i0),t_ypos(j0),1,
     1          zh(i0,j0,k0)
             enddo
             enddo
             close(90)
             write(*,*)"SUCCESS writing zh m3 data ",Fname_3
         enddo 
        deallocate(t_xpos)
        deallocate(t_ypos)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成多模式集成 micaps 4 类数据 
300     format(i4,3i2.2)
400     format(a8,i1,1x,a10,a,a10,a,a10,a)
500     format(i4.2,3i3.2,i4,i6)
550     format(10f8.1)
600     format(a8,i1,1x,a10,a,a10,a,a10,a)
650     format(a4,f6.1,a5,f6.1,a5,f6.1)
        diamond_type=4
        do k0=1,NT
             write(*,*)'JC diamond 4 ,k0=',k0
             call GetDateByHour(t_tpos(k0),year_out,month_out,
     1      day_out,hour_out)
             write(*,*)t_tpos(k0),year_out,month_out,day_out,hour_out
             write(cur_date,300)year_out,month_out,day_out,hour_out
             write(*,*)'cur_date=',cur_date
            !c reload initial date        
            read(reference_date_k(1:4),*)year
            read(reference_date_k(6:7),*)month
            read(reference_date_k(9:10),*)day
            read(reference_date_k(12:13),*)hour
             call adddatetime(year,month,day,hour,
     1      (k_tpos(k0)-reference_time_k)/3600,"hr")
            write(pre_date,300)year,month,day,hour
            write(*,*)"pre_date=",pre_date
            write(comment,600)'diamond ',diamond_type,initial_date,
     1      "_",pre_date,"-",cur_date,"ENS24HRain"
            write(*,*)"comment_zh_m4=",comment
             write(mhz,'(i3.3)')(k_tpos(k0+1)-reference_time_k)/3600
             write(*,*)"mhz_zh_m4=",mhz
             filename_4=trim(rawpath)//"/JC_"//
     1      pre_date//"-"//cur_date(5:10)//"_M4."//mhz
             write(6,*)"write to file : ",filename_4
             open(95,file=filename_4,iostat=status)
             write(95,*)comment(:len_trim(comment))
             write(95,500)year,month,day,hour,0,-2
             write(95,'(a)'),"0.125 -0.125 108 116 42 33 65 73 2.5 
     1      0 300 0 0"
             do j0=1,gdnl_y+1
                 write(95,550)(ens(i0,j0,k0),i0=1,gdnl_x+1)
             enddo
             close(95)
             write(*,*)"writted 生成多模式集成 m4 data ",filename_4
         enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
!C 生成综合预报micaps 4 类数据 
        do k0=1,NT
             write(*,*)'ZH diamond 4 ,k0=',k0
             call GetDateByHour(t_tpos(k0),year_out,month_out,
     1      day_out,hour_out)
             write(*,*)t_tpos(k0),year_out,month_out,day_out,hour_out
             write(cur_date,300)year_out,month_out,day_out,hour_out
             write(*,*)'cur_date=',cur_date
            read(reference_date_k(1:4),*)year
            read(reference_date_k(6:7),*)month
            read(reference_date_k(9:10),*)day
            read(reference_date_k(12:13),*)hour
             call adddatetime(year,month,day,hour,
     1      (k_tpos(k0)-reference_time_k)/3600,"hr")
            write(pre_date,300)year,month,day,hour
            write(*,*)"pre_date=",pre_date
            write(comment,600)'diamond ',diamond_type,initial_date,
     1      "_",pre_date,"-",cur_date,"ZH24HRain"
             write(*,*)comment
             write(mhz,'(i3.3)')(k_tpos(k0+1)-reference_time_k)/3600
             write(*,*)"mhz_zh_m4=",mhz
             filename_4=trim(rawpath)//"/ZH_"//
     1      pre_date//"-"//cur_date(5:10)//"_M4."//mhz
             write(*,*)"write to file zh m4: ",filename_4
             open(95,file=filename_4,iostat=status)
             write(95,*)comment(:len_trim(comment))
             write(95,500)year,month,day,hour,0,-2
             write(95,'(a)'),"0.125 -0.125 108 116 42 33 65 73 25 
     1      -100 100 0 0"
             do j0=1,gdnl_y+1
                 write(95,550)(zh(i0,j0,k0),i0=1,gdnl_x+1)
             enddo
             close(95)
             write(*,*)"writted zhonghe m4 data ",filename_4
         enddo	
        deallocate(t_tpos)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
        !C 写多模式集成grads二进制文件
        JCgrd=trim(rawpath)//"/JC_"//initial_date//"_DUO"//".GRD"
        write(*,*)'JCgrd=',JCgrd
        open(100,file=trim(JCgrd),form='binary')
        do k0=1,NT
            do j0=1,gdnl_y+1
                write(100)(ens(i0,j0,k0),i0=1,gdnl_x+1)
!C                 write(*,'(16f5.1)')(ens(i0,j0,k0),i0=1,gdnl_x+1)
            enddo
            print *, "*** SUCCESS bin file ens k0=",k0
        enddo
        
        close(100)
        print *, "*** SUCCESS bin file ",JCgrd
        deallocate(ens)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
        !C 写综合预报grads二进制文件
        ZHgrd=trim(rawpath)//"/ZH_"//initial_date//".GRD"
        write(*,*)'ZHgrd=',ZHgrd
        open(110,file=trim(ZHgrd),form='binary')
        do k0=1,NT
            do j0=1,gdnl_y+1
                write(110)(zh(i0,j0,k0),i0=1,gdnl_x+1)
!C                 write(*,'(16f5.1)')(zh(i0,j0,k0),i0=1,gdnl_x+1)
            enddo
                print *, "*** SUCCESS bin file zh k0",k0
        enddo
        close(110)
        print *, "*** SUCCESS bin file ",ZHgrd
        deallocate(zh)        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
        !C 写多模式集成grads CTL文件
        read(reference_date_k(1:4),*)year
        read(reference_date_k(6:7),*)month
        read(reference_date_k(9:10),*)day
        read(reference_date_k(12:13),*)hour
        write(*,*)year,month,day,hour
        
        call adddatetime(year,month,day,hour,
     1  (k_tpos(2)-reference_time_k)/3600,"hr")
        write(*,*)year,month,day,hour
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
610     format(i2.2,a1,i2.2,a3,i4.4,a4) 
        write(mytime,610)hour,"Z",day,monthen,year," 1dy"
        write(*,*)"mytime=",mytime
        deallocate(k_tpos)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
        JCfnctl=trim(rawpath)//"/JC_"//initial_date//"_DUO"//".CTL"
        open(120,file=trim(JCfnctl))
        write(120,'(a)')"DSET "//trim(JCgrd)
        write(120,'(a)')"undef  -9.99E33"
!C         write(120,'(a)')"options byteswapped"
        write(120,'(a)')"title Integrate Precipitation Of 24h"
        write(120,'(a)')"options yrev"
        write(120,'(a)')"xdef 65 linear 108 0.125"
        write(120,'(a)')"ydef 73 linear 33 0.125"
        write(120,'(a)')"zdef 1 linear 1 1"
        write(120,'(a)')"tdef 7 linear "//trim(mytime)
        write(120,'(a)')"vars 1"
        write(120,'(a)')"TPsfc 0 228,1,0 * surface precipitation [mm]"
        write(120,'(a)')"endvars"
        close(120)
        write(*,*)"*** SUCCESS writted CTL file ",JCfnctl
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
        !C 写综合预报grads CTL文件
        ZHfnctl=trim(rawpath)//"/ZH_"//initial_date//".CTL"
        open(130,file=trim(ZHfnctl))
        write(130,'(a)')"DSET "//trim(ZHgrd)
        write(130,'(a)')"undef  -9.99E33"
!C         write(130,'(a)')"options byteswapped"
        write(130,'(a)')"title Synthetical Precipitation Of 24h"
        write(130,'(a)')"options yrev"
        write(130,'(a)')"xdef 65 linear 108 0.125"
        write(130,'(a)')"ydef 73 linear 33 0.125"
        write(130,'(a)')"zdef 1 linear 1 1"
        write(130,'(a)')"tdef 7 linear "//trim(mytime)
        write(130,'(a)')"vars 1"
        write(130,'(a)')"TPsfc 0 228,1,0 * surface precipitation [mm]"
        write(130,'(a)')"endvars"
        close(130)
        write(*,*)"*** SUCCESS writted CTL file ",ZHfnctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
!C 写多模式集成grads GS文件 
        gsctl=trim(rawpath)//"/JC_"//initial_date//"_DUO"//".GS"
        open(140,file=trim(gsctl))
        write(140,'(a)')"'reinit'"
        write(140,'(a)')"'open "//trim(JCfnctl)//"'" 
        write(140,'(a)')"j=1"
        write(140,'(a)')"while(j<8)"
        write(140,'(a)')"'c'"
        write(140,'(a)')"'set display color white'"        
        write(140,'(a)')"'set map 4 1 10'"
        write(140,'(a)')"'set mpdset cnworld cnriver shanxi shanxi_q'"        
        write(140,'(a)')"'set parea 1 10 1 7.7'"
!C        write(140,'(a)')"'set lon 108 116'"
!C        write(140,'(a)')"'set lat 34 42'"
        write(140,'(a)')"'set csmooth on'"
        write(140,'(a)')"'set t 'j"
        write(140,'(a)')"'q time'"        
        write(140,'(a)')"say j' 'result"
        write(140,'(a)')"in_time1= subwrd(result,3)"        
        write(140,'(a)')"in_time_hour= substr(in_time1,1,2)"
        write(140,'(a)')"in_time_day= substr(in_time1,4,2)"        
        write(140,'(a)')"in_time_mon= substr(in_time1,6,3)"
        write(140,'(a)')"in_time_year= substr(in_time1,9,4)"        
        write(140,'(a)')"year=subwrd(in_time_year,1)"
        write(140,'(a)')"day=subwrd(in_time_day,1)" 
        write(140,'(a)')"hour=subwrd(in_time_hour,1)" 
        write(140,'(a)')" if(in_time_mon='JAN')"
        write(140,'(a)')"  month=01" 
        write(140,'(a)')" endif"
        write(140,'(a)')" if(in_time_mon='FEB')"        
        write(140,'(a)')"  month=02"
        write(140,'(a)')" endif" 
        write(140,'(a)')" if(in_time_mon='MAR')"
        write(140,'(a)')"  month=03" 
        write(140,'(a)')" endif"
        write(140,'(a)')" if(in_time_mon='APR')"        
        write(140,'(a)')"  month=04"
        write(140,'(a)')" endif"        
        write(140,'(a)')" if(in_time_mon='MAY')"
        write(140,'(a)')"  month=05" 
        write(140,'(a)')" endif"
        write(140,'(a)')" if(in_time_mon='JUN')"        
        write(140,'(a)')"  month=06"
        write(140,'(a)')" endif"        
        write(140,'(a)')" if(in_time_mon='JUL')"
        write(140,'(a)')"  month=07" 
        write(140,'(a)')" endif"
        write(140,'(a)')" if(in_time_mon='AUG')"        
        write(140,'(a)')"  month=08"
        write(140,'(a)')" endif"        
        write(140,'(a)')" if(in_time_mon='SEP')"
        write(140,'(a)')"  month=09" 
        write(140,'(a)')" endif"
        write(140,'(a)')" if(in_time_mon='OCT')"        
        write(140,'(a)')"  month=10"
        write(140,'(a)')" endif"        
        write(140,'(a)')" if(in_time_mon='NOV')"
        write(140,'(a)')"  month=11" 
        write(140,'(a)')" endif"
        write(140,'(a)')" if(in_time_mon='DEC')"        
        write(140,'(a)')"  month=12"
        write(140,'(a)')" endif"        
        write(140,'(a)')"say year month day hour"        
        write(140,'(a)')"'set grads off'"
        write(140,'(a)')"'set grid off'"        
        write(140,'(a)')"'set gxout shaded'"
        write(140,'(a)')"'set clevs 1 10 25 50 100 250'"        
        write(140,'(a)')"'set rgb 100 253 253 253'"
        write(140,'(a)')"'set rgb 21  166   242  143'"        
        write(140,'(a)')"'set rgb 22  61   186   61'"
        write(140,'(a)')"'set rgb 23  97  184   255'"        
        write(140,'(a)')"'set rgb 24  0  0   225'"
        write(140,'(a)')"'set rgb 25  250  0   250'"        
        write(140,'(a)')"'set rgb 26  128 0   64'"
        write(140,'(a)')"'set ccols 100 21 22 23 24 25 26 '"        
        write(140,'(a)')"'d TPsfc'"
        write(140,'(a)')"'run cbar.gs'"        
        write(140,'(a)')"'set gxout contour'"
        write(140,'(a)')"'set cint 5'"        
        write(140,'(a)')"'set clab on'"
        write(140,'(a)')"'d TPsfc'"
        write(140,'(a)')"'draw title Integrate Precipitation Of 24h ' 
     1  year month day hour"
        write(140,'(a)')"'draw string 6.0 0.2 "//initial_date//"'"
        write(140,'(a)')"'printim JC_"//initial_date//
     1  "-'month day hour'.gif gif'"       
        write(140,'(a)')"j=j+1"
        write(140,'(a)')"endwhile" 
        write(140,'(a)')"'quit'"
        close(140)
       write(*,*)"*** SUCCESS writing GS file ",gsctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
!C 写综合预报grads GS文件 
        gsctl=trim(rawpath)//"/ZH_"//initial_date//".GS"
        open(150,file=trim(gsctl))
        write(150,'(a)')"'reinit'"
        write(150,'(a)')"'open "//trim(ZHfnctl)//"'" 
        write(150,'(a)')"j=1"
        write(150,'(a)')"while(j<8)"
        write(150,'(a)')"'c'"
        write(150,'(a)')"'set display color white'"        
        write(150,'(a)')"'set map 4 1 10'"
        write(150,'(a)')"'set mpdset cnworld cnriver shanxi shanxi_q'"        
        write(150,'(a)')"'set parea 1 10 1 7.7'"
!C        write(150,'(a)')"'set lon 108 116'"
!C        write(150,'(a)')"'set lat 33 42'"
        write(150,'(a)')"'set csmooth on'"
        write(150,'(a)')"'set t 'j"
        write(150,'(a)')"'q time'"        
        write(150,'(a)')"say j' 'result"
        write(150,'(a)')"in_time1= subwrd(result,3)"        
        write(150,'(a)')"in_time_hour= substr(in_time1,1,2)"
        write(150,'(a)')"in_time_day= substr(in_time1,4,2)"        
        write(150,'(a)')"in_time_mon= substr(in_time1,6,3)"
        write(150,'(a)')"in_time_year= substr(in_time1,9,4)"        
        write(150,'(a)')"year=subwrd(in_time_year,1)"
        write(150,'(a)')"day=subwrd(in_time_day,1)" 
        write(150,'(a)')"hour=subwrd(in_time_hour,1)" 
        write(150,'(a)')" if(in_time_mon='JAN')"
        write(150,'(a)')"  month=01" 
        write(150,'(a)')" endif"
        write(150,'(a)')" if(in_time_mon='FEB')"        
        write(150,'(a)')"  month=02"
        write(150,'(a)')" endif" 
        write(150,'(a)')" if(in_time_mon='MAR')"
        write(150,'(a)')"  month=03" 
        write(150,'(a)')" endif"
        write(150,'(a)')" if(in_time_mon='APR')"        
        write(150,'(a)')"  month=04"
        write(150,'(a)')" endif"        
        write(150,'(a)')" if(in_time_mon='MAY')"
        write(150,'(a)')"  month=05" 
        write(150,'(a)')" endif"
        write(150,'(a)')" if(in_time_mon='JUN')"        
        write(150,'(a)')"  month=06"
        write(150,'(a)')" endif"        
        write(150,'(a)')" if(in_time_mon='JUL')"
        write(150,'(a)')"  month=07" 
        write(150,'(a)')" endif"
        write(150,'(a)')" if(in_time_mon='AUG')"        
        write(150,'(a)')"  month=08"
        write(150,'(a)')" endif"        
        write(150,'(a)')" if(in_time_mon='SEP')"
        write(150,'(a)')"  month=09" 
        write(150,'(a)')" endif"
        write(150,'(a)')" if(in_time_mon='OCT')"        
        write(150,'(a)')"  month=10"
        write(150,'(a)')" endif"        
        write(150,'(a)')" if(in_time_mon='NOV')"
        write(150,'(a)')"  month=11" 
        write(150,'(a)')" endif"
        write(150,'(a)')" if(in_time_mon='DEC')"        
        write(150,'(a)')"  month=12"
        write(150,'(a)')" endif"        
        write(150,'(a)')"say year month day hour"        
        write(150,'(a)')"'set grads off'"
        write(150,'(a)')"'set grid off'"        
        write(150,'(a)')"'set gxout shaded'"
        write(150,'(a)')"'set rgb 100 253 253 253'"
        write(150,'(a)')"'set rgb 23  97  184   255'"        
        write(150,'(a)')"'set rgb 24  0  0   225'"
        write(150,'(a)')"'set rgb 25  250  0   250'"        
        write(150,'(a)')"'set rgb 26  128 0   64'"
        write(150,'(a)')"'set clevs 25 50 100 250'"        
        write(150,'(a)')"'set ccols 100 23 24 25 26 '"        
        write(150,'(a)')"'d TPsfc'"
        write(150,'(a)')"'run cbar.gs'"        
        write(150,'(a)')"'set gxout contour'"
        write(150,'(a)')"'set cint 25'" 
        write(150,'(a)')"'set clab on'"
        write(150,'(a)')"'d TPsfc'"
        write(150,'(a)')"'draw title Synthetical Precipitation Of 24h '
     1  year month day hour"
        write(150,'(a)')"'draw string 6.0 0.2 "//initial_date//"'"
        write(150,'(a)')"'printim ZH_"//initial_date//
     1  "-'month day hour'.gif gif'"        
        write(150,'(a)')"j=j+1"
        write(150,'(a)')"endwhile" 
        write(150,'(a)')"'quit'"
        close(150)
        write(*,*)"*** SUCCESS writing GS file ",gsctl
       	

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成micaps 3 类数据文件
        subroutine write_to_m3(xpos_out,ypos_out,tpos,m_q,
     1   initial_date,yaosu,rawpath,NX,NY,NT)
        IMPLICIT NONE
        INTEGER :: i0,j0,k0,NX,NY,NT,status
        INTEGER :: year_out,month_out,day_out,hour_out
        real :: xpos_out(NX),ypos_out(NY)
        integer(KIND=1) :: m_q(NX,NY,NT)
        integer :: tpos(NT)
        character(len=10) :: initial_date,cur_date
        character(len=*) :: yaosu,rawpath
        character(len=3) :: mhz
        character(len=200) :: comment,Fname_3
140     format(i5.5,f8.3,f7.3,i2,1x,i2)
150     format(i4,3i3.2,i5)
200     format(a8,i1,1x,a10,a)
300     format(i4,3i2.2)
        write(*,*)"tpos=",tpos
        write(*,*)"yaosu=",yaosu
        do k0=1,NT
            call GetDateByHour(tpos(k0),year_out,month_out,
     1      day_out,hour_out)
            write(*,*)tpos(k0),year_out,month_out,day_out,hour_out
            write(cur_date,300)year_out,month_out,day_out,hour_out
            write(comment,200)'diamond ',3,cur_date,"_"//trim(yaosu)
            write(*,*)comment
            write(mhz,'(i3.3)')k0*24
            write(*,*)mhz
            Fname_3=trim(rawpath)//"/"//trim(yaosu)//"_"
     1      //initial_date//"-"//cur_date(5:10)//
     1      "_M3."//mhz
            write(*,*)"Fname_3=",Fname_3
            open(10,file=trim(Fname_3),iostat=status)
            write(10,*)comment(:len_trim(comment))
            write(10,150)year_out,month_out,day_out,hour_out,999
            write(10,'(a)')'0'
            write(10,'(a)')'1 2.5 0 '
            write(10,'(i5,i6')1,NX*NY
            do j0=1,NY
            do i0=1,NX
                write(10,140)i0*j0,xpos_out(i0),ypos_out(j0),1,
     1          m_q(i0,j0,k0)
            enddo
            enddo
            close(10)
            print *, "SUCCESS writing micaps 3 data "//Fname_3
        enddo
        end subroutine write_to_m3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C 生成micaps 4 类数据文件 
        subroutine write_to_m4(tpos,m_yaosu,initial_date,
     1  yaosu,rawpath,NX,NY,NT)
        IMPLICIT NONE
        INTEGER :: i0,j0,k0,status,NX,NY,NT
        INTEGER :: year_out,month_out,day_out,hour_out
        integer :: tpos(NT)
        integer(KIND=1) :: m_yaosu(NX,NY,NT)
        character(len=10) :: initial_date,cur_date
        character(len=*) :: yaosu,rawpath
        character(len=3) :: mhz,SNX,SNY
        character(len=100) :: comment,Fname_4
300     format(i4,3i2.2)
400     format(a8,i1,1x,a10,a,a10,a,a10,a)
410     format(a8,i1,1x,a10,a,a10,a,a3,a)
500     format(i4.2,3i3.2,i4,i6)
550     format(40i2)
        do k0=1,NT
            call GetDateByHour(tpos(k0),year_out,month_out,
     1      day_out,hour_out)
            write(*,*)'tpos(k0)=',k0,tpos(k0)
            write(*,*)'tpos=',k0,year_out,month_out,day_out,hour_out
            write(cur_date,300)year_out,month_out,day_out,hour_out
            write(*,*)'cur_date=',cur_date
            write(SNX,'(i3.3)')NX
            write(SNY,'(i3.3)')NY
            write(comment,410)'diamond ',4,initial_date,
     1          "_",cur_date,"_"//yaosu
            write(*,*)comment
            write(mhz,'(i3.3)')(k0)*24
            write(*,*)mhz
            Fname_4=trim(rawpath)//"/"//yaosu//"_"//initial_date
     1      //"-"//cur_date(5:10)//"_M4."//mhz
            write(6,*)"write to file : ",Fname_4
            open(30,file=Fname_4,iostat=status)
            write(30,*)comment(:len_trim(comment))
            write(30,500)year_out,month_out,day_out,hour_out,999
            write(30,'(a)'),"0.125 -0.125 108 116 42 33 65 73 25 
     1      -100 100 0 0"                
            write(30,550)((m_yaosu(i0,j0,k0),i0=1,NX),j0=1,NY)
            close(30)
            print *, "SUCCESS writing micaps 4 data "
        enddo
       end subroutine write_to_m4
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
