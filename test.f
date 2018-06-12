!laps to grads
!written by wonder,2009,07,23
        program main
        use netcdf
C
C  def moudle
C
	integer,parameter :: fnum2d=40,fnum3d=13,levpnum=41
	real,parameter  :: miss=-999.9,fillvalue=1.e35
        integer :: i,j,k
	 integer :: NX_L,NY_L,NK_L !laps格点变量数
        integer :: diamond_type
        real :: interval
        character netcdf_lsx_dir*100,systime_dir*100
        character cmn*150,time*9
        character(len=100) :: filename,filename1,micaps_dir,sub_dir
        character(len=100) :: filename_4,filename_11,filename_5

        character(len=50) :: comment
        character fn*13,fn2*14
        character laps_data_root*100,lapstime*100
        real :: wind_min,wind_max,t_min,t_max,p_min,p_max
        integer :: len_path
        real :: corner_latlon(2,4)                      !corner points lat/lon
        integer :: lambrt,merctr,plrstr,irec
        logical alive,alive1

        real,allocatable :: pty(:,:),ptt(:,:),sct(:,:)
        integer element_number
	  real,allocatable :: element(:,:,:,:)
	  real,allocatable :: element_max(:)
	  real,allocatable :: element_min(:)
	  real,allocatable :: element_interval(:)
	  real,allocatable :: element_scale(:)
	  real,allocatable :: element_base(:)
        character(len=5),allocatable :: element_id(:)
        character(len=20),allocatable :: element_name(:)
        character(len=20),allocatable :: element_subdir(:)

	!! <=== wangyan 2006-02-23
        integer(kind=4)    :: ilen,i4time_system,i4time,istatus
        character(len=9)   :: a9_time,a9_time_pkt
        character(len=10)   :: a10_time,a9_to_yr_a10_time
!        def netcdf vars,dims

        integer :: ncid, status, ndims,nvars,nGlobalAtts,unlimDimID
!  def dims         
        integer :: dim_len(nf90_max_var_dims)
        integer :: dimids(nf90_max_var_dims)

!  def vars

        integer :: level_varid,numlevels
        real,allocatable :: level(:)

        integer :: valtime_varid
        real :: valtime
        integer :: year,month,day,hour,minute,second

        
        integer :: nx_varid,ny_varid,varid
        integer(kind=2) :: nx,ny        
        integer :: dx_varid,dy_varid
        real :: dx,dy
!added by weidong,2009,07,21
!variables for get time
	
	integer  myyear,mymonth,myday,myhour,mymin,junday
	character*12  bjtime,bjtime2
        character*100 gradsdir
	character*9   a9timebkt
	character*7   ctemp
	character*3   cjunday
	character*2  cmonth,cday,cmin,chour
!variables for get time define finished!
       integer   :: inum2d,inum3d,argnum !2d 和 3d文件存在数量,参数量
       integer   :: result
       integer   :: in  !循环变量
       real,allocatable :: element2d(:,:),element3d(:,:,:)
!       real,allocatable :: element3d2(:,:,:)
	   character(len=5),dimension(levpnum)   :: levelp
!       character(len=300),dimension(fnum)  :: fnlist !47个文件名列表
       character(len=3),dimension(fnum2d)  :: fnvar2d,varname2d !二维变量
       character(len=3),dimension(fnum3d)  :: fnvar3d,varname3d !三维变量
       character(len=100),dimension(fnum2d)  :: varnameex2d !二维变量的中文含义
	   character(len=100),dimension(fnum3d)  :: varnameex3d !三维变量的中文含义
	   character(len=2)      :: mnow
	   character(len=3)      :: monthen
	   character(len=100)    :: fngrd,fnctl,fngs,fngmf,mytime
	   character(len=100)    :: fntime,dirtime,mydir,datadir,gmfdir
!variables for file if exist
	logical,dimension(fnum2d)     :: ifexist2d
	logical,dimension(fnum3d)     :: ifexist3d
	integer                  :: existnum2d,existnum3d
       data lambrt,merctr,plrstr /1000,2000,3000/
		!fnvar是文件中变量名，先地面，后云分析单层变量,高空21层次之，最后是云分析21层变量
		!varname是需要读取的每个文件中对应的变量名
       data fnvar2d /"lsx","lsx","lsx","lsx","lsx","lsx","lsx","lsx",
     1	 "lsx","lsx","lsx","lsx","lsx","lsx","lsx","lsx","lsx",
     1  "lsx","lsx","lsx","lsx","lsx","lsx","lsx","lsx","lcb","lcb",
     1  "lct","lct","lcv","lil","lmt","lhe","lst","lst","lst","lst",
     1   "lst","lst","lst"/
       data fnvar3d /"lt1","lt1","lh3","lw3","lw3","lw3","vrz",
     1	 "lco","lcp","lty","lty","lwc","lwc"/
       data varname2d /"u","v","p","PP","t","tgd","td","spd",
     1   "vv","vis","rh","tad","th","the","tha","vor","div","mr",
     1   "mrc","mra","hi","fwx","css","ps","msl","lcb","lct",
     1   "ptt","sct","lcv","lil","lmt","lhe",
     1   "li","pbe","nbe","si","tt","k","lcl"/
       data varname3d /"ht","t3","rh3","u3","v3","om","ref","com","lcp",
     1   "pty","cty","lwc","ice"/
       data varnameex2d /"地面2米U风(m/s)","地面2米V风(m/s)",
     1 "参考气压(PA)","1小时变压(PA)","地面气温(K)","地表温度(K)",
     1 "地面露点(K)","地面风速(m/s)","垂直速度(m/s)","水平能见度(M)",
     1 "相对湿度(M)","温度平流(K/S)","地面位温(K)","相当位温(K)",
     1 "位温平流(K/S)","地面涡度(/S)","地面散度(/S)",
     1 "地面混合比(G/KG)","水汽辐合(G/KG/S)","水汽平流(G/KG/S)",
     1 "热力指数(无)","火险指数(无)","科罗拉多强风暴指数(无)",
     1 "地面气压(PA)","海平面气压(PA)","云底高度(M)","云顶高度(M)",
     1 "降水类型(UNDIM)","云分类(UNDIM)","总云量(UNDIM)",
     1 "总液态水含量(M)","回波顶高(M)","螺旋度(M**2/S**2)",
     1 "抬升指数LI(K)","对流有效位能CAPE(J/KG)",
     1 "对流抑制能量CIN(J/KG)","沙氏指数SI(K)","总温度TT(K)",
     1 "K指数(K)","抬升凝结高度LCL(M)"/ 
       data varnameex3d /"位势高度(METERS)","等压面温度(K)",
     1  "等压面相对湿度(PERCENT)","等压面U风(M/S)","等压面V风(M/S)",
     1  "等压面垂直速度(PA/S)","等压面反射率因子(DBZ)",
     1 "等压面云垂直速度(PA/S)","等压面云量(FRACTIONAL)",
     1 "等压面降水类型(NONE)","等压面云类型(NONE)",
     1 "等压面云中液态水含量(KG/M**3)","等压面云中冰晶含量(KG/M**3)"/ 
       data levelp  /"100","125","150","175","200","225","250","275",
     1 "300","325","350","375","400","425","450","475","500","525",
     1 "550","575","600","625","650","675","700","725","750","775",
     1 "800","826","850","875","900","925","950","975","1000","1025",
     1 "1050","1075","1100"/
	 !added by weidong is finished!
	ifexist2d=.false.
	ifexist3d=.false.
	existnum2d=0
	existnum3d=0
!  def attributes

!  other vars(use for test)

!  get laps data root from args
      argnum=iargc()
	if(argnum.eq.1)then
	    write(*,*)"process current data!"
		call getarg(1,laps_data_root)
! get lapstime
!C  get filename
		call GETENV('LAPS_A9TIME',a9_time)
		call s_len(a9_time,ilen)

	if(ilen .eq. 9)then
	write(6,*)' systime (from env) = ',a9_time
	call i4time_fname_lp(a9_time,i4time_sys,istatus)
	call i4time_fname_lp(a9_time1,i4time_sys-3600,istatus)
	else
	call get_systime(i4time_sys,a9_time,istatus)
	if(istatus .ne. 1)go to 999
	write(6,*)' systime = ',a9_time
	endif
	call cv_asc_i4time(a9_time,I4time)
! convert utc to pkt
	i4time=i4time+8*3600
	call make_fnam_lp (i4time, a9_time_pkt, ISTATUS)
	a10_time=a9_to_yr_a10_time(a9_time_pkt,istatus)

	  if(a10_time(1:1).eq.'0') then
		a10_time(4:4)=a10_time(3:3)
		a10_time(3:3)=a10_time(2:2)
		a10_time(2:2)=a10_time(1:1)
		a10_time(1:1)='2'
          endif
          lapstime=a10_time(1:10)//a9_time(8:9)

	!get dim
        NX_L=601
        NY_L=801
        NK_L=41 !laps格点变量数
	write(*,*)"NX_L=301,NY_L=301,NK_L=41;if wrong,modified in src!"
        write(*,*)"NX_L_CMN",NX_L
        write(*,*)"NY_L_CMN",NY_L
        write(*,*)"NK_LAPS",NK_L
        numlevels = NK_L

	else
	write(*,*)"error!no args!"
        write(*,*)"usage: nc2grads.exe laps_data_root"

		write(*,*)
		call exit()
      endif
      

! get dims
!!       call get_grid_dim_xy(NX_L,NY_L,istatus)
!!       print*,'NX_L_CMN ',NX_L
!!       print*,'NY_L_CMN ',NY_L
!!       call get_laps_dimensions(NK_L,istatus)
!!       print*,'NK_LAPS ',NK_L
!!       numlevels = NK_L
!!	   element_number=18
       allocate(level(NK_L))
       allocate(element2d(NX_L,NY_L))        
       allocate(element3d(NX_L,NY_L,NK_L))
!	 allocate(element3d2(NX_L,NY_L,NK_L))
	   !将二维和三维变量设置为缺测
       element2d=miss
	   element3d=miss
!       allocate(element_max(element_number))
!       allocate(element_min(element_number))
!       allocate(element_interval(element_number))
!       allocate(element_scale(element_number))
!       allocate(element_base(element_number))
!       allocate(element_id(element_number))
!       allocate(element_name(element_number))
!       allocate(element_subdir(element_number))
!
!C  get laps croner lat/lon
!C

!added by weidong,code to prepare for fngrd,fnctl,fngs and their contents
	mnow=lapstime(5:6)
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

	fntime=trim(lapstime)
       fntime=trim(fntime)
       dirtime=fntime(1:8)
       cmin=fntime(11:12)
       mydir=trim(laps_data_root)//"/grads/gifpic/"//trim(dirtime)//"/"
       datadir=trim(laps_data_root)//"/grads/data/"//trim(dirtime)//"/"
       gmfdir=trim(laps_data_root)//"/grads/gmfpic/"//trim(dirtime)//"/"
       fngrd=trim(datadir)//"laps"//trim(lapstime)//".grd"
       fngs=trim(laps_data_root)//"/grads/data/laps.gs"
       fnctl=trim(laps_data_root)//"/grads/data/laps.ctl"
       fngmf=trim(gmfdir)//"laps"//trim(lapstime)//".gmf"
       mytime=fntime(9:10)//":"//fntime(11:12)//"Z"//
     1 fntime(7:8)//monthen//fntime(1:4)//" 5mn"
       result=system("mkdir -p "//trim(laps_data_root)//
     1 "/grads/gifpic/"//fntime(1:8))
       result=system("mkdir -p "//trim(laps_data_root)//
     1 "/grads/gmfpic/"//fntime(1:8))
       result=system("mkdir -p "//trim(laps_data_root)//
     1 "/grads/data/"//fntime(1:8))
       open(9,file=trim(fngrd),form='binary')

!开始二维变量文件循环，读取二维变量并写入fngrd
        write(*,*)"2d var make begin!!!"
	inum2d=0
	allocate(element(NX_L,NY_L,1,1))  

	do in=1,fnum2d
!C  open file for input and output
!C
        filename=trim(laps_data_root)//"/lapsprd/"//trim(fnvar2d(in))
     1   //"/"//a9_time//"."//trim(fnvar2d(in))
!        write(*,*)'filename',trim(filename)
        inquire(file=trim(filename),exist=alive)
        if(alive)then
          ifexist2d(in)=.true.
	existnum2d=existnum2d+1
        write(*,*)'filename',trim(filename)
	 inum2d=inum2d+1
            status = nf90_open(trim(filename),nf90_nowrite,ncid)
!C  read netcdf 
!C  get time info
            status = nf90_inq_varid(ncid,"valtime",valtime_varid)
            status = nf90_get_var(ncid,valtime_varid,valtime)
!C  get other info(may be useless)
            status = nf90_inq_varid(ncid,"Nx",Nx_varid)
            status = nf90_get_var(ncid,Nx_varid,Nx)
            status = nf90_inq_varid(ncid,"Ny",Ny_varid)
            status = nf90_get_var(ncid,Ny_varid,Ny)

            status = nf90_inq_varid(ncid,"Dx",Dx_varid)
            status = nf90_get_var(ncid,Dx_varid,Dx)
            status = nf90_inq_varid(ncid,"Dy",Dy_varid)
            status = nf90_get_var(ncid,Dy_varid,Dy)

            status = nf90_inq_varid(ncid,trim(varname2d(in)),varid)
            status = nf90_get_var(ncid,varid,element)
         
	do i0=1,NX_L
	do j0=1,NY_L
          if(element(i0,j0,1,1).lt.fillvalue)then 
             element2d(i0,j0)=element(i0,j0,1,1)
          else
             element2d(i0,j0)=miss
          endif
	enddo
	enddo
		    write(9)element2d
	   else
              element2d=miss
              write(9)element2d
	   endif

      enddo   !in
      deallocate(element)
      deallocate(element2d)

!开始三维变量文件循环，读取三维变量并写入fngrd
        write(*,*)"3d var make begin!!!"
	inum3d=0
      allocate(element(NX_L,NY_L,NK_L,1)) 
	do in=1,fnum3d
!C  open file for input and output
!C
        filename=trim(laps_data_root)//"/lapsprd/"//trim(fnvar3d(in))
     1  //"/"//a9_time//"."//trim(fnvar3d(in))
!        write(*,*)'filename',trim(filename)
        inquire(file=trim(filename),exist=alive)
        if(alive)then
	   existnum3d=existnum3d+1
         ifexist3d(in)=.true.
         write(*,*)'filename',trim(filename)
         inum3d=inum3d+1
            status = nf90_open(trim(filename),nf90_nowrite,ncid)
!C  read netcdf 
!C  get level info
            status = nf90_inq_varid(ncid,"level",level_varid)
            status = nf90_get_var(ncid,level_varid,level)
!C  get time info
            status = nf90_inq_varid(ncid,"valtime",valtime_varid)
            status = nf90_get_var(ncid,valtime_varid,valtime)
!C  get other info(may be useless)
            status = nf90_inq_varid(ncid,"Nx",Nx_varid)
            status = nf90_get_var(ncid,Nx_varid,Nx)
            status = nf90_inq_varid(ncid,"Ny",Ny_varid)
            status = nf90_get_var(ncid,Ny_varid,Ny)

            status = nf90_inq_varid(ncid,"Dx",Dx_varid)
            status = nf90_get_var(ncid,Dx_varid,Dx)
            status = nf90_inq_varid(ncid,"Dy",Dy_varid)
            status = nf90_get_var(ncid,Dy_varid,Dy)

            status = nf90_inq_varid(ncid,trim(varname3d(in)),varid)
            status = nf90_get_var(ncid,varid,element)
	!垂直维数反转，从低层到高层
	    do k0=1,NK_L
	    do j0=1,NY_L
	    do i0=1,NX_L
               if(element(i0,j0,NK_L-k0+1,1).lt.fillvalue)then
	          element3d(i0,j0,k0)=element(i0,j0,NK_L-k0+1,1)
               else
                  element3d(i0,j0,k0)=miss
               endif
	    enddo
	    enddo
	    enddo
		    write(9)element3d
           else
              element3d=miss
              write(9)element3d
	   endif
       enddo   !in
      deallocate(element)
      deallocate(element3d)
       close(9)

       write(*,*)"2d variables exist num: ",inum2d
	write(*,*)"3d variables exist num: ",inum3d	
	if((inum2d+inum3d).eq.0)then
	call exit()
	else

       open(100,file=trim(fnctl))
       write(100,'(a)')"DSET "//trim(fngrd)
       write(100,'(a)')"undef  -999.9"
       write(100,'(a)')"options byteswapped"
       write(100,'(a)')"title stmasrunresult of 1 hour"
       write(100,'(a)')"pdef 301 301 lcc 38 113 150.5 150.5 30
     1  60 113 3000 3000"
!C 301   = #pts in x 
!C 301   = #pts in y 
!C lcc   = Lambert-Conformal 
!C 38    = lat of ref point 
! C 113    = lon of ref point (E is positive, W is negative) 
! C 150.5  = i of ref point 
! C 150.5  = j of ref point 
! C 30    = S true lat 
! C 60    = N true lat 
! C 113    = standard lon 
! C 3000 = dx in M 
! C 3000 = dy in M 
       write(100,'(a)')"xdef 301 linear 107.3642 0.027615"
       write(100,'(a)')"ydef 301 linear 33.73996 0.032941"
       write(100,'(a)')"zdef 41 levels 1100 1075 1050 1025 1000 975
     1    950 925 900 875 850 825 800 775 750 725 700 675 650 625
     1    600 575 550 525 500 475 450 425 400 375 350 325 300 275
     1    250 225 200 175 150 125 100"
       write(100,'(a)')"tdef  1  linear "//trim(mytime)
       write(100,'(a,1x,i3)')"vars ",fnum2d+fnum3d
       do i=1,fnum2d
        write(100,'(a)')trim(varname2d(i))//
     1 " 0 99 "//trim(varnameex2d(i))
       enddo
	   do i=1,fnum3d
           write(100,'(a,i2,a)')trim(varname3d(i))
     1 //" ",levpnum," 99 "//trim(varnameex3d(i))
	   enddo
       write(100,'(a)')"endvars"
       close(100)
	   
!写出gs文件
       open(8,file=trim(fngs))
       write(8,'(a)')"'reinit'"
       write(8,'(a)')"'open "//trim(fnctl)//"'"
!       write(8,'(a)')"'set parea 1 7.5 1 10'"
       write(8,'(a)')"'enable print "//trim(fngmf)//"'"
       write(8,'(a)')"'set mpdset cnworld cnriver shanxi_q shanxi'"
       write(8,'(a)')"'set map 4 1 7'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set lat 33.6 42.0'"
       write(8,'(a)')"'set clab forced'"
       write(8,'(a)')"'set xlopts 1 6 0.18'"
       write(8,'(a)')"'set ylopts 1 6 0.18'"
       write(8,'(a)')"*****************地面*****************"
	if(ifexist2d(5))then
!       write(8,'(a)')"'set grid off'"
!       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(t-273.15)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS SURFACE T(C) "//
     1	 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS01"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(6))then
       write(8,'(a)')"***********地表温度等51-56*************"
	 write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(tgd-273.15)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS Ground T(C) "//
     1	 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS51"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(22))then
	 write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(fwx)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS Fire Danger(K) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS52"//trim(fntime)//
     1 ".gif x600 y800 white'"
	 write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(21))then
	 write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
 !      write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(hi)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS Heat Index "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS53"//trim(fntime)//
     1 ".gif x600 y800 white'"
	 write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(11).and.ifexist2d(1).and.ifexist2d(2))then
	 write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
  !     write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(rh)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout vector'"
       write(8,'(a)')"'d skip(u,4);skip(v,4)'"
       write(8,'(a)')"'draw title LAPS RH(%) & Wind "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS54"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(12).and.ifexist2d(1).and.ifexist2d(2))then
	 write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
   !    write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(tad)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout vector'"
       write(8,'(a)')"'d skip(u,4);skip(v,4)'"
       write(8,'(a)')"'draw title LAPS Surface T Adv(k/s) & Wind "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS55"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(19).and.ifexist2d(1).and.ifexist2d(2))then
		 write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
    !   write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(mrc)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout vector'"
       write(8,'(a)')"'d skip(u,4);skip(v,4)'"
       write(8,'(a)')"'draw title LAPS moisture convergence & Wind "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS56"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(7))then
!       write(8,'(a)')"**********地表温度等后续6变量结束********"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(td-273.15)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS SURFACE Td(C) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS02"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(7).and.ifexist2d(5))then
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(t-td)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS SURFACE T-Td(C) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS03"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(25).and.ifexist2d(1).and.ifexist2d(2))then
	 write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
1       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(msl/100.0)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout vector'"
       write(8,'(a)')"'d skip(u,4);skip(v,4)'"
       write(8,'(a)')"'draw title LAPS MSL(hPa) & Wind "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS04"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(9).and.ifexist2d(1).and.ifexist2d(2))then
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(vv)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout vector'"
       write(8,'(a)')"'d skip(u,4);skip(v,4)'"
       write(8,'(a)')"'draw title LAPS Vertical Velocity(m/s) & Wind "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS05"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(16))then
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set cmin 0'"
       write(8,'(a)')"'d smth9(vor*1000)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS SURFACE VOR(10-3 s-1) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS06"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(17))then
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
	write(8,'(a)')"'set cmax 0'"
       write(8,'(a)')"'d smth9(div*1000)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS SURFACE DIV(10-3 s-1) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS07"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(10))then
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
 !      write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set clevs 0.5 1 2 3 4 5 6 7 8 9 10 20'"
       write(8,'(a)')"'set ccols 29 25 23 21 32 35 38 42 45 48 11 4 0'"
       write(8,'(a)')"'d smth9(vis/1000.0)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS SURFACE VIS(km) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS08"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(23))then
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(css)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS Strong Storm Index "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS09"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
!	 if(cmin.eq."00".or.cmin.eq."15".or.cmin.eq."30".or.
!     1 cmin.eq."45")then
	if(ifexist2d(33))then
      write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(lhe)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS helicity(m2/s2) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS10"//trim(fntime)//
     1 ".gif x600 y800 white'"
	 write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(34))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(li)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS LI(K) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS11"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(39))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(k)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS K(K) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS12"//trim(fntime)//
     1 ".gif x600 y800 white'"
	 write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(35))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(pbe)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS CAPE(J/Kg) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS13"//trim(fntime)//
     1 ".gif x600 y800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(36))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(nbe)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS CIN(J/Kg) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS14"//trim(fntime)//
     1 ".gif x600 y800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(38))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(tt)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS TT(K) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS15"//trim(fntime)//
     1 ".gif x600 y800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(37))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(si)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS SI(K) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS16"//trim(fntime)//
     1 ".gif x600 y800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(40))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(lcl/1000.0)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS LCL(km) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS17"//trim(fntime)//
     1 ".gif x600 800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(29))then
       write(8,'(a)')"********************云**********************"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"*'set cmin 0.1'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set cint 1'"
       write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set clevs 1 2 3 4 5 6 7 8 9 10'"
       write(8,'(a)')"'set ccols 0 49 46 43 33 36 39 21 24 27 29'"
       write(8,'(a)')"'d smth9(sct)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set string 1 l 6'"
       write(8,'(a)')"'draw string 1.5 0.45 0:No'"
       write(8,'(a)')"'draw string 2.8 0.45 1:St'"
       write(8,'(a)')"'draw string 4.3 0.45 2:Sc'"
       write(8,'(a)')"'draw string 5.6 0.45 3:Cu'"
       write(8,'(a)')"'draw string 6.9 0.45 4:Ns'"
       write(8,'(a)')"'draw string 8.2 0.45 5:Ac'"
       write(8,'(a)')"'draw string 1.5 0.15 6:As'"
       write(8,'(a)')"'draw string 2.8 0.15 7:Cs'"
       write(8,'(a)')"'draw string 4.3 0.15 8:Ci'"
       write(8,'(a)')"'draw string 5.6 0.15 9:Cc'"
       write(8,'(a)')"'draw string 6.9 0.15 10:Cb'"
       write(8,'(a)')"'draw title LAPS CLOUD Type "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS18"//trim(fntime)//
     1 ".gif x600 y800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(28))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set cmin 0.1'"
       write(8,'(a)')"'set cint 1'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set clevs 1 2 3 4 5 6 7'"
       write(8,'(a)')"'set ccols 0 49 44 34 39 22 25 29'"
       write(8,'(a)')"'d smth9(ptt)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set string 1 l 6'"
       write(8,'(a)')"'draw string 1.5 0.45 0:No Precip'"
       write(8,'(a)')"'draw string 3.8 0.45 1:Rain'"
       write(8,'(a)')"'draw string 5.5 0.45 2:Snow'"
       write(8,'(a)')"'draw string 7.3 0.45 3:Freezing Rain'"
       write(8,'(a)')"'draw string 1.5 0.15 4:Ice Pellets'"
       write(8,'(a)')"'draw string 3.8 0.15 5:Hail'"
       write(8,'(a)')"'draw string 5.5 0.15 6:Drizzle'"
       write(8,'(a)')"'draw string 7.3 0.15 7:Freezing Drizzle'"
       write(8,'(a)')"'draw title LAPS Rain Type "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS19"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(26))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(lcb/1000.0)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS CLOUD Base(km) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS20"//trim(fntime)//
     1 ".gif x600 y800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(27))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(lct/1000.0)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS CLOUD Top(km) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS21"//trim(fntime)//
     1 ".gif x600 y800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(30))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(lcv*100)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS Total CLOUD Cover(%) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS22"//trim(fntime)//
     1 ".gif x600 y800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
       endif
	if(ifexist2d(32))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(lmt/1000.0)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS Echo Top(km) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS23"//trim(fntime)//
     1 ".gif x600 y800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist2d(31))then
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(lil*1000)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS Total Lquid Water(mm) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS24"//trim(fntime)//
     1 ".gif x600 y800 white'"
	write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	 endif
	if(ifexist3d(7))then
       write(8,'(a)')"********** draw ref500****"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 500'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set cmin 0'"
       write(8,'(a)')"'d smth9(ref)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS 500hPa DBZ "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS25"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	write(8,'(a)')"********** draw ref700****"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
!       write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 700'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set cmin 0'"
       write(8,'(a)')"'d smth9(ref)'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS 700hPa DBZ "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS26"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
       write(8,'(a)')"********** draw ref850****"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 850'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set cmin 0'"
       write(8,'(a)')"'d smth9(ref)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS 850hPa DBZ "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS27"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
       write(8,'(a)')"********** draw ref900****"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 900'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set cmin 0'"
       write(8,'(a)')"'d smth9(ref)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS 900hPa DBZ "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS28"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
       write(8,'(a)')"********** draw ref950****"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 950'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set cmin 0'"
       write(8,'(a)')"'d smth9(ref)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS 950hPa DBZ "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS29"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(1).and.ifexist3d(3))then
       write(8,'(a)')"********************高空********************"
       write(8,'(a)')"********** draw hgt500+rh850****"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set lev 850'"
       write(8,'(a)')"'set gxout shaded'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set clevs 50 60 70 80 90'"
       write(8,'(a)')"'set ccols 0 31 33 35 37 39'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(rh3)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout contour'"
       write(8,'(a)')"'set lev 500'"
       write(8,'(a)')"'set cmin 50'"
       write(8,'(a)')"'set cthick 6'"
       write(8,'(a)')"'set clskip 2'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(ht/10)'"
       write(8,'(a)')"'draw title LAPS 500hPa ght & 850hPa RH(%) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS30"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(3).and.ifexist3d(4).and.ifexist3d(5))then
	 write(8,'(a)')"**** Draw 850hPa RH and 500hPa wind*******"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set lev 850'"
       write(8,'(a)')"'set gxout shaded'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set clevs 50 60 70 80 90'"
       write(8,'(a)')"'set ccols 0 31 33 35 37 39'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(rh3)'"
	   write(8,'(a)')"'run cbar.gs'"
       write(8,'(a)')"'set lev 500'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout barb'"
       write(8,'(a)')"'set ccolor 4'"
       write(8,'(a)')"'set cthick 6'"
       write(8,'(a)')"'d skip(u3*2.413,6);skip(v3*2.413,6)'"
       write(8,'(a)')"'draw title LAPS 500hPa WIND & 850hPa RH(%) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS31"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(2).and.ifexist3d(4).and.ifexist3d(5))then
       write(8,'(a)')"********** Draw 850hPa T and 850hPa wind**"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set lev 850'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set cint 2'"
       write(8,'(a)')"'set clskip 2'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(t3-273.15)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout barb'"
       write(8,'(a)')"'set cthick 6'"
       write(8,'(a)')"'d skip(u3*2.413,6);skip(v3*2.413,6)'"
       write(8,'(a)')"'draw title LAPS 850hPa T(C) & WIND "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS32"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(3).and.ifexist3d(4).and.ifexist3d(5))then

       write(8,'(a)')"*** Draw 700hPa RH and 700hPa wind*******"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set lev 700'"
       write(8,'(a)')"'set gxout shaded'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set clevs 50 60 70 80 90'"
       write(8,'(a)')"'set ccols 0 31 33 35 37 39'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(rh3)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout barb'"
       write(8,'(a)')"'set ccolor 4'"
       write(8,'(a)')"'set cthick 6'"
       write(8,'(a)')"'d skip(u3*2.413,6);skip(v3*2.413,6)'"
       write(8,'(a)')"'draw title LAPS 700hPa Rh(%) & Wind "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS33"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	 write(8,'(a)')"**** Draw 750hPa RH and 750hPa wind******"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set lev 750'"
       write(8,'(a)')"'set gxout shaded'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set clevs 50 60 70 80 90'"
       write(8,'(a)')"'set ccols 0 31 33 35 37 39'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(rh3)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout barb'"
       write(8,'(a)')"'set ccolor 4'"
       write(8,'(a)')"'set cthick 6'"
       write(8,'(a)')"'d skip(u3*2.413,6);skip(v3*2.413,6)'"
       write(8,'(a)')"'draw title LAPS 750hPa Rh(%) & Wind "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS34"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
       write(8,'(a)')"**** Draw 850hPa RH and 850hPa wind******"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set lev 850'"
       write(8,'(a)')"'set gxout shaded'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set clevs 50 60 70 80 90'"
       write(8,'(a)')"'set ccols 0 31 33 35 37 39'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(rh3)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout barb'"
       write(8,'(a)')"'set ccolor 4'"
       write(8,'(a)')"'set cthick 6'"
       write(8,'(a)')"'d skip(u3*2.413,6);skip(v3*2.413,6)'"
       write(8,'(a)')"'draw title LAPS 850hPa Rh(%) & Wind "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS35"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"

	 write(8,'(a)')"**** Draw 900hPa RH and 900hPa wind******"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set lev 900'"
       write(8,'(a)')"'set gxout shaded'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set clevs 50 60 70 80 90'"
       write(8,'(a)')"'set ccols 0 31 33 35 37 39'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(rh3)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout barb'"
       write(8,'(a)')"'set ccolor 4'"
       write(8,'(a)')"'set cthick 6'"
       write(8,'(a)')"'d skip(u3*2.413,6);skip(v3*2.413,6)'"
       write(8,'(a)')"'draw title LAPS 900hPa Rh(%) & Wind "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS36"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
       write(8,'(a)')"**** Draw 950hPa RH and 950hPa wind******"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set lev 950'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set clevs 50 60 70 80 90'"
       write(8,'(a)')"'set ccols 0 31 33 35 37 39'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d smth9(rh3)'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'set gxout barb'"
       write(8,'(a)')"'set ccolor 4'"
       write(8,'(a)')"'set cthick 6'"
       write(8,'(a)')"'d skip(u3*2.413,6);skip(v3*2.413,6)'"
       write(8,'(a)')"'draw title LAPS 950hPa Rh(%) & Wind "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS37"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(6).and.ifexist3d(4).and.ifexist3d(5))then

       write(8,'(a)')"**** 700 Div & Pressure vertical velocity ****"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 700'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set cmax -0.05'"
       write(8,'(a)')"'d smth9(om)'"
	   write(8,'(a)')"'run cbar.gs'"
       write(8,'(a)')"'set gxout contour'"
       write(8,'(a)')"'set cmax 0'"
       write(8,'(a)')"'d hdivg(u3,v3)*1.e3'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS 700hPa DIV & Omega(Pa/s) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS38"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"

       write(8,'(a)')"**** 850 Div & Pressure vertical velocity ****"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 850'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set cmax -0.05'"
       write(8,'(a)')"'d smth9(om)'"
	   write(8,'(a)')"'run cbar.gs'"
       write(8,'(a)')"'set gxout contour'"
       write(8,'(a)')"'set cmax 0'"
       write(8,'(a)')"'d hdivg(u3,v3)*1.e3'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS 850hPa DIV & Omega(Pa/s) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS39"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
       write(8,'(a)')"**** 900 Div & Pressure vertical velocity ****"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 900'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set cmax -0.05'"
       write(8,'(a)')"'d smth9(om)'"
       write(8,'(a)')"'set gxout contour'"
       write(8,'(a)')"'set cmax 0'"
       write(8,'(a)')"'d hdivg(u3,v3)*1.e3'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS 900hPa DIV & Omega(Pa/s) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS40"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
       write(8,'(a)')"**** 950 Div & Pressure vertical velocity ****"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 950'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set cmax -0.05'"
       write(8,'(a)')"'d smth9(om)'"
	   write(8,'(a)')"'run cbar.gs'"
       write(8,'(a)')"'set gxout contour'"
       write(8,'(a)')"'set cmax 0'"
       write(8,'(a)')"'d hdivg(u3,v3)*1.e3'"
       !write(8,'(a)')"'run cbarn 1 1 9.10 4.25'"
       write(8,'(a)')"'draw title LAPS 950hPa DIV & Omega(Pa/s) "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS41"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(4).and.ifexist3d(5))then
       write(8,'(a)')"******** Vertical plot 40N div ********"
       write(8,'(a)')"'set lev 1000 100 50'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set lat 33.6 42'"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
       !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'define ddive=hdivg(u3,v3)*1.e3'"
       write(8,'(a)')"'set lev 1000 100'"
       write(8,'(a)')"'set lat 33.6 42'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set zlog on'"
	 write(8,'(a)')"'set cmax 0'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d ddive'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 10.11 4.25'"
       write(8,'(a)')"'draw title LAPS DIV along 40N "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS42"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(6))then
       write(8,'(a)')"******** Vertical plot 40N omega ********"
	 write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
	 !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 1000 100'"
       write(8,'(a)')"'set lat 33.6 42'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set zlog on'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
	   write(8,'(a)')"'set cmax 0'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d om'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 10.11 4.25'"
       write(8,'(a)')"'draw title LAPS Omega(Pa/s) along 40N "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS43"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(7))then
       write(8,'(a)')"******** Vertical plot 40N ref ********"
	 write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
	 !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 1000 100'"
       write(8,'(a)')"'set lat 33.6 42'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set zlog on'"
	 write(8,'(a)')"'set cmin 0'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d ref'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 10.11 4.25'"
       write(8,'(a)')"'draw title LAPS Dbz along 40N "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS44"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(8))then
       write(8,'(a)')"******** Vertical plot 40N cloud omega ********"
	 write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
	 !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 1000 100'"
       write(8,'(a)')"'set lat 33.6 42'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set zlog on'"
       write(8,'(a)')"'set cmax 0'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d com'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 10.11 4.25'"
       write(8,'(a)')"'draw title LAPS Cloud Omega(Pa/s) along 40N "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS45"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(9))then
       write(8,'(a)')"******** Vertical plot 40N lcp ********"
	 write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
	 !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 1000 100'"
       write(8,'(a)')"'set lat 33.6 42'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set zlog on'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d lcp*100.0'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 10.11 4.25'"
       write(8,'(a)')"'draw title LAPS Cloud Cover(%) along 40N "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS46"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(10))then
       write(8,'(a)')"******** Vertical plot 40N pty ********"
	 write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
	 !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 1000 100'"
       write(8,'(a)')"'set lat 33.6 422'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set zlog on'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set csmooth on'"
	 write(8,'(a)')"'set cint 1'"
       write(8,'(a)')"'d pty'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 10.11 4.25'"
       write(8,'(a)')"'set string 1 l 6'"
       write(8,'(a)')"'draw string 1.5 0.45 0:No Precip'"
       write(8,'(a)')"'draw string 3.8 0.45 1:Rain'"
       write(8,'(a)')"'draw string 5.5 0.45 2:Snow'"
       write(8,'(a)')"'draw string 7.3 0.45 3:Freezing Rain'"
       write(8,'(a)')"'draw string 1.5 0.15 4:Ice Pellets'"
       write(8,'(a)')"'draw string 3.8 0.15 5:Hail'"
       write(8,'(a)')"'draw string 5.5 0.15 6:Drizzle'"
       write(8,'(a)')"'draw string 7.3 0.15 7:Freezing Drizzle'"
       write(8,'(a)')"'draw title LAPS Prec Type along 40N "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS47"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(11))then
       write(8,'(a)')"******** Vertical plot 40N cty ********"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
!      write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 1000 100'"
       write(8,'(a)')"'set lat 33.6 42'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set zlog on'"
       write(8,'(a)')"'set gxout shaded'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set cint 1'"
       write(8,'(a)')"'d cty'"
       write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 10.11 4.25'"
       write(8,'(a)')"'set string 1 l 6'"
       write(8,'(a)')"'draw string 1.5 0.45 0:No'"
       write(8,'(a)')"'draw string 2.8 0.45 1:St'"
       write(8,'(a)')"'draw string 4.3 0.45 2:Sc'"
       write(8,'(a)')"'draw string 5.6 0.45 3:Cu'"
       write(8,'(a)')"'draw string 6.9 0.45 4:Ns'"
       write(8,'(a)')"'draw string 8.2 0.45 5:Ac'"
       write(8,'(a)')"'draw string 1.5 0.15 6:As'"
       write(8,'(a)')"'draw string 2.8 0.15 7:Cs'"
       write(8,'(a)')"'draw string 4.3 0.15 8:Ci'"
       write(8,'(a)')"'draw string 5.6 0.15 9:Cc'"
       write(8,'(a)')"'draw string 6.9 0.15 10:Cb'"
       write(8,'(a)')"'draw title LAPS Cloud Type along 40N "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS48"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
	if(ifexist3d(12))then
       write(8,'(a)')"******** Vertical plot 40N lwc ********"
	 write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
	 !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 1000 100'"
       write(8,'(a)')"'set lat 33.6 42'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set zlog on'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d lwc*1000'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 10.11 4.25'"
      write(8,'(a)')"'draw title LAPS Cloud Lquid Water(g/m3) "//
     1	 "along40N "//trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS49"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
		if(ifexist3d(13))then
       write(8,'(a)')"******** Vertical plot 40N ice ********"
       write(8,'(a)')"'set grads off'"
       write(8,'(a)')"'set grid off'"
	 !write(8,'(a)')"'set xlint 0.4'"
       write(8,'(a)')"'set lev 1000 100'"
       write(8,'(a)')"'set lat 33.6 42'"
       write(8,'(a)')"'set lon 108 115.6'"
       write(8,'(a)')"'set zlog on'"
       write(8,'(a)')"'set gxout shaded'"
	   write(8,'(a)')"'run rgbset.gs'"
       write(8,'(a)')"*'set black -0.1 0.1'"
       write(8,'(a)')"'set csmooth on'"
       write(8,'(a)')"'d ice*1000.0'"
	   write(8,'(a)')"'run cbar.gs'"
       !write(8,'(a)')"'run cbarn 1 1 10.11 4.25'"
       write(8,'(a)')"'draw title LAPS Cloud Ice(g/m3) along 40N "//
     1 trim(fntime)//"'"
       write(8,'(a)')"'printim "//trim(mydir)//"LAPS50"//trim(fntime)//
     1 ".gif x600 y800 white'"
       write(8,'(a)')"'print'"
       write(8,'(a)')"'clear'"
	endif
       write(8,'(a)')"'disable print'"
       write(8,'(a)')"'set parea off'"
       write(8,'(a)')"reinit"
       write(8,'(a)')"quit"
       close(8)
	endif
	  deallocate(level)
       call exit()
999     print*,'Error:cannot get systime'
       call exit()
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	FUNCTION num_days0( year ) 
      ! Compute the number of days in February for the given year
      IMPLICIT NONE
      INTEGER :: year
      INTEGER :: num_days0
      num_days0 = 28 ! By default, February has 28 days ...
      IF (MOD(year,4).eq.0) THEN  
        num_days0 = 29  ! But every four years, it has 29 days ...
        IF (MOD(year,100).eq.0) THEN
           num_days0 = 28  ! Except every 100 years, when it has 28 days ...
            IF (MOD(year,400).eq.0) THEN
               num_days0 = 29  ! Except every 400 years, when it has 29 days.
            END IF
         END IF
      END IF
	  END FUNCTION num_days0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 SUBROUTINE split_date_char0 ( date0 , century_year , month , day 
     1	 , hour , minute)
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
	subroutine day_of_year0(century_year, month, day,day2)
!get day2 of year by gived year,month,day
!	IMPLICIT NONE
	INTEGER                     :: century_year, month, day,day2
	INTEGER                     :: m
	INTEGER :: days_in_month(12)
      DATA days_in_month /31,28,31,30,31,30,31, 31, 30, 31, 30, 31/
	 days_in_month(2) = num_days0(century_year)           
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
	END
