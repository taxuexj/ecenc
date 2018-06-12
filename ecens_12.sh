#!/bin/sh
echo $PATH
export Here_PATH=/home/qxt
export rawdata_PATH=${Here_PATH}/rawdata
export PATH=$PATH:${Here_PATH}/bin
export PGI=/usr/local/pgi
export PATH=/usr/local/grads/bin:/usr/local/hdf5/bin:/usr/local/jpeg/bin:/usr/local/grads/bin:/usr/local/grib_api/bin:/usr/local/bin:/usr/local/lib:/usr/local/lib64:/usr/local/include:/usr/local/pgi/linux86-64/7.1-4/bin:$PATH
export MANPATH=$MANPATH:/usr/local/netcdf40/share/man:/usr/local/pgi/linux86-64/7.1-4/man
export GADDIR=/usr/local/grads/lib
export GASCRP=/usr/local/grads/scripts
export GAUDFT=/usr/local/grads/lib/tables
echo $PATH
################################################################################################
#日期计算
today=`date -d "today"  +%d`
today_mmdd=`date -d "today"  +%m%d`
today_yymmdd=`date -d "today"  +%y%m%d`
today_yyyymmdd=`date -d "today"  +%Y%m%d`
echo 今天是：${today}日${today_yyyymmdd}
yesterday=`date -d "yesterday"  +%d`
yesterday_mmdd=`date -d "yesterday"  +%m%d`
yesterday_yymmdd=`date -d "yesterday"  +%y%m%d`
yesterday_yyyymmdd=`date -d "yesterday"  +%Y%m%d`
echo 昨天是：${yesterday}日${yesterday_yyyymmdd}
thedaybefor=`date -d " -2 day"  +%d`
thedaybefor_mmdd=`date -d " -2 day"  +%m%d`
thedaybefor_yymmdd=`date -d " -2 day"  +%y%m%d`
thedaybefor_yyyymmdd=`date -d " -2 day"  +%Y%m%d`
echo 前天是：${thedaybefor}日${thedaybefor_yyyymmdd}
sc='12'
echo ${sc}
if [ "${sc}" = "18" ] || [ "${sc}" = "12" ]
then
mmdd=${yesterday_mmdd}
yyyymmdd=${yesterday_yyyymmdd}
fi
if [ "${sc}" = "00" ] || [ "${sc}" = "06" ]
then
mmdd=${today_mmdd}
yyyymmdd=${today_yyyymmdd}
fi
echo 模式初始日期：${yyyymmdd} ${mmdd}
################################################################################################
cd ${rawdata_PATH}
rm -f ${rawdata_PATH}/*
cp ${Here_PATH}/bin/cbar.gs ${rawdata_PATH}
cp ${Here_PATH}/bin/rgbset.gs ${rawdata_PATH}
echo "mmdd="${mmdd}
echo "yyyymmdd="${yyyymmdd}
################################################################################################
let shicha=24
let buchang=5
let shichang=${shicha}*${buchang}
echo $shichang
################################################################################################
for k in {1..5}
do
    let ybh=${k}*24
    echo "ybh="$ybh
    yb_yyyymmddhh=`/home/qxt/bin/newdate ${yyyymmdd}${sc} +${ybh}`
    echo "yb_yyyymmddhh="${yb_yyyymmddhh}
          yb_mmddhh=${yb_yyyymmddhh:4:6}
    echo "yb_mmddhh="${yb_mmddhh}
    echo wget -q -nv ftp://getdown:getdown1@172.18.152.9/nafp/ecmf/W_NAFP_C_ECMF_*_P_C1D${mmdd}${sc}00${yb_mmddhh}001*.bz2
         wget -q -nv ftp://getdown:getdown1@172.18.152.9/nafp/ecmf/W_NAFP_C_ECMF_*_P_C1D${mmdd}${sc}00${yb_mmddhh}001*.bz2
    bunzip2 ./W_NAFP_C_ECMF_*_P_C1D${mmdd}${sc}00${yb_mmddhh}001*.bz2
done
################################################################################################
for k in {1..5}
do
    let ybh=${k}*24
    echo "ybh="$ybh
    yb_yyyymmddhh=`/home/qxt/bin/newdate ${yyyymmdd}${sc} +${ybh}`
    echo "yb_yyyymmddhh="${yb_yyyymmddhh}
    yb_mmddhh=${yb_yyyymmddhh:4:6}
    echo "yb_mmddhh="${yb_mmddhh}
	ls W_NAFP_C_ECMF_*_P_C1D${mmdd}${sc}00${yb_mmddhh}001*| while read line
    do
        eval $(echo $line|awk -F"_" '{for ( x = 1; x <= NF; x++ ) { print "arrfold["x"]="$x}}')
        mscs_mmddhh=${arrfold[7]:3:6}
        echo "mscs_mmddhh="${mscs_mmddhh} "current_mmddsc="${yyyymmdd}${sc}
        ybsx_mmddhh=${arrfold[7]:11:6}
        echo "ybsx_mmddhh="${ybsx_mmddhh} "yb_yyyymmddhh="${yb_yyyymmddhh}
        fname=T_${yyyymmdd}${sc}_${ybsx_mmddhh}
        echo wgrib -s $line|grep ":TP:"|wgrib $line -i -grib -append -o T_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
             wgrib -s $line|grep ":TP:"|wgrib $line -i -grib -append -o T_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
        echo wgrib -s $line|grep ":500 mb:"|wgrib $line -i -grib -append -o H_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
             wgrib -s $line|grep ":500 mb:"|wgrib $line -i -grib -append -o H_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
        echo wgrib -s $line|grep ":700 mb:"|wgrib $line -i -grib -append -o H_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
             wgrib -s $line|grep ":700 mb:"|wgrib $line -i -grib -append -o H_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
        echo wgrib -s $line|grep ":850 mb:"|wgrib $line -i -grib -append -o H_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
             wgrib -s $line|grep ":850 mb:"|wgrib $line -i -grib -append -o H_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
    done
done
################################################################################################
echo grib_to_netcdf -o T_${yyyymmdd}${sc}_${shichang}_${shicha}.NC T_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
     grib_to_netcdf -o T_${yyyymmdd}${sc}_${shichang}_${shicha}.NC T_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
echo grib_to_netcdf -o H_${yyyymmdd}${sc}_${shichang}_${shicha}.NC H_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
     grib_to_netcdf -o H_${yyyymmdd}${sc}_${shichang}_${shicha}.NC H_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
################################################################################################
echo ncdump -h T_${yyyymmdd}${sc}_${shichang}_${shicha}.NC 
     ncdump -h T_${yyyymmdd}${sc}_${shichang}_${shicha}.NC > T_${yyyymmdd}${sc}_${shichang}_${shicha}.CDL
echo ncdump -h H_${yyyymmdd}${sc}_${shichang}_${shicha}.NC 
     ncdump -h H_${yyyymmdd}${sc}_${shichang}_${shicha}.NC > H_${yyyymmdd}${sc}_${shichang}_${shicha}.CDL
################################################################################################
#echo ${Here_PATH}/bin/enthin.exe ${rawdata_PATH} T_${yyyymmdd}${sc}_${shichang}_${shicha}.NC
#     ${Here_PATH}/bin/enthin.exe ${rawdata_PATH} T_${yyyymmdd}${sc}_${shichang}_${shicha}.NC > ECTHIN_${yyyymmdd}${sc}.log
#echo ${Here_PATH}/bin/enh.exe ${rawdata_PATH} H_${yyyymmdd}${sc}_${shichang}_${shicha}.NC
#     ${Here_PATH}/bin/enh.exe ${rawdata_PATH} H_${yyyymmdd}${sc}_${shichang}_${shicha}.NC > enh_${yyyymmdd}${sc}.log
echo ${Here_PATH}/bin/enthin.exe ${rawdata_PATH} ${yyyymmdd}${sc}
	 ${Here_PATH}/bin/enthin.exe ${rawdata_PATH} ${yyyymmdd}${sc} > ECTHIN_${yyyymmdd}${sc}.log
echo ${Here_PATH}/bin/enh.exe ${rawdata_PATH} ${yyyymmdd}${sc}
	 ${Here_PATH}/bin/enh.exe ${rawdata_PATH} ${yyyymmdd}${sc} > enh_${yyyymmdd}${sc}.log
################################################################################################
echo grib2ctl.pl -verf T_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1 
     grib2ctl.pl -verf T_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1 > T_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl
echo grib2ctl.pl -verf H_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
     grib2ctl.pl -verf H_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1 > H_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl
echo gribmap -i T_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl
     gribmap -i T_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl
echo gribmap -i H_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl
     gribmap -i H_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl
################################################################################################
cat > T_${yyyymmdd}${sc}_${shichang}_${shicha}.gs << EOF
'reinit'
'open T_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl'
j=1
while(j<5)
'c'
'set display color white'
'set map 4 1 10'
'set mpdset cnworld cnriver '
'set parea 1 10 1 7.7'
'set lon 100 120'
'set lat 25 40'
'set csmooth on' 
*24小时降水量
'set t 'j
'q time'
say j' 'result
in_time1= subwrd(result,3)
in_time_hour= substr(in_time1,1,2)
in_time_day= substr(in_time1,4,2)
in_time_mon= substr(in_time1,6,3)
in_time_year= substr(in_time1,9,4)
year.1=subwrd(in_time_year,1)  
day.1=subwrd(in_time_day,1)  
hour.1=subwrd(in_time_hour,1)
  if(in_time_mon='JAN')
  month.1=01
  endif
  if(in_time_mon='FEB')
  month.1=02
  endif
  if(in_time_mon='MAR')
  month.1=03
  endif
  if(in_time_mon='APR')
  month.1=04
  endif
  if(in_time_mon='MAY')
  month.1=05
  endif
  if(in_time_mon='JUN')
  month.1=06
  endif
  if(in_time_mon='JUL')
  month.1=07
  endif
  if(in_time_mon='AUG')
  month.1=08
  endif
  if(in_time_mon='SEP')
  month.1=09
  endif
  if(in_time_mon='OCT')
  month.1=10
  endif
  if(in_time_mon='NOV')
  month.1=11
  endif
  if(in_time_mon='DEC')
  month.1=12
  endif
say year.1 month.1 day.1 hour.1
j=j+1
'set t 'j
'q time'
say j' 'result
in_time1= subwrd(result,3)
in_time_hour= substr(in_time1,1,2)
in_time_day= substr(in_time1,4,2)
in_time_mon= substr(in_time1,6,3)
in_time_year= substr(in_time1,9,4)
year.2=subwrd(in_time_year,1)  
day.2=subwrd(in_time_day,1)  
hour.2=subwrd(in_time_hour,1)
  if(in_time_mon='JAN')
  month.2=01
  endif
  if(in_time_mon='FEB')
  month.2=02
  endif
  if(in_time_mon='MAR')
  month.2=03
  endif
  if(in_time_mon='APR')
  month.2=04
  endif
  if(in_time_mon='MAY')
  month.2=05
  endif
  if(in_time_mon='JUN')
  month.2=06
  endif
  if(in_time_mon='JUL')
  month.2=07
  endif
  if(in_time_mon='AUG')
  month.2=08
  endif
  if(in_time_mon='SEP')
  month.2=09
  endif
  if(in_time_mon='OCT')
  month.2=10
  endif
  if(in_time_mon='NOV')
  month.2=11
  endif
  if(in_time_mon='DEC')
  month.2=12
  endif
say year.2 month.2 day.2 hour.2
'set grads off'
'set grid off'
'set gxout shaded'
'set clevs 1 10 25 50 100 250'
'set rgb 100 253 253 253'
'set rgb 21  166   242  143'
'set rgb 22  61   186   61'
'set rgb 23  97  184   255'
'set rgb 24  0  0   225'
'set rgb 25  250  0   250'
'set rgb 26  128 0   64'
'set ccols 100 21 22 23 24 25 26 '
'd (TPsfc-TPsfc(t-1))*1000'
'run cbar.gs'
'set gxout contour'
'set clevs 1 10 25 50 100 250'
'set clab on'
'd (TPsfc-TPsfc(t-1))*1000'
'draw title   ECthin rain 'year.1 month.1 day.1 hour.1'-'day.2 hour.2
'draw string 6.0 0.2 ${yyyymmdd}${sc}(CST)'
'printim T_${yyyymmdd}${sc}_'month.2 day.2 hour.2'.gif gif'
endwhile
'quit'
EOF
grads -lbc "T_${yyyymmdd}${sc}_${shichang}_${shicha}.gs"
################################################################################################
current_filename=E_${yyyymmdd}${sc}
declare k ybh
for k in {1..5}
do
    let ybh=${k}*24
    echo "ybh="$ybh
    yb_yyyymmddhh=`/home/qxt/bin/newdate ${yyyymmdd}${sc} +${ybh}`
    echo "yb_yyyymmddhh="${yb_yyyymmddhh}
    yb_mmddhh=${yb_yyyymmddhh:4:6}
    echo "yb_mmddhh="${yb_mmddhh} /nafp/ECMWF/ENS/ACHN/20170413/12
    echo wget -q -nv ftp://getdown:getdown1@172.18.152.9/nafp/ecmf/W_NAFP_C_ECMF_*_P_C3E${mmdd}${sc}00${yb_mmddhh}001-ACHN.bz2
         wget -q -nv ftp://getdown:getdown1@172.18.152.9/nafp/ecmf/W_NAFP_C_ECMF_*_P_C3E${mmdd}${sc}00${yb_mmddhh}001-ACHN.bz2
	echo bunzip2 W_NAFP_C_ECMF_*_P_C3E${mmdd}${sc}00${yb_mmddhh}001-ACHN.bz2
         bunzip2 W_NAFP_C_ECMF_*_P_C3E${mmdd}${sc}00${yb_mmddhh}001-ACHN.bz2
done
################################################################################################
for k in {1..5}
do
    let ybh=${k}*24
    echo "ybh="$ybh
    yb_yyyymmddhh=`/home/qxt/bin/newdate ${yyyymmdd}${sc} +${ybh}`
    echo "yb_yyyymmddhh="${yb_yyyymmddhh}
    yb_mmddhh=${yb_yyyymmddhh:4:6}
    echo "yb_mmddhh="${yb_mmddhh}
	ls W_NAFP_C_ECMF_*_P_C3E${mmdd}${sc}00${yb_mmddhh}001-ACHN| while read line
    do
        eval $(echo $line|awk -F"_" '{for ( x = 1; x <= NF; x++ ) { print "arrfold["x"]="$x}}')
        mscs_mmddhh=${arrfold[7]:3:6}
        echo "mscs_mmddhh="${mscs_mmddhh} "current_mmddsc="${yyyymmdd}${sc}
        ybsx_mmddhh=${arrfold[7]:11:6}
        echo "ybsx_mmddhh="${ybsx_mmddhh} "yb_yyyymmddhh="${yb_yyyymmddhh}
        fname=E_${yyyymmdd}${sc}_${ybsx_mmddhh}
        echo wgrib -s $line|grep ":type=Perturbed"|grep ":TP:"|wgrib $line -i -grib -append -o P_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
             wgrib -s $line|grep ":type=Perturbed"|grep ":TP:"|wgrib $line -i -grib -append -o P_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
		echo wgrib2 -match "pert=0" $line -grib_out ${fname}_pert0.GRIB2
			 wgrib2 -match "pert=0" $line -grib_out ${fname}_pert0.GRIB2
		echo wgrib2 -match "KX" ${fname}_pert0.GRIB2 -append -grib_out K_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB2
			 wgrib2 -match "KX" ${fname}_pert0.GRIB2 -append -grib_out K_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB2
		echo wgrib -s $line|grep "forecast 0:"|grep ":TP:"|wgrib $line -i -grib -append -o C_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
			 wgrib -s $line|grep "forecast 0:"|grep ":TP:"|wgrib $line -i -grib -append -o C_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
        for num in {1..50}
        do
            echo wgrib -s $line|grep "forecast ${num}:"|grep ":TP:"|wgrib $line -i -grib -append -o P${num}_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
                 wgrib -s $line|grep "forecast ${num}:"|grep ":TP:"|wgrib $line -i -grib -append -o P${num}_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
        done 
    done
done
################################################################################################
echo wgrib2 K_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB2 -netcdf K_${yyyymmdd}${sc}_${shichang}_${shicha}.NC
     wgrib2 K_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB2 -netcdf K_${yyyymmdd}${sc}_${shichang}_${shicha}.NC
echo ncdump -h K_${yyyymmdd}${sc}_${shichang}_${shicha}.NC
     ncdump -h K_${yyyymmdd}${sc}_${shichang}_${shicha}.NC > K_${yyyymmdd}${sc}_${shichang}_${shicha}.CDL
################################################################################################
echo grib_to_netcdf -o C_${yyyymmdd}${sc}_${shichang}_${shicha}.NC C_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
     grib_to_netcdf -o C_${yyyymmdd}${sc}_${shichang}_${shicha}.NC C_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
# echo ${Here_PATH}/bin/ecensc.exe ${rawdata_PATH} C_${yyyymmdd}${sc}_${shichang}_${shicha}.NC 
#      ${Here_PATH}/bin/ecensc.exe ${rawdata_PATH} C_${yyyymmdd}${sc}_${shichang}_${shicha}.NC > ecensc_${yyyymmdd}${sc}.log
echo ${Here_PATH}/bin/ecensc.exe ${rawdata_PATH} ${yyyymmdd}${sc} 
	 ${Here_PATH}/bin/ecensc.exe ${rawdata_PATH} ${yyyymmdd}${sc} > ecensc_${yyyymmdd}${sc}.log
################################################################################################
echo grib_to_netcdf -o P_${yyyymmdd}${sc}_${shichang}_${shicha}.NC P_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
     grib_to_netcdf -o P_${yyyymmdd}${sc}_${shichang}_${shicha}.NC P_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
#echo ${Here_PATH}/bin/ecensp.exe ${rawdata_PATH} P_${yyyymmdd}${sc}_${shichang}_${shicha}.NC
#     ${Here_PATH}/bin/ecensp.exe ${rawdata_PATH} P_${yyyymmdd}${sc}_${shichang}_${shicha}.NC > ecensp_${yyyymmdd}${sc}.log
echo ${Here_PATH}/bin/ecensp.exe ${rawdata_PATH} ${yyyymmdd}${sc}
	 ${Here_PATH}/bin/ecensp.exe ${rawdata_PATH} ${yyyymmdd}${sc} > ecensp_${yyyymmdd}${sc}.log
################################################################################################
#echo ${Here_PATH}/bin/kindex.exe ${rawdata_PATH} K_${yyyymmdd}${sc}_${shichang}_${shicha}.NC
#     ${Here_PATH}/bin/kindex.exe ${rawdata_PATH} K_${yyyymmdd}${sc}_${shichang}_${shicha}.NC > kindex_${yyyymmdd}${sc}.log
echo ${Here_PATH}/bin/kindex.exe ${rawdata_PATH} ${yyyymmdd}${sc}
	 ${Here_PATH}/bin/kindex.exe ${rawdata_PATH} ${yyyymmdd}${sc} > kindex_${yyyymmdd}${sc}.log
################################################################################################
#echo ${Here_PATH}/bin/jh.exe ${rawdata_PATH} ${yyyymmdd}${sc}_${shichang}_${shicha}
#     ${Here_PATH}/bin/jh.exe ${rawdata_PATH} ${yyyymmdd}${sc}_${shichang}_${shicha} > jh_${yyyymmdd}${sc}.log
echo ${Here_PATH}/bin/jh.exe ${rawdata_PATH} ${yyyymmdd}${sc}
	 ${Here_PATH}/bin/jh.exe ${rawdata_PATH} ${yyyymmdd}${sc} > jh_${yyyymmdd}${sc}.log
################################################################################################
echo grads -lbc "JC_${yyyymmdd}${sc}_DUO.GS"
	 grads -lbc "JC_${yyyymmdd}${sc}_DUO.GS"
echo grads -lbc "ZH_${yyyymmdd}${sc}.GS"
	 grads -lbc "ZH_${yyyymmdd}${sc}.GS"
################################################################################################
echo grib2ctl.pl -verf C_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1
     grib2ctl.pl -verf C_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1 > C_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl
echo gribmap -i C_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl
     gribmap -i C_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl
cat > C_${yyyymmdd}${sc}_${shichang}_${shicha}.gs << EOF	 
'reinit'
'open C_${yyyymmdd}${sc}_${shichang}_${shicha}.ctl'
j=1
while(j<5)
'c'
'set display color white'
'set map 4 1 10'
'set mpdset cnworld cnriver '
'set parea 1 10 1 7.7'
'set lon 100 120'
'set lat 25 40'
'set csmooth on' 
*24小时降水量
'set t 'j
'q time'
say j' 'result
in_time1= subwrd(result,3)
in_time_hour= substr(in_time1,1,2)
in_time_day= substr(in_time1,4,2)
in_time_mon= substr(in_time1,6,3)
in_time_year= substr(in_time1,9,4)
year.1=subwrd(in_time_year,1)  
day.1=subwrd(in_time_day,1)  
hour.1=subwrd(in_time_hour,1)
  if(in_time_mon='JAN')
  month.1=01
  endif
  if(in_time_mon='FEB')
  month.1=02
  endif
  if(in_time_mon='MAR')
  month.1=03
  endif
  if(in_time_mon='APR')
  month.1=04
  endif
  if(in_time_mon='MAY')
  month.1=05
  endif
  if(in_time_mon='JUN')
  month.1=06
  endif
  if(in_time_mon='JUL')
  month.1=07
  endif
  if(in_time_mon='AUG')
  month.1=08
  endif
  if(in_time_mon='SEP')
  month.1=09
  endif
  if(in_time_mon='OCT')
  month.1=10
  endif
  if(in_time_mon='NOV')
  month.1=11
  endif
  if(in_time_mon='DEC')
  month.1=12
  endif
say year.1 month.1 day.1 hour.1
j=j+1
'set t 'j
'q time'
say j' 'result
in_time1= subwrd(result,3)
in_time_hour= substr(in_time1,1,2)
in_time_day= substr(in_time1,4,2)
in_time_mon= substr(in_time1,6,3)
in_time_year= substr(in_time1,9,4)
year.2=subwrd(in_time_year,1)  
day.2=subwrd(in_time_day,1)  
hour.2=subwrd(in_time_hour,1)
  if(in_time_mon='JAN')
  month.2=01
  endif
  if(in_time_mon='FEB')
  month.2=02
  endif
  if(in_time_mon='MAR')
  month.2=03
  endif
  if(in_time_mon='APR')
  month.2=04
  endif
  if(in_time_mon='MAY')
  month.2=05
  endif
  if(in_time_mon='JUN')
  month.2=06
  endif
  if(in_time_mon='JUL')
  month.2=07
  endif
  if(in_time_mon='AUG')
  month.2=08
  endif
  if(in_time_mon='SEP')
  month.2=09
  endif
  if(in_time_mon='OCT')
  month.2=10
  endif
  if(in_time_mon='NOV')
  month.2=11
  endif
  if(in_time_mon='DEC')
  month.2=12
  endif
say year.2 month.2 day.2 hour.2
'set grads off'
'set grid off'
'set gxout shaded'
'set clevs 1 10 25 50 100 250'
'set rgb 100 253 253 253'
'set rgb 21  166   242  143'
'set rgb 22  61   186   61'
'set rgb 23  97  184   255'
'set rgb 24  0  0   225'
'set rgb 25  250  0   250'
'set rgb 26  128 0   64'
'set ccols 100 21 22 23 24 25 26 '
'd (TPsfc-TPsfc(t-1))*1000'
'run cbar.gs'
'set gxout contour'
'set clevs 1 10 25 50 100 250'
'set clab on'
'd (TPsfc-TPsfc(t-1))*1000'
'draw title   ECENS c0 rain 'year.1 month.1 day.1 hour.1'-'day.2 hour.2
'draw string 6.0 0.2 ${yyyymmdd}${sc}(CST)'
'printim C0_${yyyymmdd}${sc}_'month.2 day.2 hour.2'.gif gif'
endwhile
'quit'
EOF
grads -lbc "C_${yyyymmdd}${sc}_${shichang}_${shicha}.gs"
################################################################################################
for num in {1..50}
do
    grib2ctl.pl -verf P${num}_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1 > P${num}_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1.ctl
    gribmap -i P${num}_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1.ctl
    cat > P${num}_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1.gs << EOF
'reinit'
'open P${num}_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1.ctl'
j=1
while(j<5)
'c'
'set display color white'
'set map 4 1 10'
'set mpdset cnworld cnriver '
'set parea 1 10 1 7.7'
'set lon 100 120'
'set lat 25 40'
'set csmooth on' 
*24小时降水量
'set t 'j
'q time'
say j' 'result
in_time1= subwrd(result,3)
in_time_hour= substr(in_time1,1,2)
in_time_day= substr(in_time1,4,2)
in_time_mon= substr(in_time1,6,3)
in_time_year= substr(in_time1,9,4)
year.1=subwrd(in_time_year,1)  
day.1=subwrd(in_time_day,1)  
hour.1=subwrd(in_time_hour,1)
  if(in_time_mon='JAN')
  month.1=01
  endif
  if(in_time_mon='FEB')
  month.1=02
  endif
  if(in_time_mon='MAR')
  month.1=03
  endif
  if(in_time_mon='APR')
  month.1=04
  endif
  if(in_time_mon='MAY')
  month.1=05
  endif
  if(in_time_mon='JUN')
  month.1=06
  endif
  if(in_time_mon='JUL')
  month.1=07
  endif
  if(in_time_mon='AUG')
  month.1=08
  endif
  if(in_time_mon='SEP')
  month.1=09
  endif
  if(in_time_mon='OCT')
  month.1=10
  endif
  if(in_time_mon='NOV')
  month.1=11
  endif
  if(in_time_mon='DEC')
  month.1=12
  endif
say year.1 month.1 day.1 hour.1
j=j+1
'set t 'j
'q time'
say j' 'result
in_time1= subwrd(result,3)
in_time_hour= substr(in_time1,1,2)
in_time_day= substr(in_time1,4,2)
in_time_mon= substr(in_time1,6,3)
in_time_year= substr(in_time1,9,4)
year.2=subwrd(in_time_year,1)  
day.2=subwrd(in_time_day,1)  
hour.2=subwrd(in_time_hour,1)
  if(in_time_mon='JAN')
  month.2=01
  endif
  if(in_time_mon='FEB')
  month.2=02
  endif
  if(in_time_mon='MAR')
  month.2=03
  endif
  if(in_time_mon='APR')
  month.2=04
  endif
  if(in_time_mon='MAY')
  month.2=05
  endif
  if(in_time_mon='JUN')
  month.2=06
  endif
  if(in_time_mon='JUL')
  month.2=07
  endif
  if(in_time_mon='AUG')
  month.2=08
  endif
  if(in_time_mon='SEP')
  month.2=09
  endif
  if(in_time_mon='OCT')
  month.2=10
  endif
  if(in_time_mon='NOV')
  month.2=11
  endif
  if(in_time_mon='DEC')
  month.2=12
  endif
say year.2 month.2 day.2 hour.2
'set grads off'
'set grid off'
'set gxout shaded'
'set clevs 1 10 25 50 100 250'
'set rgb 100 253 253 253'
'set rgb 21  166   242  143'
'set rgb 22  61   186   61'
'set rgb 23  97  184   255'
'set rgb 24  0  0   225'
'set rgb 25  250  0   250'
'set rgb 26  128 0   64'
'set ccols 100 21 22 23 24 25 26 '
'd (TPsfc-TPsfc(t-1))*1000'
'run cbar.gs'
'set gxout contour'
'set clevs 1 10 25 50 100 250'
'set clab on'
'd (TPsfc-TPsfc(t-1))*1000'
'draw title   ECENS p${num} rain 'year.1 month.1 day.1 hour.1'-'day.2 hour.2
'draw string 6.0 0.2 ${yyyymmdd}${sc}(CST)'
'printim ECENS_P${num}_rain_${yyyymmdd}${sc}_'month.2 day.2 hour.2'.gif gif'
endwhile
'quit'
EOF
    grads -lbc "P${num}_${yyyymmdd}${sc}_${shichang}_${shicha}.GRIB1.gs"
#done
done
################################################################################################
ftpsrc='E_'${sc}'_GIF_PUT'
cat > ${ftpsrc} << EOF
user dqkf2 dq127
bi
prompt
pass
cd /ecenc/JC_4
mput JC_*M4.*
cd /ecenc/JC_3
mput JC_*M3.*
cd /ecenc/JC_gif
mput JC_*.gif
cd /ecenc/ZH_4
mput ZH_*M4.*
cd /ecenc/ZH_3
mput ZH_*M3.*
cd /ecenc/ZH_gif
mput ZH_*.gif
cd /ecenc/C
mput C*M*
mput C0*.gif
cd /ecenc/H
mput H*M*
cd /ecenc/P
mput P*M*
mput ECENS_P*.gif
cd /ecenc/T
mput T*M*
mput T*.gif
cd /ecenc/log
mput *.log
cd /ecenc/TJ
mput TJ*
mput DT*
bye
EOF
ftp -nv 10.69.4.66 < ${ftpsrc}

################################################################################################
#find /home/WORK/data/ecens/ecens${sc} -exec rm -rf {} \;
#test -d /home/WORK/data/ecens/ecens${sc} || mkdir -p /home/WORK/data/ecens/ecens${sc}
#cp  ${rawdata_PATH}/* /home/WORK/data/ecens/ecens${sc}
exit
