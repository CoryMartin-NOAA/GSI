#!/bin/bash
#SBATCH --ntasks-per-node 8
#SBATCH -A da-cpu
#SBATCH -J run_gsi_observer
#SBATCH -t 00:30:00
#SBATCH -q debug
#SBATCH --nodes 30

##### things you should change/confirm
GSIDir=/scratch1/NCEPDEV/da/Cory.R.Martin/GSI/GSI_forJEDI_v16/
adate=2020102300
format='nemsio' # nemsio or netcdf
gfsv16='false' # false or true
guessroot=/scratch1/NCEPDEV/rstprod/com/gfs/prod
jcap=766
jcap_b=1534
levs=64
dump='gdas'
iodaconvbuild=/scratch1/NCEPDEV/da/Cory.R.Martin/JEDI/ioda-converters/build
JEDImodule=/scratch1/NCEPDEV/da/Cory.R.Martin/JEDI/env_jedi_rh
##### things it is not necessary to change but you can
RootWork=/scratch1/NCEPDEV/stmp2/$LOGNAME/GSI_forJEDI/
obsdir=/scratch1/NCEPDEV/global/glopara/dump/
rstprod='true'
cleanup='true'

##### do not modify below here ####
MyDir=$GSIDir/ush/JEDI
Ya=`echo $adate | cut -c1-4`
Ma=`echo $adate | cut -c5-6`
Da=`echo $adate | cut -c7-8`
Ha=`echo $adate | cut -c9-10`

mkdir -p $RootWork

# create YAML for GSI observer
rm -rf $RootWork/GSI_observer_$adate.yaml
cat > $RootWork/GSI_observer_$adate.yaml << EOF
background:
  format: $format
  gfsv16: $gfsv16
  guessdir: $guessroot
  jcap: $jcap
  jcap_b: $jcap_b
  levs: $levs
env:
  launcher: srun --export=ALL
  nthreads: 1
observations:
  dump: $dump
  obsdir: $obsdir
  restricted: $rstprod
observer:
  cleanup: $cleanup
  gsidir: $GSIDir
  outputdir: $RootWork/${adate}/GSI_out
  workdir: $RootWork/${adate}/GSI_work
time:
  cycle: '$Ha'
  day: '$Da'
  month: '$Ma'
  year: '$Ya'
EOF

# run GSI observer
$MyDir/run_gsi_observer.sh $RootWork/GSI_observer_$adate.yaml || exit 1

# create YAML for IODA converters
rm -rf $RootWork/GSI_iodaconv_$adate.yaml
cat > $RootWork/GSI_iodaconv_$adate.yaml << EOF
data:
  gsiindir: $RootWork/${adate}/GSI_out
  iodaoutdir: $RootWork/${adate}/output
  iodaworkdir: $RootWork/${adate}/iodaconv_work
env:
  launcher: srun --export=ALL
  modulefile: $JEDImodule
  nthreads: 1
iodaconv:
  iodaconvbin: $iodaconvbuild/bin/proc_gsi_ncdiag.py
time:
  cycle: '$Ha'
  day: '$Da'
  month: '$Ma'
  year: '$Ya'
EOF

# run IODA converters
$MyDir/run_gsi_iodaconv.sh $RootWork/GSI_iodaconv_$adate.yaml || exit 1
