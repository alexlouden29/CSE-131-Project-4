#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
YELL='\033[0;33m' # bold yellow
NC='\033[0m' # Default Color



dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $dir  # This should allow you to call this script from anywhere



set -o pipefail



if [ ! -f ../glc ]; then
  printf "$YELL Rebuilding project...\n $NC"
  (cd .. && make clean && make -j8)
fi


if [ ! -f ../glc ]; then
  printf "$RED Unable to make project\n $NC"
  exit 1
fi


rm *.bc 2> /dev/null






for glsl in *.glsl; do
        fbname=${glsl%%.*}
        bc=${fbname}.bc
        out=${fbname}.out

        printf "${NC}Test case %s: " $fbname
            	 
        # glc
        ../glc < $glsl > $bc 2> /dev/null    	 
        if [ $? -ne 0 ]; then 
                printf "$YELL\nglc exited with error status\n"
                printf "run '../glc < $glsl' for more info\n"
                continue
        fi

        # gli
        ../gli $bc &> /dev/null
        if [ $? -ne 0 ]; then
                printf "$YELL\ngli exited with error status\n"
                printf "run '../gli $bc' for more info\n"
                continue
        fi


        # Test ouput
        diff <(../gli $bc 2> /dev/null) $out
        result=$?


        # Print status
        if (exit $result) ; then
                printf "$GREEN PASS\n"
        else
                printf "$RED FAIL\n"  
        fi 
done


printf $NC

