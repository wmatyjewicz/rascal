# Copyright (C) 2013 Wojciech Matyjewicz
#
# This file is distributed under the terms of the MIT License.
# See LICENSE file for details.

# Simple wrapper script

if [ $# -ne 1 ]
then
    echo "Syntax: compile.sh <input>"
    exit 1
fi

base=`basename $1 .pas`

./rascal $1 && llc $base.bc && gcc -g $base.s runtime.c
