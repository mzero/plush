# Copyright 2012-2013 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

extra=""
while true
do
    case $1 in
        --) shift; break;;
        -*) extra="$extra $1"; shift;;
        *) break;;
    esac
done

tests="${*:-tests/*.doctest}"
plush="./dist/build/plush/plush"

test_shell_if_exists() {
    shell=$1
    if which $shell >/dev/null
    then
        $plush --shelltest $extra "$shell" $tests
    fi
}

$plush --doctest $extra $tests
test_shell_if_exists $plush
test_shell_if_exists dash
test_shell_if_exists bash

# sh is often some other shell, detect that here, and act accordingly
if shpath=`which sh`
then
    if shlink=`readlink $shpath`
    then
        realshbase=`basename $shlink`
        case "$realshbase" in
            dash|bash)
                echo sh is $realshbase, not retesting
                ;;

            *)
                echo sh is $realshbase, testing as sh
                test_shell_if_exists sh
                ;;
        esac
    else
        if strings $shpath | grep -q BASH_VERSION
        then
            echo sh appears to be bash, not retesting
        else
            test_shell_if_exists sh
        fi
    fi
else
    echo no sh found on path
fi
