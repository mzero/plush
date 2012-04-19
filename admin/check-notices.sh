# Copyright 2012 Google Inc. All Rights Reserved.
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

NOTICES=`dirname $0`/notices
if [ \! -d $NOTICES ]
then
	echo 'could not find the notices directory'
	exit 1
fi

for f in `git ls-tree -r --name-only HEAD`
do
  case $f in
    static/js/libs/*/*)         echo '#' third-party file: $f ;;
    static/templates/*.html)    echo '#' exempted file: $f ;;
    tests/simple.sh)            echo '#' exempted file: $f ;;
	*.*)
	  c=${f/*./${NOTICES}\/notice.}
	  if [ -f $f -a -e $c ]
	  then
	    if grep -q -i Copyright $f
	    then
	      echo '#' already copyrighted: $f
	    else
	      echo cat $c $f '>' tmp ';' mv tmp $f
	    fi
	  else
	    echo '#' not a source file: $f
	  fi
	  ;;
  esac
done
