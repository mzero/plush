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

./dist/build/plush/plush --doctest tests/*.doctest
./dist/build/plush/plush --shelltest sh tests/*.doctest
./dist/build/plush/plush --shelltest dash tests/*.doctest
./dist/build/plush/plush --shelltest bash tests/*.doctest
plush_datadir=`pwd` \
  ./dist/build/plush/plush --shelltest ./dist/build/plush/plush tests/*.doctest
