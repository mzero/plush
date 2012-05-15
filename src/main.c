/*
Copyright 2012 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include <fcntl.h>
#include <unistd.h>
#include "HsFFI.h"

// #include "Plush/Main_stub.h"
// Should include this, but in a clean cabal build, cabal builds C files before
// Haskell ones, so this include file doesn't exist yet. So, the one needed
// declaration is just copied here:
extern void plushMain();

extern void __stginit_Main(void);

void reserveFileDescriptors() {
    int nullFd = open("/dev/null", O_RDWR);
    int f;
    for (f = 3; f <= 9; f++) {
        dup2(nullFd, f);
        fcntl(f, F_SETFD, FD_CLOEXEC);
    }
    if (nullFd < 3 || nullFd > 9)
        close(nullFd);
}

void releaseFileDescriptors() {
    int f;
    for (f = 3; f <= 9; f++)
        close(f);
}

int main(int argc, char *argv[]) {
    reserveFileDescriptors();
    hs_init(&argc, &argv);
    //releaseFileDescriptors(); // not sure yet why this doesn't work
    hs_add_root(__stginit_Main);
    plushMain();
    hs_exit();
    return 0;
}
