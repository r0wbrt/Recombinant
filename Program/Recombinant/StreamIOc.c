/**
Copyright 2017 Robert Christian Taylor

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

#define _GNU_SOURCE
#include <fcntl.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <errno.h>


#define MAXBUFFERSIZE 16384

static loff_t
copy_file_range(int fd_in, loff_t *off_in, int fd_out,
                loff_t *off_out, size_t len, unsigned int flags)
{
    return syscall(__NR_copy_file_range, fd_in, off_in, fd_out,
                    off_out, len, flags);
}


int spliceHandler(int fd_in, int fdInPipe, int fd_out, int fdOutPipe, int size)
{
    loff_t fdInOffset, fdOutOffset;
    loff_t * off_in;
    loff_t * off_out;
    size_t remainingBytes = size;
    int ret;
    
    if(fdInPipe) {
        off_in = NULL;
    } else {
        off_in = &fdInOffset;
        fdInOffset = lseek(fd_in, 0, SEEK_CUR);
    }
    
    if(fdOutPipe) {
        off_out = NULL;
    } else {
        off_out = &fdOutOffset;
        fdOutOffset = lseek(fd_out, 0, SEEK_CUR);
    }
    
    while(remainingBytes > 0) {
        
        size_t len;
        if(remainingBytes >  MAXBUFFERSIZE) {
            len = MAXBUFFERSIZE;
            remainingBytes -= MAXBUFFERSIZE;
        } else {
            len = remainingBytes;
            remainingBytes = 0;
        }
        
        ret = splice(fd_in, off_in, fd_out, off_out, len, SPLICE_F_MOVE);
        if(ret == 0) {
            return -1;
        } else if(ret == -1) {
            if(errno == EINVAL) {
                return 0;
            } else  {
                return -1;
            }
        } else {
            len -= ret;
            remainingBytes += len;
        }
    }
    
    if(off_in != NULL) {
        lseek(fd_in, *off_in, SEEK_SET);
    }

    if(off_out != NULL) {
        lseek(fd_out, *off_out, SEEK_SET);
    }
    
    return 1;
}


int copyRange(int fd_in, int fd_out, int length) 
{
    loff_t off_in, off_out, len, ret, inTotalLen;
    
    int retValue = 1;
    
    off_in = lseek(fd_in, 0, SEEK_CUR);
    off_out = lseek(fd_out, 0, SEEK_CUR);
    inTotalLen = lseek(fd_in, 0, SEEK_END);
    
    //Don't do half copies of the data. Only copy 
    //if the source has enough data to accomodate the request.
    //If calling code wants different behavior, it can truncate 
    //length its self.
    if(off_in + length > inTotalLen) {
        return -1;
    }
    
    len = length;
    
    
    do {
        ret = copy_file_range(fd_in, &off_in, fd_out, &off_out, len, 0);
        if(ret == -1) {
            if(errno == EXDEV) {
                retValue = 0;
            } else {
                retValue = -1;
            }
            
            goto end;
        }
        
        len -= ret;
        
    } while (len > 0);


end:
    lseek(fd_in, off_in, SEEK_SET);
    lseek(fd_out, off_out, SEEK_SET);
    
    return retValue;
}
