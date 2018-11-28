#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>

#define SIZE 1024

char * read_file_into_string (char filename[])
{
    struct stat sb;
    if(-1 == stat(filename, &sb)) {
        perror("stat file");
        printf("function: read_file_into_string\n");
        printf("filename: [%s]\n", filename);
        return NULL;
    }
    /* printf("filesize: %ld\n", sb.st_size); */

    FILE *fp=fopen(filename, "r");
    if(NULL == fp) {
        perror("fopen file");
        printf("function: read_file_into_string\n");
        printf("filename: [%s]\n", filename);
        return NULL;
    }
    /* printf("file handle: %p\n", fp); */

    char *str = malloc(sb.st_size+1);
    if(NULL == str) {
        perror("malloc");
        printf("function: read_file_into_string\n");
        printf("filename: [%s]\n", filename);
        return NULL;
    }
    /* printf("malloc pntr: %p\n", str); */

    int cnt = fread(str, 1, sb.st_size, fp);
    if(sb.st_size != cnt) {
        perror("fread");
        printf("function: read_file_into_string\n");
        printf("filename: [%s]\n", filename);
        return NULL;
    }
    /* printf("read bytes: %d\n", cnt); */

    fclose(fp);
    str[sb.st_size] = 0;
    /* printf("content is:%s\n", str); */
    return str;
}

void toPipe (int inPipe[], char outstr[])
{
    int len = strlen(outstr);
    printf(":: strlen(outstr) = %d\n", len);
    fflush(stdout);
    int cnt = write(inPipe[1], outstr, len);
    if (-1 == cnt) { perror("write to pipe"); exit(-1); }
}

void fromPipe(int outPipe[], int len, char retval[])
{
    char buf[SIZE]; memset(buf, 0, SIZE);
    int cnt = read(outPipe[0], buf, len);
    if (-1 == cnt) { perror("read from pipe"); exit(-1); }
    printf(":: %d [child out]\n%s\n", cnt, buf);
    fflush(stdout);
    if (NULL != retval) {
        if (SIZE <= len) { printf("out of buf\n"); fflush(stdout); exit(-1); }
        strncpy(retval, buf, len);
    }
}


void runvfm (char vfm[], char base[], char code[], char *params[], char *env[], char run[],
             char hash[])
{
    int pid, in, out, cnt, inPipe[2], outPipe[2];
    if (pipe(inPipe)  == -1) { perror("In Pipe Failed");  exit(-1); }
    if (pipe(outPipe) == -1) { perror("Out Pipe Failed"); exit(-1); }
    switch(pid = fork()) {
    case -1:
        perror("fork");
        exit(-1);
    case 0:
        close(0);
        close(1);
        dup2(inPipe[0], 0);
        close(inPipe[1]);
        close(outPipe[0]);
        dup2(outPipe[1], 1);
        execvp("/home/rigidus/repo/from-C-to-Forth/interact", params);
    }
    printf(":: pid = %d\n", pid);
    fflush(stdout);
    char result[SIZE];
    fromPipe(outPipe, 30, NULL);
    toPipe(inPipe, "5\n");
    fromPipe(outPipe, 30, NULL);
    toPipe(inPipe, "7\n");
    fromPipe(outPipe, 30, result);
    toPipe(inPipe, "y\n");
    int pos = strcspn(result, "\n");
    *(result+pos) = 0;
    fromPipe(outPipe, 30, NULL);
    printf("Result is [%s]\n", result);
    fflush(stdout);
}
