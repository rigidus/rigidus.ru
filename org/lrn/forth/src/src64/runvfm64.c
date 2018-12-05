#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>

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

void toFVM (int inPipe[], char outstr[])
{
    int len = strlen(outstr);
    /* printf(":: strlen(outstr) = %d\n", len); */
    /* fflush(stdout); */
    int cnt = write(inPipe[1], outstr, len);
    if (-1 == cnt) { perror("write to pipe"); exit(-1); }
}

char* fromFVM(int outPipe[], int len)
{
    char *buf = malloc(len);
    if(NULL == buf) {
        perror("malloc");
        printf("function: fromFVM\n");
        return NULL;
    }
    memset(buf, 0, len);
    int cnt = read(outPipe[0], buf, len);
    if (-1 == cnt) { perror("read from pipe"); exit(-1); }
    /* if (0  == cnt) { perror("eof"); exit(-1); } */
    /* if (1  == cnt) { perror("1"); exit(-1); } */
    /* printf(":: %d [child out]\n%s\n", cnt, buf); */
    /* fflush(stdout); */
    return buf;
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
        execve(vfm, params, env);
    }
    printf(":: pid = %d\n", pid);
    fflush(stdout);

    toFVM(inPipe, base); sleep(1);
    char vfm_hello[] = "VFM VERSION 47 OK\n";
    char *hello_str = fromFVM(outPipe, 30);
    if (0 != strncmp(vfm_hello, hello_str, sizeof(vfm_hello))) {
        printf(":: vfm hello error:\n");
        printf("[%s]\n", hello_str);
        printf("expected: [%s]\n", vfm_hello);
        exit(-1);
    }
    free(hello_str);

    char one[] = "4 3 + . \n";
    printf(">> [%s]\n", one);
    fflush(stdout);
    toFVM(inPipe, one);

    char *result2 = fromFVM(outPipe, SIZE);
    printf("<< [%s]\n", result2);
    fflush(stdout);
    free(result2);

    char two[] = "1 2 3 + . BYE \n";
    printf(">> [%s]\n", two);
    fflush(stdout);
    toFVM(inPipe, two);

    char *result1 = fromFVM(outPipe, SIZE);
    printf("<< [%s]\n", result1);
    fflush(stdout);
    free(result1);

    int status;
    wait(&status);

    printf("Fin: %d\n", status);
    fflush(stdout);

    exit(0);
}
