#include <stdio.h>
#include <stdlib.h>

#include "runvfm64.h"


int main(int argc, char * argv[])
{
    char * libf = read_file_into_string("./src64/jonesforth64.f");
    if (NULL == libf)
    {
        printf("Aborting: no lib.f file\n");
        exit(-1);
    }
    char * args[] = { "forth64", "asd", "qwe", NULL };
    char * envp[] = { "USER=test", "HOME=/home/test", NULL };

    runvfm("./forth64",
           libf,
           ": ALFA .\" ᚜do-beta-gamma᚛\" CR ;\n",
           args,
           envp,
           "ALFA\n",
           "hash"
           );

    return 0;
}
