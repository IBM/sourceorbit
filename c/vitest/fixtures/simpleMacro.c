#include <stdio.h>
#define MAX(a,b) {\
    printf("%d ", a); \
    printf("%d\n", b); \
}
#define COOL
#define AWESOME 1337

int main()
{
    printf("Hello, World!\n");
    MAX(4, 5);
    return 0;
}