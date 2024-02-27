#include <stdio.h>
#define RLS 5

#if RLS < 3
   #define SOBAD
#else
   #define SOGOOD
#endif

#if RLS > 4
   #define SICKMATE
#endif

#if RLS > 10
   #define NAUGHTY
#else
   #define NICE
#endif

#if RLS <= 5
   #define COOLNESS
#endif

#if DEFINED(RLA)
   #define NOICE
#endif

#if RLA > 5
   #define BAD
#elif RLA < 5
   #define ALSOBAD
#else
   #define ACTUALLYGOOD
#endif

int main()
{
    printf("Hello, World!\n");
    MAX(4, 5);
    return 0;
}