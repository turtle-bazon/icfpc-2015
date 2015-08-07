#include <stdio.h>

int main ()
{
    srand (17);
    int i;
    for (i = 0; i < 10; i++)
    {
        printf ("next rand: %d\r\n", rand());
    }
    return 0;
}
