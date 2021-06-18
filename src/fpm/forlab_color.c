#include <stdio.h>
#include <windows.h>

void color(long int x)
// ref: https://docs.microsoft.com/en-us/windows/console/setconsoletextattribute
{
    if(x>=0 && x<=255)
        SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), x);
    else
        SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
}