#include <stddef.h>

struct st;

struct st {
   __attribute__((annotate("str"))) int  a ; 
  /*hello=1*/int b;
  long c; // off=2
  struct st* d;
};

struct stAr {
  int a;
  int b[2];
  struct st c;
};

typedef unsigned long undefined64;

undefined64 abcc;

// int argt(int, int);
// int argt(int a, int b);
// extern int stGet(struct st struc);
// extern int arrGet(int arr[]);
// struct st g1 = (struct st) {1, 2};

// int main1() {
//   do {
//   ;
//   }while (1);
//   int a = 11;

//   *&a = 1+1;
//   *(&a+1) = sizeof(int);
//     goto abcd;
//   abcd:
//   main1();
//   arrGet((int[3]){1,2,3});
//   stGet( (struct st) {1, 2} );
//   if (a) (&a)[2];
//   if (a> 0) {
//     return (&a)[2];
//   } else {
//     return (&a)[3];
//   }
//   return a>0?g1.a: a;
// }
// int (*funcs[2])() = {0};
// int (*funcs1[2])() = { };
// int (*funcs2[])() = {main1, 0};
// struct stAr g = {1, {2, 3}, {4, 5}};
// int * aaa = NULL;
// // heell
// struct {
//   int a;
//   int b;
// } g2 = {1, 2};
// // int func3(struct {
// //   int a;
// //   int b;
// // } g3);
// int tme = 1;
// const int fme = 0;
// int argt(int a, int b) {
//   return a+b;
// }

// int switch_test(int a) {
//   switch (a) {
//   case 1:
//   case 2:
//     return 3;
//   case 4:
//     return 5;
//   }
//   return -1;
// }
