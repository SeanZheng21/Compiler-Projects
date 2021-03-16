#include <stdio.h>
#include <stdlib.h>

struct A
{
  void* (**vtable)();
  int a;
};

void* A_f(struct A* this, int arg)
{
  printf("A_f %d %d\n", this->a, arg);
}

void* (*A_vtable[1])() = { A_f };

struct B
{
  void* (**vtable)();
  int a;
  int b;
};

void* B_g(struct B* this, int arg1, int arg2)
{
  printf("B_g %d %d %d\n", this->b, arg1, arg2);
  return (void*)123456789;
}

void* (*B_vtable[2])() = { A_f, B_g };

struct C
{
  void* (**vtable)();
  int a;
  int b;
  int c;
};

void* C_f(struct C* this, int arg)
{
  printf("C_f %d %d\n", this->c, arg);
}

void* C_g(struct C* this, int arg1, int arg2)
{
  printf("C_g %d %d %d\n", this->c, arg1, arg2);
  return (void*)10101010;
}

void* (*C_vtable[2])() = { C_f, C_g };

int main(int argc, char *argv[])
{
  struct A* object_a = (struct A*) malloc(sizeof(*object_a));
  object_a->vtable = A_vtable;
  object_a->a = 42;

  object_a->vtable[0](object_a, 123);

  struct B* object_b = (struct B*) malloc(sizeof(*object_b));
  object_b->vtable = B_vtable;
  object_b->a = 1;
  object_b->b = 2;

  object_b->vtable[0](object_b, 4);
  long res = (long)object_b->vtable[1](object_b, 6, 7);
  printf("%ld\n", res);

  struct C* object_c = (struct C*) malloc(sizeof(*object_c));
  object_c->vtable = C_vtable;
  object_c->a = 1;
  object_c->b = 2;
  object_c->c = 3;

  struct B* object_bb = (struct B*)object_c;

  object_bb->vtable[0](object_bb, 2017);
  object_bb->vtable[1](object_bb, 2016, 2017);

  object_bb = object_b;
  object_bb->vtable[1](object_bb, 2016, 2017);

  struct A* object_aa = (struct A*)object_c;
  object_aa->vtable[0](object_aa, 1975);

  object_aa = (struct A*)object_b;
  object_aa->vtable[0](object_aa, 1975);
  return 0;
}
