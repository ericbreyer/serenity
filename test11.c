struct S {
  int c
};

struct S A[1] = {{10}};


int main() {
    A[0].c = A[0].c + 10;
}