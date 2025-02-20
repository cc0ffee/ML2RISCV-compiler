//Result: 6
struct s1 {
  int a;
  char b;
  int c;
};

struct s2 {
  int d;
  s1 e;
};

int main()
{
  s1 st = new(s1);
  st.a = 2;
  st.b = 'a';
  st.c = 1;
  s2 st2 = new(s2);
  st2.d = 3;
  st2.e = st;
  return st2.d + st2.e.a + st2.e.c;
}
