//Result: 45
int main()
{
  int A[] = new(int[10]);
  int result = 0;
  for (int i = 0; i < 10; i++) {
    A[i] = i;
  }
  for (int j = 0; j < 10; j++) {
    result += j;
  }
  return result;
}
