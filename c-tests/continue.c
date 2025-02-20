//Result: 50
int main() {
  int i = 0;
  int j = 0;
  while (i < 50) {
    i++;
    j++;
    continue;
    i++;
  }
  return j;
}
