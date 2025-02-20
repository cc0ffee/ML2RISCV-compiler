//Result: 9
struct node {
  int val;
  node tl;
};

int peek(node list)
{
  if (((int)list) == 0) return -1;
  return list.val;
}

node push(int val, node list)
{
  node newnode = new(node);
  newnode.val = val;
  newnode.tl = list;
  return newnode;
}

node pop(node list)
{
  if (((int)list) == 0) return list;
  return list.tl;
}

int main()
{
  node h = (node)0;
  for (int i = 0; i < 20; i++) {
    h = push(i, h);
  }
  for (i = 0; i < 10; i++) {
    h = pop(h);
  }
  return peek(h);
}
