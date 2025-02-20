//Result: 75

struct car {
    int position;
    int speed;
    int time;
};

void increase(car c) {
    c.speed = c.speed + 5;
}

void time_step(car c) {
    c.time = c.time + 1;
    c.position = c.position + c.speed;
}

int main() {
  car c1 = new(car);
  c1.position = 0;
  c1.time = 0;
  c1.speed = 5;

  for (int i = 0; i < 5; i++) {
    time_step(c1);
    increase(c1);
  }

  int result = c1.position; 

  return result;
}