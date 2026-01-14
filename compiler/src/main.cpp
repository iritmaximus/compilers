#include <iostream>

int main(int argc, char *argv[]) {
  if (argc >= 2) {
    std::cout << argv[1] << std::endl;
  } else {
    std::cout << "No args :(" << std::endl;
  }
  std::cout << "Hello world!" << std::endl;
  return 0;
}
