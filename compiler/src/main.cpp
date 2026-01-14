#include <fstream>
#include <iostream>

int main(int argc, char *argv[]) {
  if (argc < 3) {
    std::cout << "Incorrect arg count :(" << std::endl;
    return 1;
  }
  std::cout << argv[1] << argv[2] << std::endl;
  std::cout << "Hello world!" << std::endl;

  std::ofstream result_file(argv[2]);
}
