#include <iostream>
#include <stdint.h>
#include <string>
#include <vector>
#include <algorithm>
#include "include/fplus/fplus.hpp"
#include <sys/time.h>
#include <chrono>

using namespace fplus;

uint8_t f(uint8_t a, uint8_t b) {
  return a + b;
}


template<typename Out>
void split(const std::string &s, char delim, Out result) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        *(result++) = item;
    }
}

std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}

int main() {
  std::ifstream ifs("data.txt");
  std::string data( (std::istreambuf_iterator<char>(ifs) ),
                       (std::istreambuf_iterator<char>()    ) );
  std::stringstream s(data);
  
  std::vector<std::string> as = split(data, ' ');

  std::vector<uint8_t> a(2000000);
  std::vector<uint32_t> b(2000000);

  for (size_t z = 0; z < 2000000; z += 1) {
    a[z] = (uint8_t)std::stoi(as[z], nullptr, 64);
    b[z] = z;
  }

  
  auto start = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now().time_since_epoch()).count();
  auto n = all_by(
    ([](int it) { return it > 4; }),
    transform(
      ([](int it) { return it * 2; }),
      keep_if(
        ([](int it) { return it % 4 > 1; }),
        transform(
          ([](std::pair<int, int> x) { return x.first + x.second; }),
          zip(a, b)))));
  auto finish = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now().time_since_epoch()).count();
  
  std::cout << "example0 " << finish - start << "\n";
    
  start = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now().time_since_epoch()).count();
  auto o = fold_left(
    ([](int memo, int x) { return memo + x; }),
    0,
    transform(
      ([](int it) { return f(it, it); }),
      transform(
        ([](int it) { return (int8_t)it - 7; }),
        a)));

  finish = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now().time_since_epoch()).count();
  
  std::cout << "example1 " << finish - start << "\n";
}
