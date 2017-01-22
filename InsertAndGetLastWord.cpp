#include <string.h>
#include <iostream>
#include <string>
#include <fstream>
#include <map>

using namespace std;

int main() {

  std::ifstream infile("words.txt");
  std::string line;
  std::map<string, int> Words;

  int i;
  i = 0;
  while (std::getline(infile, line)) {
    if (i > 3) {
      break;
      }
    Words[line] = 0;
    // std::cout << line << std::endl;
    i++;
  }


  std::map<std::string, int>::iterator it;
  if((it = Words.find("'ll")) != Words.end())
    std::cout << it->first<<" =>"<< it->second << '\n';


  return 0;
}
