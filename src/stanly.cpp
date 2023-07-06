#include <fstream>
#include <iostream>
#include <string>

std::string loadFileToString(const std::string& filename) {
  std::ifstream file(filename);  // Open the file
  std::string content;           // String to store the file content

  if (file) {
    // Read the entire file content into the string
    content.assign((std::istreambuf_iterator<char>(file)), (std::istreambuf_iterator<char>()));
  } else {
    std::cout << "Error opening file: " << filename << std::endl;
  }

  return content;
}

int main(int argc, char* argv[]) {
  std::string fileContent = loadFileToString(argv[1]);

  // Display the loaded content
  std::cout << "File content: " << std::endl;
  std::cout << fileContent << std::endl;

  return 0;
}
