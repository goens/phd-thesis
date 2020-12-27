#include <iostream>
#include <filesystem>
#include <fstream>

using namespace std;
using namespace std::filesystem;

int main(){
   for(auto& p: filesystem::directory_iterator("examples"))
        cout << p.path() << '\n';
}
