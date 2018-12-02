#include <unordered_map>
#include <vector>
#include <string>
#include <cstdlib>
#include <iostream>
#include <chrono>

using namespace std;

int main(int, char **)
{
    int n = 1000000;
    vector<string> strings;
    strings.reserve(n);
    int len = 50;
    for (int i = 0; i < n; ++i) {
        vector<char> chars(len + 1, '\0');
        for (int j = 0; j < len; ++j) {
            int r = rand() % 62;
            chars[j] = "ABCDEFGHIJKLMNOPOQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"[r];
        }
        strings.push_back(chars.data());
    }

    unordered_map<string, int> h;
    auto before = chrono::high_resolution_clock::now();
    for (int i = 0; i < n; ++i) {
        h[strings[i]] = 1;
    }
    auto after = chrono::high_resolution_clock::now();
    cout << n << " insertions: "
         << chrono::duration_cast<chrono::microseconds>(after - before).count()
         << " us" << endl;

    h = unordered_map<string, int>();
    h.reserve(n);
    before = chrono::high_resolution_clock::now();
    for (int i = 0; i < n; ++i) {
        h[strings[i]] = 1;
    }
    after = chrono::high_resolution_clock::now();
    cout << n << " insertions with preallocation: "
         << chrono::duration_cast<chrono::microseconds>(after - before).count()
         << " us" << endl;

    for (int i = n - 1; i > 0; --i) {
        int j = rand() % (i + 1);
        auto iv = strings[i];
        strings[i] = strings[j];
        strings[j] = iv;
    }

    before = chrono::high_resolution_clock::now();
    int total = 0;
    for (int i = 0; i < n; ++i) {
        total += h[strings[i]];
    }
    after = chrono::high_resolution_clock::now();
    cout << n << " reads: "
         << chrono::duration_cast<chrono::microseconds>(after - before).count()
         << " us" << endl;
}
