
// This file is variation on the file in RcppIconvExample at
//   https://github.com/eddelbuettel/rcppiconvexample/blob/master/src/strings.cpp
// which is itself a variation / refactoring of the two functions in
//   https://fishandwhistle.net/post/2021/using-rs-cross-platform-iconv-wrapper-from-cpp11/

#include <fstream>
#include <R_ext/Riconv.h>
#include <tidyCpp>

extern "C" {

SEXP _readAndConvert(SEXP filesexp, SEXP encsexp) {
    std::string filename(R::asCharacter(filesexp));
    std::string encoding(R::asCharacter(encsexp));

    const int len = 2048;
    char buffer[len/2];

    std::ifstream file;
    file.open(filename, std::ifstream::in | std::ifstream::binary);

    file.read(buffer, len/2);
    size_t n_read = file.gcount();
    file.close();

    if (encoding == "") {       // no encoding given so return 'as is'
        std::string s(buffer, n_read);
        R::Protect vec(R::allocVectorCharacter(1));
        R::setStringElement(vec, 0, R::mkChar(s.c_str()));
        return vec;
    }

    std::string str_source(buffer, n_read);

    void* iconv_handle = Riconv_open("UTF-8", encoding.c_str());
    if (iconv_handle == ((void*) -1)) {
        std::string s = std::string("Can't convert from '") + encoding + std::string("' to 'UTF-8'");
        R::error(s.c_str());
    }

    const char* in_buffer = str_source.c_str();
    char utf8_buffer[len];
    char* utf8_buffer_mut = utf8_buffer;
    size_t in_bytes_left = n_read;
    size_t out_bytes_left = len;

    size_t result = Riconv(iconv_handle, &in_buffer, &in_bytes_left, &utf8_buffer_mut, &out_bytes_left);
    Riconv_close(iconv_handle);

    if (result == ((size_t) -1) || (in_bytes_left != 0)) {
        std::string s = std::string("Failed to convert file contents to UTF-8");
        R::error(s.c_str());
    }

    std::string s(utf8_buffer, len - out_bytes_left);
    R::Protect vec(R::allocVectorCharacter(1));
    R::setStringElement(vec, 0, R::mkChar(s.c_str()));
    return vec;
}

}
