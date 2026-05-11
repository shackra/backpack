/* abi-check.c — Read the ABI version from a compiled tree-sitter grammar.
 *
 * Usage: abi-check <library.so> <tree_sitter_LANG>
 *
 * Opens the shared library, calls the tree_sitter_LANG() function, and
 * prints the ABI version (the first uint32_t in the TSLanguage struct)
 * to stdout.
 */

#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "usage: abi-check <library.so> <tree_sitter_LANG>\n");
        return 1;
    }

    void *lib = dlopen(argv[1], RTLD_NOW);
    if (!lib) {
        fprintf(stderr, "dlopen: %s\n", dlerror());
        return 1;
    }

    /* The TSLanguage struct's first field is uint32_t version (the ABI). */
    typedef struct { uint32_t version; } TSLanguage;
    typedef const TSLanguage *(*LangFunc)(void);

    LangFunc fn = (LangFunc)dlsym(lib, argv[2]);
    if (!fn) {
        fprintf(stderr, "dlsym: %s\n", dlerror());
        dlclose(lib);
        return 1;
    }

    printf("%u\n", fn()->version);
    dlclose(lib);
    return 0;
}
