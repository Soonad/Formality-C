## Formality-C

### About Formality-C
This is a Formality-Net Runtime written in C language. To know more about the Formality Language, you can access the Wiki [here](https://docsmoonad.readthedocs.io/en/latest/language/Motivation.html)

### Compilation and Execution
To compile the runtime you can simply execute the Makefile inside the FM-Net folder.

There are 3 compile options available:
  * **default**: This option compiles the code optimized for fast execution. It aims to reduce function calls, optimize cache use and avoid redundant computations in each rewrite. The source code for this part focuses in speed and is less human-readable than its _legacy_ counterpart.

  * **legacy**: This option compiled the code that served as basis for the optimized code. This build option was included for comparison and testing purposes only. It still includes compiler optimizations.

  * **debug**: This option is for debugging only. It disables all compiler optimizations, what consequently produces the largest executable and slowest code.

### Testing

To test the code in a unix-based machine, just access the terminal, enter the FM-Net folder and type:

`make <compile option>`

This should create a executable file called `fm_net_<CompileOption>`.

The main file contains a hard-coded benchmark net which represents the application of keccak256 1024 times in an empty string. To test with other inputs, just change the net in this file.

### Using fm-net
To use fm-net in other C programs, just include fm-net.h in a C file just like any other library. The file main.c serves as an example.
