# cart

Tool for creating the driver program for a [veggies] or [garden] test suite.

## Downloading and installing

Build and install with the Fortran Package Manager ([fpm]):

```
git clone https://gitlab.com/everythingfunctional/cart.git
cd cart
fpm install
```

## Using

Make a veggies/cart driver as follows:

```
cart [--garden] [--setup_module "setup_m" --setup_procedure "setup" --teardown_module "teardown_m" --teardown_procedure "teardown"] driver_name test_file [more [test [files [...]]]]
```

It is expected that each `test_file` contains a single module with a name ending with `_test`,
and that the name of the file is the same as the module name, with `.f90` appended.
This module is expected to contain at least 1 public function with names starting with `test_`.
Thus the most common usage is a command like:

```
cart test/main.f90 test/*_test.f90
```

If you have code that needs to be executed before and/or after the entire test suite,
then place it in subroutines that take no arguments,
and provide the names of the procedures and modules they are in as the optional arguments.

[veggies]: https://gitlab.com/everythingfunctional/veggies
[garden]: https://gitlab.com/everythingfunctional/garden
[fpm]: https://github.com/fortran-lang/fpm
