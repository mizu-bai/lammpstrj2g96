# lammpstrj2g96

A lammpstrj to g96 converter

## Build

```
$ gfortran src/lammpstrj2g96.f90 -O2 -o lammpstrj2g96.x
```

## Usage

Simply run this command below to get your `md.g96`.

```
$ ./lammpstrj2g96.x md.lammpstrj
```

NOTE: This converter only supports lammpstrj dumped by:

```
dump ID group-ID custom N file id type x y z vx vy vz
dump_modify ID sort id time yes
```

or

```
dump ID group-ID custom N file id type x y z
dump_modify ID sort id time yes
```

If you already have a lammpstrj but not in this format, rerun to dump a compatible one.

The converted `g96` files can be converted to other formats like `trr` or `xtc` with `gmx trjconv` command.

## License

BSD 2-Clause License
