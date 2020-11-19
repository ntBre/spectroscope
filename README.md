# spectroscope

spectroscope is a molecule and vibrational frequency viewer designed
for use with the SPECTRO program.

## Installation

Installation assumes that you have Racket installed with the `raco`
tool. First clone this repository:

```
$ git clone https://github.com/ntBre/spectroscope.git
```

If you have `make` installed, you can go into the `spectroscope`
directory and run

```
$ make
```

to build the executable.

The makefile is currently very minimal, so you can also just run

```
$ raco exe -o scope main.rkt
```

to have the same effect without the `make` dependency.

## Usage

The only use currently is to run

```
$ scope [spectro output file]
```

If no name is supplied for the output file, the name "spectro.out" is
assumed.

Once the program opens, you can select a frequency with the mouse or
with j and k to display its motion. The magnitude of the motion can be
adjusted with the slider or the h and l keys. You can display the
reference geometry without any displacements by pressing Escape. Exit
with q.
