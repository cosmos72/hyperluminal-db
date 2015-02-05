Hyperluminal-DB
===============

Summary
-------
Hyperluminal-DB is a high-performance, memory-mapped object database for Common Lisp.

It is based on Hyperluminal-MEM, a serialization/deserialization library,
and STMX, a high-performance implementation of transactional memory.

Features
--------
Hyperluminal-DB is designed and optimized for the following scenarios:
- adding persistence and transactions to Lisp objects through an easy-to-use
  API, without an external relational database.
- suitable for very large datasets that can exceed available RAM.
- perfectly suited for fast storage as RAID or Solid State Disks (SSD).
- designed to allow high concurrency, i.e. hundreds or thousands of threads
  that simultaneously access the same dataset.
- designed and optimized for extremely fast transactions - as of 2013,
  theoretical peak is approx. **400 millions** concurrent transactions
  per second, on a fast desktop computer (Intel Core i7 4770, which
  supports hardware memory transactions) running 64 bit SBCL.
  - on the same hardware, micro-benchmarks exceeding **200 millions**
    concurrent transactions per second have been already measured in
    practice.
    The speed difference is largely due to the need to make hardware
    transactions compatible with software ones.
- fairly portable file format, independent from the Lisp implementation.
  File format only depends on endianity (small or big endian)
  and on user's choice between 32 and 64 bit formats.
  Conversion between small and big endian file format is trivial.
- optimized for 64 bit systems, where dataset is limited only by `mmap()`
  maximum size (on Linux 3.x, the limit is about 128 terabytes).
- usable on 32 bit systems, either retaining 64 bit file format
  (with some performance loss), or using native 32 bit file format - fast,
  but has the following limitations:
  - 100 user-defined persistent classes
  - 16 millions instances per user-defined persistent class
  - 256 megabytes storage per user-defined persistent class
  
  In any case, on 32 bit systems the dataset size is limited by `mmap()`
  maximum size, usually around 1 gigabyte

### Latest news, 24th January 2015

Released version 0.5.1.

This is the last release packaging together Hyperluminal-MEM and
Hyperluminal-DB in a single GPLv3 library.

### News, 9th February 2014

Released version 0.5.0.

The serialization library is tested, documented and ready to use.
It may still contain some rough edges and small bugs.

### News, 1st February 2014

The serialization library works and is in BETA status.

The memory-mapped database (built on top of the serialization library)
is in the early-implementation stage, not yet ready for general use.

Supported systems
-----------------
Hyperluminal-DB is currently tested on the following Common Lisp implementations:

* [SBCL](http://sbcl.org/)
  * version 1.1.20       (x86_64)   on Debian GNU/Linux jessie (x86_64)
  * version 1.1.15       (x86_64)   on Debian GNU/Linux jessie (x86_64)
  * version 1.1.14       (x86)      on Debian GNU/Linux jessie (x86_64)
  * version 1.1.14       (powerpc)  on Debian GNU/Linux jessie (powerpc) inside Qemu
  * version 1.0.57       (x86)      on Debian Ubuntu Linux 12.04LTS (x86)

* [CCL](http://ccl.clozure.com/)
  * version 1.9-r15769   (x86_64)   on Debian GNU/Linux jessie (x86_64)
  * version 1.9-r15769M  (x86)      on Debian GNU/Linux jessie (x86_64)
  * version 1.9-r15761   (linuxppc) on Debian GNU/Linux jessie (powerpc) inside Qemu
  * version 1.9-dev-r15475M-trunk (LinuxARM32) on Raspbian GNU/Linux (armhf) Raspberry Pi

* [ABCL](http://www.abcl.org/)
  * version 1.3.1                   on OpenJDK 1.7.0_55 (x86_64) on Debian GNU/Linux jessie (x86_64)
  
  Note: on ABCL, memory buffers are implemented using java.nio.ByteBuffer instead of CFFI-SYS
  raw memory pointers due to currently limited compatibility between ABCL and CFFI/OSICAT libraries.
  Memory-mapped files are supported, and internally use java.nio.channels.FileChannel.map()
  instead of OSICAT-POSIX (mmap)
  
* [CMUCL](http://www.cons.org/cmucl/)
  * version 20d Unicode  (x86)      on Debian GNU/Linux jessie  (x86_64)
  * version 20c Unicode  (x86)      on Debian GNU/Linux jessie  (x86_64)

  Note: CMUCL needs to be started with the option `-fpu x87` to run Hyperluminal-DB reliably,
  see [STMX documentation](https://github.com/cosmos72/stmx/blob/master/doc/supported-systems.md)
  for details.

### Unsupported systems

* [ECL](http://ecls.sourceforge.net/) has some known issues with CFFI, OSICAT and STMX,
  three libraries required by Hyperluminal-DB. Once support for these three libraries improves,
  Hyperluminal-DB can be tested on it too.

### Other systems

Hyperluminal-DB requires several libraries to work: LOG4CL, CLOSER-MOP, TRIVIAL-GARBAGE,
BORDEAUX-THREADS, CFFI, OSICAT, STMX and HYPERLUMINAL-MEM. The last five, while reasonably portable,
exploit features well beyond ANSI Common Lisp and their support for the various Common Lisp
implementations varies widely.

For this reason no general guarantees can be given: Hyperluminal-DB
may or **may not** work on other, untested Common Lisp implementations.

Installation and loading
------------------------

Hyperluminal-DB is available from [GitHub](https://github.com/cosmos72/hyperluminal-db).
The simplest way to obtain it is to first install [Quicklisp](http://www.quicklisp.org)
then download Hyperluminal-MEM and Hyperluminal-DB into your Quicklisp local-projects folder.
Open a shell and run the commands:

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/cosmos72/hyperluminal-mem.git
    $ git clone git://github.com/cosmos72/hyperluminal-db.git

then open a REPL and run:

    CL-USER> (ql:quickload "hyperluminal-db")
    ;; lots of output...
    CL-USER> (use-package :hlmem)
    CL-USER> (use-package :hldb)
     
If all goes well, this will load Hyperluminal-DB and its dependencies:

- `log4cl`
- `closer-mop`
- `trivial-garbage`
- `bordeaux-threads`
- `cffi`
- `osicat`
- `stmx`
- `hyperluminal-mem`


### Troubleshooting

In case you get errors:

- check that Quicklisp is installed correctly, for example by
  executing at REPL:

        CL-USER> (ql:quickload "closer-mop")

- check that you downloaded Hyperluminal-DB creating an `hyperluminal-db/` folder inside
  your Quicklisp local-projects folder, usually `~/quicklisp/local-projects`


### Testing that it works

After loading Hyperluminal-DB for the first time, it is recommended to run
STMX, Hyperluminal-MEM and Hyperluminal-DB test suites to check that everything works as expected.
From the REPL, run:

    CL-USER> (asdf:test-system :stmx)
    ;; lots of output...
     Did 7133 checks.
        Pass: 7133 (100%)
        Skip: 0 ( 0%)
        Fail: 0 ( 0%)

    CL-USER> (asdf:test-system :hyperluminal-mem)
    ;; lots of output...
     Did 4092 checks.
        Pass: 4092 (100%)
        Skip: 0 ( 0%)
        Fail: 0 ( 0%)
        
    CL-USER> (asdf:test-system :hyperluminal-db)
    ;; lots of output...
     Did 2505 checks.
        Pass: 2505 (100%)
        Skip: 0 ( 0%)
        Fail: 0 ( 0%)
        
Note: all the `(asdf:test-system ...)` intentionally work only **after**
the corresponding library has been loaded successfully with `(ql:quickload ...)`.

All the test suites should report zero Skip and zero Fail; the number of Pass may vary.
You are welcome to report any failure you get while running the test suites,
please include in the report:
- operating system name and version (example: Debian GNU/Linux x86_64 version 7.0)
- Common Lisp implementation and version (example: SBCL 1.0.57.0.debian, x86_64)
- **exact** output produced by the test suite
  (remember to specify if the error is in STMX, Hyperluminal-MEM or Hyperluminal-DB test suite)
- any other relevant information

See "Contacts, help, discussion" below for the preferred method to send the report.


Implementation
--------------
Hyperluminal-DB is loosely inspired by some techniques used by
[manardb](http://cl-www.msi.co.jp/projects/manardb/index.html)
but it is a completely separate and independent project.

It is based on [STMX](https://github.com/cosmos72/stmx),
a high-performance hybrid transactional memory library,
and Hyperluminal-MEM, a high-performance serialization library,
both from the same author.

Hyperluminal-DB uses a (supposedly) clever trick in order to overcome Intel
claims that hardware memory transactions (specifically, Intel TSX) cannot
perform input/output.

The result is that Hyperluminal-DB is able to perform **transactional**
input/output while running hardware memory transactions - an apparent
paradox - in an extremely specific but significant case: reading and
writing memory-mapped files.

This allows reaching extremely high transaction speeds: the only hard limit
is the hardware - an Intel Core i7 4770 peaks at **400 millions** transactions
per second when all the 4 cores and hyperthreading are exploited.

Quite clearly, the speed also strongly depends on the amount (and type) of
data read and written during each transaction.


Basic usage
-----------

TODO


File format and ABI
-------------------
  
Hyperluminal-DB file format and ABI is inherited from Hyperluminal-MEM,
which performs autodetection to match Lisp idea of CFFI-SYS pointers:
* 32 bit when CFFI-SYS pointers are 32 bit,
* 64 bit when CFFI-SYS pointers are 64 bit,
* and so on...

It is possible to override such autodetection by adding an appropriate entry
in the global variable `*FEATURES*` **before** compiling and loading Hyperluminal-MEM,
see [Hyperluminal-MEM/README](https://github.com/cosmos72/hyperluminal-mem/README.md)
for details.

------
As of February 2015, Hyperluminal-DB is being written by Massimiliano Ghilardi
and it is considered by the author to be in ALPHA status.

Contacts, help, discussion
--------------------------
As long as the traffic is low enough, [GitHub Issues](https://github.com/cosmos72/hyperluminal-db/issues)
can be used to report test suite failures, bugs, suggestions, general discussion etc.

If the traffic becomes high, more appropriate discussion channels will be set-up.

The author will also try to answer support requests, but gives no guarantees.


Legal
-----

Hyperluminal-DB is released under the terms of the [GNU General Public
License v3.0](http://www.gnu.org/licenses/gpl-3.0.html), known
as the GPLv3.
