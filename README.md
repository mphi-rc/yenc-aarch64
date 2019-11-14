# yEnc-aarch64

This is a yEnc encoder I built to learn ARM64 assembly programming on Linux.

I've [documented the code excessively](https://github.com/mphi-rc/yenc-aarch64/blob/master/encoder.s#L251). If you're
looking to learn assembly or low level systems programming, I think reading the source isn't a bad place to start.

Also, check out [why using yEnc encoding is a bad idea](https://en.wikipedia.org/wiki/YEnc#Problems).

## Features
- Pure assembly with no dependencies
- Optimized (vectorized) implementation using SIMD instructions
- Low memory requirements

## Build

On a 64-bit ARM host, build a static ELF executable with: `gcc -nostdlib -static encoder.s`.

## Performance

You'll need access to an aarch64 machine with hardware performance counters exposed. The bare metal Cavium ThunderX
servers from [Packet.net](http://packet.net/) worked well for me.

Install perf and, ideally, taskset. On Ubuntu, that's `sudo apt install linux-tools-common linux-tools-$(uname -r)`.

Profile with `taskset -c 2 perf stat ./a.out input output`.

     Performance counter stats for './a.out /dev/shm/random_1M /dev/shm/output':
     
              4.919380      task-clock (msec)         #    0.914 CPUs utilized
                     0      context-switches          #    0.000 K/sec
                     0      cpu-migrations            #    0.000 K/sec
                   269      page-faults               #    0.055 M/sec
             9,829,420      cycles                    #    1.998 GHz
             3,743,103      instructions              #    0.38  insn per cycle
               376,389      branches                  #   76.511 M/sec
               117,801      branch-misses             #   31.30% of all branches
    
           0.005381709 seconds time elapsed
