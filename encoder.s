.arch armv8-a
.globl _start

.set SYS_OPENAT, 56
.set AT_FDCWD, -100
.set O_RDONLY, 0
.set O_CREAT, 64
.set O_WRONLY, 1
.set O_RDWR, 2
.set MODE_644, 420

.set SYS_FSTAT, 80
.set stat.st_size, 48

.set SYS_MMAP, 222
.set PROT_READ, 1
.set PROT_WRITE, 2
.set MAP_SHARED, 1

.set SYS_MADVISE, 233
.set MADV_SEQUENTIAL, 2

.set SYS_FALLOCATE, 47
.set DEFAULT, 0

.set SYS_EXIT, 93

.set SYS_FTRUNCATE, 46

.set SYS_WRITE, 64
.set STDERR, 2

.macro openat dirfd, filename, flags
	mov x0, \dirfd
	mov x1, \filename
	mov x2, \flags
	mov x8, #SYS_OPENAT
	svc 0
.endm

.macro openat_create dirfd, filename, flags, permissions
	mov x0, \dirfd
	mov x1, \filename
	mov x2, \flags
	orr x2, x2, #O_CREAT
	mov x3, \permissions
	mov x8, #SYS_OPENAT
	svc 0
.endm

.macro fstat fd, statbuf
	mov x0, \fd
	mov x1, \statbuf
	mov x8, #SYS_FSTAT
	svc 0
.endm

.macro mmap addr, length, prot, flags, fd, offset
	mov x0, \addr
	mov x1, \length
	mov x2, \prot
	mov x3, \flags
	mov x4, \fd
	mov x5, \offset
	mov x8, #SYS_MMAP
	svc 0
.endm

.macro madvise addr, length, advice
	mov x0, \addr
	mov x1, \length
	mov x2, \advice
	mov x8, #SYS_MADVISE
	svc 0
.endm

.macro fallocate fd, mode, offset, length
	mov x0, \fd
	mov x1, \mode
	mov x2, \offset
	mov x8, #SYS_FALLOCATE
	svc 0
.endm

.macro ftruncate fd, length
	mov x0, \fd
	mov x1, \length
	mov x8, #SYS_FTRUNCATE
	svc 0
.endm

.macro exit code
	mov x0, \code
	mov x8, #SYS_EXIT
	svc 0
.endm

.macro validate cond, actual, expected, message, len
	cmp \actual, \expected
	b.\cond #40 // 4*(6+3+1)
	mov x0, #STDERR
	ldr x1, =\message
	mov x2, #\len
	mov x8, #SYS_WRITE
	svc 0
	exit #1
	b end
.endm

/* This is an optimized scalar implementation that operates on single bytes.
 *
 * We're aiming to optimize the number of instructions executed for the "average" byte. The average byte is unlikely
 * to need escaping, so we optimize handling the average byte fast, at the cost of handling rare values slow.
 *
 * A naive algorithm that equality checks against each of the four values use ~4 comparison and ~4 branch
 * instructions, on average. This implementation uses ~1.6 and ~2, respectively.
 */
.macro process_8 tmp1, tmp2
	add \tmp2, \tmp1, #42

	/* Since our register is wider than 8-bits, we have to handle the modulo 256 step explicitly. */
	and \tmp2, \tmp2, #0xff

	/* We know that 0x3d (61 decimal) is largest of the values needing escaping. If our byte is greater than this,
	 * we can branch immediately, write it to the output and we're done. */
	cmp \tmp2, #0x3d
	b.gt #40 // 4*(9+1)
	b.eq #28 // 4*(6+1)

	/* Now we use the same trick for everything between 0x3d and 0x0d. */
	cmp \tmp2, #0x0d
	b.gt #28 // 4*(6+1)
	b.eq #16 // 4*(3+1)

	/* We have two more escape characters to check. Let's only branch if our byte = 0x0a. */
	cmp \tmp2, #0x0a
	b.eq #8 // 4*(1+1)

	/* Our final escape character to check is zero. Luckily, we can use single compare-and-branch-if-non-zero
	 * instruction to handle this case. */
	cbnz \tmp2, #12 // 4*(2+1)

	/* If we fall through, we must have 0x0. Otherwise, we jumped here. We don't have to handle overflow because
	 * our largest value to escape is 61, and we're using the single byte store instruction. */
	strb w15, [x16], #1
	add \tmp2, \tmp2, #64

	/* If we fall through, we'll print the escaped value. Otherwise, we jumped here and we'll print the value
	 * unmodified. */
	strb \tmp2, [x16], #1
.endm

.macro postprocess_8 tmp
	cmp \tmp, #0x3d
	b.gt #40 // 4*(9+1)
	b.eq #28 // 4*(6+1)
	cmp \tmp, #0x0d
	b.gt #28 // 4*(6+1)
	b.eq #16 // 4*(3+1)
	cmp \tmp, #0x0a
	b.eq #8 // 4*(1+1)
	cbnz \tmp, #12 // 4*(2+1)
	strb w15, [x16], #1
	add \tmp, \tmp, #64
	strb \tmp, [x16], #1
.endm

/* Once we're processing 4 bytes at a time, we've hit the point of diminishing returns for our divide and conquer
 * algorithm, and it's empirically faster to process each byte sequentially.
 *
 * I suspect this is faster because we're better exploiting out-of-order execution. The store instructions in
 * the postprocess_8 macro are the only instructions, after the initial mov, that require linear ordering. The CPU
 * is therefore free to schedule all other instructions in the way that maximizes use of its pipelines. */
.macro process_32 v_srcs, idx_s, tmp1, tmp2
	mov \tmp1, \v_srcs[\idx_s]
	ubfx \tmp2, \tmp1, #0, #8
	postprocess_8 \tmp2
	ubfx \tmp2, \tmp1, #8, #8
	postprocess_8 \tmp2
	ubfx \tmp2, \tmp1, #16, #8
	postprocess_8 \tmp2
	ubfx \tmp2, \tmp1, #24, #8
	postprocess_8 \tmp2
.endm

.macro process_64 v_srcs, v_masks, idx_a, idx_b
	mov w10, \v_masks[\idx_a]
	cbnz w10, #12 // 4*(2+1)
	st1 {\v_srcs}[\idx_a], [x16], #4
	b #216 // 4*(53+1)
	process_32 \v_srcs, \idx_a, w13, w14
	mov w19, \v_masks[\idx_b]
	cbnz w19, #12 // 4*(2+1)
	st1 {\v_srcs}[\idx_b], [x16], #4
	b #216 // 4*(53+1)
	process_32 \v_srcs, \idx_b, w13, w14
.endm

.macro process_128 v_src, v_srcd, v_srcs, v_maskd, v_masks
	/* To process 128 bits we must read each half of the mask vector register. Compare instructions only support
	 * scalar registers, so we must move our mask vector into two 64-bit scalar registers.
	 *
	 * Moving values between vector and scalar registers was historically an expensive operation. This changed
	 * with armv8 when SIMD became a requirement, rather than an optional extension hardware vendors could omit
	 * from their designs. */
	mov x10, \v_maskd[0]
	mov x19, \v_maskd[1]

	/* We now check each of the four possible escaping permutations, and branch to handle the relevant case.
	 *
	 * The following compare and branch instructions jump by a relative number, rather than to a label. Using
	 * labels would complicate things because they reference unique, absolute lines of code -- we're inside
	 * a macro that will be inlined into our program at multiple locations.
	 *
	 * Using relative offsets is error-prone and risks memory safety if we do not account for changes when we
	 * refactor our code. So, unless you're a sadist, relative offsets are probably worth avoiding.
	 *
	 * To jump n instructions, we provide an immediate value of 4(n + 1). This value is the number of bytes to
	 * advance the program counter. The 4 multiplier is explained by instructions being 4 bytes long, and we
	 * add 1 because we want the program counter to point at the instruction following the nth. */
	cbnz x10, #16 // 4*(3+1)
	cbnz x19, #1396 // 4*(114+114+114+6+1)

	/* Case 1: no escaping is required, so a single vector store operation writes 128-bits to the output. */
	st1 {\v_src}, [x16], #16
	b #1848 // 4*(114+114+114+114+5+1)

	/* If we know must escape the first 64 bits, we'll check whether to escape the second, and branch if so. */
	cbnz x19, #468 // 4*(114+2+1)

	/* Case 2: the second doesn't need escaping, so we inline a 64 bit version of this macro, to handle the first
	 * 64-bits. We then write only the second 64 bits in a single vector store instruction. */
	process_64 \v_srcs, \v_masks, 0, 1
	st1 {\v_srcd}[1], [x16], #8
	b #1380 // 4*(114+114+114+2+1)

	/* Case 3: we must escape both sides. */
	process_64 \v_srcs, \v_masks, 0, 1
	process_64 \v_srcs, \v_masks, 2, 3
	b #464 // 4*(114+1+1)

	/* Case 4: only the second 64-bits need escaping. */
	st1 {\v_srcd}[0], [x16], #8
	process_64 \v_srcs, \v_masks, 2, 3
.endm

/* '.text' indicates that what follows are machine instructions. */
.text

/* The entry point into an ELF program binary is defined by the location of the '_start' label. */
_start:
	/* When Linux loads our binary, it writes various information to the stack, including references to command
	 * line arguments. Just like in C, the arguments are specified as a count, argc, plus a string array, argv.
	 * These are written to the stack as two values: an integer count and an array of pointers.
	 *
	 * Our binary must be invoked with two arguments -- an input file and an output file. Therefore we first
	 * validate that the count is 3, or exit early with an error message. (Note that argc is always >=1, since
	 * the first argument is the file name of the binary.)
	 *
	 * We load argc into x1, from main memory. */
	ldr x1, [sp]
	validate eq, x1, #3, error_missing_arg, error_missing_arg_len

	/* In aarch64, the stack pointer must be aligned to a 16 byte boundary. It also "grows down" in memory, so
	 * pushing new values means subtracting from the stack pointer, and reading old values means adding to it.
	 *
	 * We modify the stack pointer to reference argv. Since arrays are stored contiguously in memory we can
	 * simply offset argv to read its elements. We do so in increments of 8 bytes, because aarch64's word size is
	 * 64 bits.
	 *
         * argv[0] is in memory at sp + 16 and argv[1] is at sp + 24. If we increment sp by 16, read memory at sp,
	 * increment sp by 8, then read memory at sp again, our program will fail with a memory error. This happens
	 * because the stack pointer wasn't aligned to a 16 byte boundary at the second read. Instead, we offset the
	 * (second) load instruction by 8 bytes rather than the stack pointer. */
	add sp, sp, #16
	ldr x20, [sp]
	ldr x21, [sp, 8]
	sub sp, sp, #16

	/* We now want to open the input file for reading. This operation is beyond the scope of the aarch64
	 * instruction set and is the responsibility of the OS. We must therefore make a system call, where our
	 * program will context switch into the kernel. When the system call completes, execution will switch back to
	 * userspace, and our program resumes from the next instruction.
	 *
	 * In aarch64, making a system call involves populating register x8 with a value that references the syscall
	 * we want to invoke. (These values are defined in the Linux kernel sources). The arguments to the
	 * system call are placed in registers x0 to x7, then we execute a 'svc 0' instruction to context-switch into
	 * the kernel, which handles the onerous syscall specifics.
	 *
	 * The result of our syscall will be returned in register x0. Or, if there's no return value, we can expect
	 * modification to a value referenced in a parameter we passed.
	 *
	 * It's important to remember that the kernel is just another program executing on the processor. Like our
	 * code, uses registers to manipulate data. Only some of our register state is guaranteed to be maintained
	 * when we return from a system call; specifically, only registers x19 to x28. If we want to rely on the state
	 * of any other registers, the onus is on us to store them to the stack.
	 *
	 * Note that this "calling convention" applies to all aarch64 procedure calls, not just system calls.
	 *
	 * Below we're issuing an open() (technically openat()) syscall. If successful, x9 will contain an open file
	 * descriptor. As above, we'll exit with an error otherwise.
	 *
	 * To make the code a little less verbose, and a little easier to read, all system calls used are are
	 * implemented as assembler macros. Wherever we're relying on constants defined in the kernel APIs, like
	 * AT_FDCWD and O_RDONLY, they've been redefined as assembler constants. See the top of this file for full
	 * implementation details. */
	openat #AT_FDCWD, x20, #O_RDONLY
	mov x9, x0
	validate gt, x9, #0, error_read_input, error_read_input_len

	/* In order to allocate an appropriately-sized output file, we need to know the size of the input file.
	 * Again, this is the responsibility of the OS, and thus we must use the stat() (or, technically, fstat())
	 * syscall.
	 *
	 * This syscall is a little different because its return value is written to memory, rather than into register
	 * x0. We need to know more about the size and structure of the returned data before we can invoke a stat()
	 * syscall.
	 *
	 * If we were writing in C, we could just import the struct from the relevant kernel header file, allocate
	 * an instance, and pass a pointer to our instance as a syscall parameter. We can think of a struct as simply
	 * an abstraction for packing data fields into contiguous memory.

	 * A C compiler takes care of turning struct field references into memory address offsets. With some
	 * knowledge of the size of data types on aarch64, we can do the same process manually. We can then directly
	 * reference the memory addresses of the field(s) we care about.
	 *
	 * For stat(), a "struct stat" is written. Its definition in <sys/stat.h> shows that the file size field we
	 * care about is preceded by 7 other fields. We can look at the typedef of each field and determine which
	 * underlying data type is used, then convert these to sizes in bytes, given aarch64 has a 64-bit word.
	 *
	 * This totals 48 bytes. We can provide a clue to anyone reading the code by defining this as assembler
	 * constant 'stat.st_size'. (See the top of this file.)
	 *
	 * Also, the largest stat struct won't be big enough to overflow our stack space, so can just pass the stack
	 * pointer to have kernel stack-allocate the value. This avoids us having to make a prior syscall to allocate
	 * heap memory. */
	fstat x9, sp
	ldr x8,	[sp, #stat.st_size]

	/* We're now going to memory-map the source file, using the mmap() syscall. This offers several benefits:
         *
	 * (1) it simplifies our assembly code, as we can treat our input file as a memory address range, and use
	 *     use the memory load instruction, rather than a multi-instruction syscall invocation, to read the file;
	 * (2) we can read entire input files into memory despite not having enough physical memory; and we let the
	 *     kernel control which memory pages are swapped into & out of physical memory -- since it has a
	 *     greater awareness of the hardware, it can make much more informed decisions;
	 * (3) if another program has mmapped the file, we can share the same pages and de-duplicate memory usage.
	 *
	 * Performance will likely be worse using one mmap() than multiple read() syscall invocations. This is because
	 * there's a large set up cost, and then the potential cost of lazy-loading memory via page fault handling. */
	mmap #0, x8, #PROT_READ, #MAP_SHARED, x9, #0
	mov x7, x0
	validate ne, x7, #-1, error_read_input, error_read_input_len

	/* As we know our memory access pattern will be sequential, we can use the madvise() syscall to provide this
	 * as a hint to the kernel. In theory, this allows the kernel to eagerly load relevant pages and minimize the
	 * performance penalty of page faults. */
	ldr x8, [sp, #stat.st_size]
	madvise x7, x8, #MADV_SEQUENTIAL

	/* Similar to above, let's open our output file for writing, and create it if it doesn't already exist. */
	openat_create #AT_FDCWD, x21, #O_RDWR, #MODE_644
	mov x18, x0
	validate gt, x18, #0, error_dest_file, error_dest_file_len

	/* In the worst case, where every byte must be escaped, our output file will be double our input file size.
	 * We can pre-allocate this space up front, using the fallocate() syscall. This means we'll never have to
	 * handle running out of disk space during encoding.
	 *
	 * What's novel about the implementation of fallocate() is that it allocates sectors of the underlying block
	 * device without significant disk I/O. This means we aren't penalized for allocating 2x our input file size
	 * (despite expecting the average output file to be ~1.04x). */
	ldr x3,	[sp, #stat.st_size]
	add x3, x3, x3
	fallocate x18, #DEFAULT, #0, x3
	validate eq, x0, #0, error_alloc_file, error_alloc_file_len

	ldr x8,	[sp, #stat.st_size]
	add x8, x8, x8

	/* Like with our input file, we're using mmap() to memory-map our output file, and we're using madvise() to
	 * indicate that we'll be writing sequentially.
	 *
	 * We'll use x16 to point to the next byte to write, updating it as we store encoded bytes, and x17 as a
	 * pointer to the start of the file. */
	mmap #0, x8, #PROT_WRITE, #MAP_SHARED, x18, #0
	mov x16, x0
	mov x17, x0
	validate ne, x16, #-1, error_mmap_dest, error_mmap_dest_len

	ldr x8,	[sp, #stat.st_size]
	add x8, x8, x8
	madvise x16, x8, #MADV_SEQUENTIAL

	/* With all that out of the way, we can now implement the yEnc encoder.
	 *
	 * Encoding is very simple. We take a byte of the input, add 42, modulo 256, and if the result is one of four
	 * control characters, we'll need to escape it. Escaping is simply prepending an escape character, '=', and
	 * adding 64, modulo 256.
	 *
	 * For yEnc-encoded binaries intended to be attached to newsgroup messages, there are additional steps. A
	 * line break is required every 128 to 256 characters, and then further escaping is required at the start and
	 * end of a line. Since this is a protocol-specific application of yEnc, albeit the only common application,
	 * we'll consider this out of scope.
	 *
	 * The simplest encoding algorithm would iterate through the input file, encoding byte-by-byte. But this isn't
	 * efficient use of hardware. aarch64 registers are 64 bits wide, yet this approach uses only 8 bits at a
	 * time. Ideally we'd like to pack more data into a register, processing more data per instruction.
	 *
	 * Luckily, "single instruction multiple data" (SIMD) instructions do exactly this. Not only that, but ARM's
	 * implementation supports 128 bit "vector" registers. That means, at the limit, we could process 16x more
	 * data using a SIMD instruction versus a regular scalar instruction.
	 *
	 * There's a complication, though. We can't just read a 128-bit vector, encode, then write a 128-bit vector.
	 * Escaping a byte means inserting a byte, and causing all following bytes to be offset. Worst case, escaping
	 * _every_ byte would mean our 128-bit input vector contributes 256 bits to the output.
	 *
	 * Figuring out how many, and which, bytes of a 16 byte vector could reduce our vectorization gains, if
	 * implemented sequentially.
	 *
	 * A saving grace is that yEnc encoded data is usually first compressed. And that means it's going to be
	 * high entropy, meaning every one of the 256 values of a byte should occur with roughly equal probability.
	 * That means there's a 4/256 (~1.5%) probability of escaping an individual byte.
	 *
	 * If bytes occur independently, and they should under optimal compression, then we can multiply the
	 * the probability for runs of characters. That means a low, 22%, chance of needing to escape one or more
	 * bytes in a 128-bit register. Luckily, the odds are in our favour. */


	/* So, how do SIMD instructions and vector registers work together? When we reference a vector register, we
	 * specify how to interpret its contents. Vector registers are split into "lanes" of fixed size. Since our
	 * application operates on bytes, we have 16 single-byte lanes, and add the ".16b" suffix to the register, as
	 * below.
	 *
	 * Conceptually, a SIMD instruction will perform an operation to every element of the vector register. In
	 * general, SIMD instructions only take vector registers as inputs. If we want to add 42 every element, we
	 * first need to load 42 into each lane of a vector register. */
	movi v0.16b, 42

	/* For convenience, we'll store our escape character '=' (ASCII value 0x3d) in register 15. We happen to have
	 * referred to this register by its 32-bit name 'w15', but it'd be identical behaviour to use the 64-bit 'x15'.
	 * In general, a 32-bit register is simply a view of the lowest 32 bits of a 64-bit register. */
	mov w15, 0x3d

	/* We'll set up a lookup table to help with escaping values later. Here we're loading the address of label
	 * 'escape_lut' (see the .data section below) into register x1, then we're loading 4*128 bits at this address
	 * into four vector registers. */
	ldr x1, =escape_lut
	ld1 {v16.16b, v17.16b, v18.16b, v19.16b}, [x1]

	/* We're going to process the input in blocks of 64 bytes. In order to avoid reading beyond the end of the
	 * input file, we'll stop reading blocks 64 bytes before the end of the input file. */
	ldr x8,	[sp, #stat.st_size]
	add x8, x8, x7
	sub x8, x8, #64

main_loop:
	cmp x7, x8
	b.ge exit_main_loop

	/* Let's read 64 bytes of our input into four 128-bit vector registers in a single instruction. This also
	 * increments x7 by 64, which, by virtue of the cmp instruction above, will mean eventually exiting this
	 * main loop. */
	ld1 {v20.16b, v21.16b, v22.16b, v23.16b}, [x7], #64

	/* Now, add our '42' vector register to each of our input registers. Any values that overflow into 9 bits
	 * after the addition are just truncated, so this acts as modulo 256. */
	add v24.16b, v20.16b, v0.16b
	add v25.16b, v21.16b, v0.16b
	add v26.16b, v22.16b, v0.16b
	add v27.16b, v23.16b, v0.16b

	/* We take each of these vectors and look up each element in our escape table. Each element value is used
	 * as an index to the table, with corresponding values saved into v28-v31. Values are either zero, if the
	 * value did not need escaping, or the escaped value (i.e., the index, plus 64, modulo 256).
	 *
	 * Luckily, all values that need escaping are less than 64. Values that map to indices beyond the last index
	 * of our escape table are just zeroed by the tbl instruction. This has the effect of providing a vector of
	 * escaped values, albeit without the escape character.
	 *
	 * If the vector contains all zeroes, no escaping is necessary. */
	tbl v28.16b, {v16.16b, v17.16b, v18.16b, v19.16b}, v24.16b
	tbl v29.16b, {v16.16b, v17.16b, v18.16b, v19.16b}, v25.16b
	tbl v30.16b, {v16.16b, v17.16b, v18.16b, v19.16b}, v26.16b
	tbl v31.16b, {v16.16b, v17.16b, v18.16b, v19.16b}, v27.16b

	/* Now we take the escaped vector and the offset vector, combine them and write the result to our output file.
	 * To avoid iterating over each byte sequentially, we check whether the escaped vector is equal to zero. If it
	 * is, we can write the entire offset vector to the output in one shot. Remember, there's good odds that 16
	 * bytes of input won't need any escaping. If the escaped vector is not zero, we split both vectors in half
	 * and process each in same way. We'll do this process in a divide and conquer fashion. */
	process_128 v24.16b, v24.d, v24.s, v28.d, v28.s
	process_128 v25.16b, v25.d, v25.s, v29.d, v29.s
	process_128 v26.16b, v26.d, v26.s, v30.d, v30.s
	process_128 v27.16b, v27.d, v27.s, v31.d, v31.s

	/* Loop back, processing the next 64 bytes if necessary. */
	b main_loop

exit_main_loop:
	/* We still have up to 64 trailing bytes that weren't processed by our vectorized loop, so lets process them
	 * next. */
	add x8, x8, #64

trailing_loop:
	cmp x7, x8
	b.eq done

	/* For simplicity, we'll process these trailing bytes one-by-one. */
	ldrb w0, [x7]
	process_8 w0, w20
	add x7, x7, #1
	b trailing_loop
done:
	/* Since we've processed the entire input, we know the size of the output file. Yet our output file is still
	 * double the length of the input file. We now use the ftruncate system call to discard the trailing empty
	 * bytes. */
	mov x1, x16
	sub x1, x1, x17
	ftruncate x18, x1
	exit #0
end:

/* '.data' indicates that what follows are not to be interpreted as instructions. */
.data

error_missing_arg:
	.ascii "ERROR: missing argument\nUsage: ./encoder input_file output_file\n\0"
/* We use the dot operator here to set a constant that is the current address minus the address of the label above. */
.set error_missing_arg_len, . - error_missing_arg

error_read_input:
	.ascii "ERROR: unable to read input file\n\0"
.set error_read_input_len, . - error_read_input

error_dest_file:
	.ascii "ERROR: unable to open output file for writing\n\0"
.set error_dest_file_len, . - error_dest_file

error_alloc_file:
	.ascii "ERROR: unable to allocate enough disk space for output file\n\0"
.set error_alloc_file_len, . - error_alloc_file

error_mmap_dest:
	.ascii "ERROR: unable to memory-map output file\n\0"
.set error_mmap_dest_len, . - error_mmap_dest

escape_lut:
	.8byte 0x0000000000000040
	.8byte 0x00004d00004a0000
	.8byte 0x0000000000000000
	.8byte 0x0000000000000000
	.8byte 0x0000000000000000
	.8byte 0x0000000000000000
	.8byte 0x0000000000000000
	.8byte 0x00007d0000000000
