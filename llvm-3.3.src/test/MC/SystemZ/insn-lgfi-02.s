# RUN: not llvm-mc -triple s390x-linux-gnu < %s 2> %t
# RUN: FileCheck < %t %s

#CHECK: error: invalid operand
#CHECK: lgfi	%r0, (-1 << 31) - 1
#CHECK: error: invalid operand
#CHECK: lgfi	%r0, (1 << 31)

	lgfi	%r0, (-1 << 31) - 1
	lgfi	%r0, (1 << 31)
