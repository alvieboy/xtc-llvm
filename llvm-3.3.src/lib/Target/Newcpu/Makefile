##===- lib/Target/Newcpu/Makefile --------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##
LEVEL = ../../..
LIBRARYNAME = LLVMNewcpuCodeGen
TARGET = Newcpu

# Make sure that tblgen is run, first thing.
BUILT_SOURCES = NewcpuGenRegisterInfo.inc NewcpuGenInstrInfo.inc \
		NewcpuGenAsmWriter.inc \
                NewcpuGenDAGISel.inc NewcpuGenAsmMatcher.inc \
                NewcpuGenCodeEmitter.inc NewcpuGenCallingConv.inc \
                NewcpuGenSubtargetInfo.inc NewcpuGenIntrinsics.inc

DIRS = InstPrinter AsmParser Disassembler TargetInfo MCTargetDesc

include $(LEVEL)/Makefile.common

