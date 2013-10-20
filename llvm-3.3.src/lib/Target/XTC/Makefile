##===- lib/Target/XTC/Makefile --------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##
LEVEL = ../../..
LIBRARYNAME = LLVMXTCCodeGen
TARGET = XTC

# Make sure that tblgen is run, first thing.
BUILT_SOURCES = XTCGenRegisterInfo.inc XTCGenInstrInfo.inc \
		XTCGenAsmWriter.inc \
                XTCGenDAGISel.inc XTCGenAsmMatcher.inc \
                XTCGenCodeEmitter.inc XTCGenCallingConv.inc \
                XTCGenSubtargetInfo.inc

DIRS = InstPrinter AsmParser Disassembler TargetInfo MCTargetDesc

include $(LEVEL)/Makefile.common

