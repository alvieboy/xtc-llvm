//===-- MBlaze.td - Describe the MBlaze Target Machine -----*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This is the top level entry point for the Newcpu target.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "NewcpuRegisterInfo.td"
include "NewcpuSchedule.td"
include "NewcpuIntrinsics.td"
include "NewcpuInstrInfo.td"
include "NewcpuCallingConv.td"

def NewcpuInstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// Newcpu Subtarget features                                              //
//===----------------------------------------------------------------------===//

def FeatureMul         : SubtargetFeature<"mul", "HasMul", "true",
                                "Implements hardware multiplier">;

//===----------------------------------------------------------------------===//
// Newcpu processors supported.
//===----------------------------------------------------------------------===//

def : Processor<"newcpu", NewcpuPipe5Itineraries, []>;

//===----------------------------------------------------------------------===//
// Instruction Descriptions
//===----------------------------------------------------------------------===//

def NewcpuAsmWriter : AsmWriter {
  string AsmWriterClassName  = "InstPrinter";
  bit isMCAsmWriter = 1;
}

//===----------------------------------------------------------------------===//
// Target Declaration
//===----------------------------------------------------------------------===//

def Newcpu : Target {
  let InstructionSet = NewcpuInstrInfo;
  let AssemblyWriters = [NewcpuAsmWriter];
}
