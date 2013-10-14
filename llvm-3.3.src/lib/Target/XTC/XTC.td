//===-- MBlaze.td - Describe the MBlaze Target Machine -----*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This is the top level entry point for the XTC target.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "XTCRegisterInfo.td"
include "XTCSchedule.td"
include "XTCIntrinsics.td"
include "XTCInstrInfo.td"
include "XTCCallingConv.td"

def XTCInstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// XTC Subtarget features                                              //
//===----------------------------------------------------------------------===//

def FeatureMul         : SubtargetFeature<"mul", "HasMul", "true",
                                "Implements hardware multiplier">;

//===----------------------------------------------------------------------===//
// XTC processors supported.
//===----------------------------------------------------------------------===//

def : Processor<"xtc", XTCPipe5Itineraries, []>;

//===----------------------------------------------------------------------===//
// Instruction Descriptions
//===----------------------------------------------------------------------===//

def XTCAsmWriter : AsmWriter {
  string AsmWriterClassName  = "InstPrinter";
  bit isMCAsmWriter = 1;
}

//===----------------------------------------------------------------------===//
// Target Declaration
//===----------------------------------------------------------------------===//

def XTC : Target {
  let InstructionSet = XTCInstrInfo;
  let AssemblyWriters = [XTCAsmWriter];
}
