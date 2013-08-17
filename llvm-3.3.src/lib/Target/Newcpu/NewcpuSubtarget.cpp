//===-- NewcpuSubtarget.cpp - Newcpu Subtarget Information ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Newcpu specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "NewcpuSubtarget.h"
#include "Newcpu.h"
#include "NewcpuRegisterInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "NewcpuGenSubtargetInfo.inc"

using namespace llvm;

NewcpuSubtarget::NewcpuSubtarget(const std::string &TT,
                                 const std::string &CPU,
                                 const std::string &FS):
  NewcpuGenSubtargetInfo(TT, CPU, FS),
    HasMul(false)
{
  // Parse features string.
  std::string CPUName = CPU;
  if (CPUName.empty())
    CPUName = "newcpu";
  ParseSubtargetFeatures(CPUName, FS);

  // Only use instruction scheduling if the selected CPU has an instruction
  // itinerary (the default CPU is the only one that doesn't).
}

bool NewcpuSubtarget::
enablePostRAScheduler(CodeGenOpt::Level OptLevel,
                      TargetSubtargetInfo::AntiDepBreakMode& Mode,
                      RegClassVector& CriticalPathRCs) const {
    /*
     Mode = TargetSubtargetInfo::ANTIDEP_CRITICAL;
     CriticalPathRCs.clear();
     CriticalPathRCs.push_back(&Newcpu::GPRegsRegClass);
     return OptLevel >= CodeGenOpt::Default;
     */
    Mode = TargetSubtargetInfo::ANTIDEP_NONE;
    return OptLevel >= CodeGenOpt::Default;
}
