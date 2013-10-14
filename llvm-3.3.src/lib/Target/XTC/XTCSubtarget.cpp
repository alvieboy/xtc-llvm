//===-- XTCSubtarget.cpp - XTC Subtarget Information ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the XTC specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "XTCSubtarget.h"
#include "XTC.h"
#include "XTCRegisterInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "XTCGenSubtargetInfo.inc"

using namespace llvm;

XTCSubtarget::XTCSubtarget(const std::string &TT,
                                 const std::string &CPU,
                                 const std::string &FS):
  XTCGenSubtargetInfo(TT, CPU, FS),
    HasMul(false)
{
  // Parse features string.
  std::string CPUName = CPU;
  if (CPUName.empty())
    CPUName = "xtc";
  ParseSubtargetFeatures(CPUName, FS);

  // Only use instruction scheduling if the selected CPU has an instruction
  // itinerary (the default CPU is the only one that doesn't).
}

bool XTCSubtarget::
enablePostRAScheduler(CodeGenOpt::Level OptLevel,
                      TargetSubtargetInfo::AntiDepBreakMode& Mode,
                      RegClassVector& CriticalPathRCs) const {
    /*
     Mode = TargetSubtargetInfo::ANTIDEP_CRITICAL;
     CriticalPathRCs.clear();
     CriticalPathRCs.push_back(&XTC::GPRegsRegClass);
     return OptLevel >= CodeGenOpt::Default;
     */
    Mode = TargetSubtargetInfo::ANTIDEP_NONE;
    return OptLevel >= CodeGenOpt::Default;
}
