//===-- XTCMCAsmInfo.cpp - XTC asm properties -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the XTCMCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "XTCMCAsmInfo.h"
using namespace llvm;

void XTCMCAsmInfo::anchor() { }

XTCMCAsmInfo::XTCMCAsmInfo() {
  IsLittleEndian              = false;
  StackGrowsUp                = false;
  SupportsDebugInformation    = true;
  AlignmentIsInBytes          = false;
  PrivateGlobalPrefix         = "$";
  GPRel32Directive            = "\t.gpword\t";
  UsesELFSectionDirectiveForBSS = true;
}
