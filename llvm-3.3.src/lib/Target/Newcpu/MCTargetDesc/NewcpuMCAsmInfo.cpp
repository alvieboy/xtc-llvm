//===-- NewcpuMCAsmInfo.cpp - Newcpu asm properties -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the NewcpuMCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "NewcpuMCAsmInfo.h"
using namespace llvm;

void NewcpuMCAsmInfo::anchor() { }

NewcpuMCAsmInfo::NewcpuMCAsmInfo() {
  IsLittleEndian              = false;
  StackGrowsUp                = false;
  SupportsDebugInformation    = true;
  AlignmentIsInBytes          = false;
  PrivateGlobalPrefix         = "$";
  GPRel32Directive            = "\t.gpword\t";
}
