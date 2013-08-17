//===-- NewcpuSelectionDAGInfo.cpp - Newcpu SelectionDAG Info -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the NewcpuSelectionDAGInfo class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "newcpu-selectiondag-info"
#include "NewcpuTargetMachine.h"
using namespace llvm;

NewcpuSelectionDAGInfo::NewcpuSelectionDAGInfo(const NewcpuTargetMachine &TM)
  : TargetSelectionDAGInfo(TM) {
}

NewcpuSelectionDAGInfo::~NewcpuSelectionDAGInfo() {
}
