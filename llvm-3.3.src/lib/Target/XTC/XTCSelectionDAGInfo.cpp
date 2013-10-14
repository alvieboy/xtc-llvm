//===-- XTCSelectionDAGInfo.cpp - XTC SelectionDAG Info -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the XTCSelectionDAGInfo class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "xtc-selectiondag-info"
#include "XTCTargetMachine.h"
using namespace llvm;

XTCSelectionDAGInfo::XTCSelectionDAGInfo(const XTCTargetMachine &TM)
  : TargetSelectionDAGInfo(TM) {
}

XTCSelectionDAGInfo::~XTCSelectionDAGInfo() {
}
