//===-- NewcpuSelectionDAGInfo.h - Newcpu SelectionDAG Info -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the Newcpu subclass for TargetSelectionDAGInfo.
//
//===----------------------------------------------------------------------===//

#ifndef NEWCPUSELECTIONDAGINFO_H
#define NEWCPUSELECTIONDAGINFO_H

#include "llvm/Target/TargetSelectionDAGInfo.h"

namespace llvm {

class NewcpuTargetMachine;

class NewcpuSelectionDAGInfo : public TargetSelectionDAGInfo {
public:
  explicit NewcpuSelectionDAGInfo(const NewcpuTargetMachine &TM);
  ~NewcpuSelectionDAGInfo();
};

}

#endif
