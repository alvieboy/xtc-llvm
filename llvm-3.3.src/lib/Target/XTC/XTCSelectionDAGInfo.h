//===-- XTCSelectionDAGInfo.h - XTC SelectionDAG Info -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the XTC subclass for TargetSelectionDAGInfo.
//
//===----------------------------------------------------------------------===//

#ifndef XTCSELECTIONDAGINFO_H
#define XTCSELECTIONDAGINFO_H

#include "llvm/Target/TargetSelectionDAGInfo.h"

namespace llvm {

class XTCTargetMachine;

class XTCSelectionDAGInfo : public TargetSelectionDAGInfo {
public:
  explicit XTCSelectionDAGInfo(const XTCTargetMachine &TM);
  ~XTCSelectionDAGInfo();
};

}

#endif
