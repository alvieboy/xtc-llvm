//===-- XTCTargetMachine.h - Define TargetMachine for XTC -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the XTC specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef MBLAZE_TARGETMACHINE_H
#define MBLAZE_TARGETMACHINE_H

#include "XTCFrameLowering.h"
#include "XTCISelLowering.h"
#include "XTCInstrInfo.h"
#include "XTCIntrinsicInfo.h"
#include "XTCSelectionDAGInfo.h"
#include "XTCSubtarget.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Target/TargetFrameLowering.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
  class formatted_raw_ostream;

  class XTCTargetMachine : public LLVMTargetMachine {
    XTCSubtarget        Subtarget;
    const DataLayout       DL; // Calculates type size & alignment
    XTCInstrInfo        InstrInfo;
    XTCFrameLowering    FrameLowering;
    XTCTargetLowering   TLInfo;
    XTCSelectionDAGInfo TSInfo;
    XTCIntrinsicInfo    IntrinsicInfo;
    InstrItineraryData     InstrItins;

  public:
    XTCTargetMachine(const Target &T, StringRef TT,
                        StringRef CPU, StringRef FS,
                        const TargetOptions &Options,
                        Reloc::Model RM, CodeModel::Model CM,
                        CodeGenOpt::Level OL);

    virtual const XTCInstrInfo *getInstrInfo() const
    { return &InstrInfo; }

    virtual const InstrItineraryData *getInstrItineraryData() const
    {  return &InstrItins; }

    virtual const TargetFrameLowering *getFrameLowering() const
    { return &FrameLowering; }

    virtual const XTCSubtarget *getSubtargetImpl() const
    { return &Subtarget; }

    virtual const DataLayout *getDataLayout() const
    { return &DL;}

    virtual const XTCRegisterInfo *getRegisterInfo() const
    { return &InstrInfo.getRegisterInfo(); }

    virtual const XTCTargetLowering *getTargetLowering() const
    { return &TLInfo; }

    virtual const XTCSelectionDAGInfo* getSelectionDAGInfo() const
    { return &TSInfo; }

    const TargetIntrinsicInfo *getIntrinsicInfo() const
    { return &IntrinsicInfo; }

    // Pass Pipeline Configuration
    virtual TargetPassConfig *createPassConfig(PassManagerBase &PM);
  };
} // End llvm namespace

#endif
