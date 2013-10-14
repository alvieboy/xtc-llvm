//===-- XTCIntrinsicInfo.cpp - Intrinsic Information -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the XTC implementation of TargetIntrinsicInfo.
//
//===----------------------------------------------------------------------===//

#include "XTCIntrinsicInfo.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cstring>

using namespace llvm;

namespace XTCIntrinsic {

  enum ID {
    last_non_xtc_intrinsic = Intrinsic::num_intrinsics-1,
#define GET_INTRINSIC_ENUM_VALUES
#include "XTCGenIntrinsics.inc"
#undef GET_INTRINSIC_ENUM_VALUES
    , num_xtc_intrinsics
  };

#define GET_LLVM_INTRINSIC_FOR_GCC_BUILTIN
#include "XTCGenIntrinsics.inc"
#undef GET_LLVM_INTRINSIC_FOR_GCC_BUILTIN
}

std::string XTCIntrinsicInfo::getName(unsigned IntrID, Type **Tys,
                                         unsigned numTys) const {
  static const char *const names[] = {
#define GET_INTRINSIC_NAME_TABLE
#include "XTCGenIntrinsics.inc"
#undef GET_INTRINSIC_NAME_TABLE
  };

  assert(!isOverloaded(IntrID) && "XTC intrinsics are not overloaded");
  if (IntrID < Intrinsic::num_intrinsics)
    return 0;
  assert(IntrID < XTCIntrinsic::num_xtc_intrinsics &&
         "Invalid intrinsic ID");

  std::string Result(names[IntrID - Intrinsic::num_intrinsics]);
  return Result;
}

unsigned XTCIntrinsicInfo::
lookupName(const char *Name, unsigned Len) const {
  if (Len < 5 || Name[4] != '.' || Name[0] != 'l' || Name[1] != 'l'
      || Name[2] != 'v' || Name[3] != 'm')
    return 0;  // All intrinsics start with 'llvm.'

#define GET_FUNCTION_RECOGNIZER
#include "XTCGenIntrinsics.inc"
#undef GET_FUNCTION_RECOGNIZER
  return 0;
}

unsigned XTCIntrinsicInfo::
lookupGCCName(const char *Name) const {
    return XTCIntrinsic::getIntrinsicForGCCBuiltin("xtc",Name);
}

bool XTCIntrinsicInfo::isOverloaded(unsigned IntrID) const {
  if (IntrID == 0)
    return false;

  unsigned id = IntrID - Intrinsic::num_intrinsics + 1;
#define GET_INTRINSIC_OVERLOAD_TABLE
#include "XTCGenIntrinsics.inc"
#undef GET_INTRINSIC_OVERLOAD_TABLE
}

/// This defines the "getAttributes(LLVMContext &C, ID id)" method.
#define GET_INTRINSIC_ATTRIBUTES
#include "XTCGenIntrinsics.inc"
#undef GET_INTRINSIC_ATTRIBUTES

static FunctionType *getType(LLVMContext &Context, unsigned id) {
  Type *ResultTy = NULL;
  SmallVector<Type*, 8> ArgTys;
  bool IsVarArg = false;

#define GET_INTRINSIC_GENERATOR
#include "XTCGenIntrinsics.inc"
#undef GET_INTRINSIC_GENERATOR

  return FunctionType::get(ResultTy, ArgTys, IsVarArg);
}

Function *XTCIntrinsicInfo::getDeclaration(Module *M, unsigned IntrID,
                                                Type **Tys,
                                                unsigned numTy) const {
  assert(!isOverloaded(IntrID) && "XTC intrinsics are not overloaded");
  AttributeSet AList = getAttributes(M->getContext(),
                                    (XTCIntrinsic::ID) IntrID);
  return cast<Function>(M->getOrInsertFunction(getName(IntrID),
                                               getType(M->getContext(), IntrID),
                                               AList));
}
