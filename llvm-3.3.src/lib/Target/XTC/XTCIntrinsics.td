//===-- IntrinsicsXTC.td - Defines XTC intrinsics ----*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines all of the MicroBlaze-specific intrinsics.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Definitions for all XTC intrinsics.
//

// XTC intrinsic classes.
let TargetPrefix = "XTC", isTarget = 1 in {
  class MBFSL_Get_Intrinsic : Intrinsic<[llvm_i32_ty], [llvm_i32_ty], []>;

  class MBFSL_Put_Intrinsic : Intrinsic<[], [llvm_i32_ty, llvm_i32_ty], []>;

  class MBFSL_PutT_Intrinsic : Intrinsic<[], [llvm_i32_ty], []>;
}

