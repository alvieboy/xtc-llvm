//===- XTCCallingConv.td - Calling Conventions for XTC -*- tablegen -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This describes the calling conventions for XTC architecture.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// XTC ABI Calling Convention
//===----------------------------------------------------------------------===//

def RetCC_XTC : CallingConv<[
  // i32 are returned in registers R1 and R13
  CCIfType<[i32], CCAssignToReg<[r1, r13]>>
]>;

def CC_XTC : CallingConv<[
  //CCIfType<[i32], CCCustom<"CC_XTC_AssignReg">>,
  CCIfType<[i32], CCAssignToReg<[r2, r3, r4]>>,
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;
