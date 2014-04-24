//===-- XTCRegisterInfo.td - MBlaze Register defs -------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the XTC register file
//===----------------------------------------------------------------------===//

// We have banks of 8 registers each.
class XTCReg<string n> : Register<n> {
  field bits<4> Num;
  let Namespace = "XTC";
}
// Special purpose registers
class XTCSReg<string n> : Register<n> {
  field bits<3> Num;
  let Namespace = "XTC";
}

// XTC general purpose registers
class GPR<bits<4> num, string n> : XTCReg<n> {
  let Num = num;
}

// XTC special purpose
class SPR<bits<3> num, string n> : XTCSReg<n> {
  let Num = num;
}

let Namespace = "XTC" in {
  // General Purpose Registers
  def r0  : GPR< 0,  "r0">,   DwarfRegNum<[0]>;
  def r1  : GPR< 1,  "r1">,   DwarfRegNum<[1]>;
  def r2  : GPR< 2,  "r2">,   DwarfRegNum<[2]>;
  def r3  : GPR< 3,  "r3">,   DwarfRegNum<[3]>;
  def r4  : GPR< 4,  "r4">,   DwarfRegNum<[4]>;
  def r5  : GPR< 5,  "r5">,   DwarfRegNum<[5]>;
  def r6  : GPR< 6,  "r6">,   DwarfRegNum<[6]>;
  def r7  : GPR< 7,  "r7">,   DwarfRegNum<[7]>;

  def r8  : GPR< 8,  "r8">,   DwarfRegNum<[8]>;
  def r9  : GPR< 9,  "r9">,   DwarfRegNum<[9]>;
  def r10 : GPR< 10,  "r10">,   DwarfRegNum<[10]>;
  def r11 : GPR< 11,  "r11">,   DwarfRegNum<[11]>;
  def r12 : GPR< 12,  "r12">,   DwarfRegNum<[12]>;
  def r13 : GPR< 13,  "r13">,   DwarfRegNum<[13]>;
  def r14 : GPR< 14,  "r14">,   DwarfRegNum<[14]>;
  def r15 : GPR< 15,  "r15">,   DwarfRegNum<[15]>;
  
  // Other regs
  def Y    : SPR<2, "y">,    DwarfRegNum<[16]>;
  def PSR  : SPR<3, "psr">,  DwarfRegNum<[17]>; /* PSR */
  def SPSR : SPR<4, "spsr">, DwarfRegNum<[18]>;
  def TPC  : SPR<5, "tpc">,  DwarfRegNum<[19]>;
}

def GPRegs : RegisterClass<"XTC", [i32], 32, (sequence "r%u", 0, 15)>;
def SPRegs : RegisterClass<"XTC", [i32], 32, (add Y, PSR, SPSR, TPC)> { let isAllocatable = 0; }
