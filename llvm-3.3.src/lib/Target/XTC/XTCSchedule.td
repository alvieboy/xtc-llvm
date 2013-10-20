//===-- XTCSchedule.td - XTC Scheduling Definitions --*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// XTC functional units.
//===----------------------------------------------------------------------===//
def Fetch : FuncUnit;
def Decode : FuncUnit;
def Data :   FuncUnit;
def Execute : FuncUnit;
def Memory : FuncUnit;
def WriteBack : FuncUnit;

//===----------------------------------------------------------------------===//
// Instruction Itinerary classes used for XTC
//===----------------------------------------------------------------------===//
def IIC_ALU1   : InstrItinClass;
def IIC_ALUm   : InstrItinClass;
def IIC_ALUd   : InstrItinClass;
def IIC_SHT    : InstrItinClass;
def IIC_MEMs   : InstrItinClass;
def IIC_MEMl   : InstrItinClass;
def IIC_BR     : InstrItinClass;
def IIC_BRc    : InstrItinClass;
def IIC_BRl    : InstrItinClass;
def IIC_Pseudo : InstrItinClass;

//===----------------------------------------------------------------------===//
// XTC instruction itineraries for five stage pipeline.
//===----------------------------------------------------------------------===//
include "XTCSchedule5.td"