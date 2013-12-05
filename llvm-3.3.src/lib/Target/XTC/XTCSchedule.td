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
def ExecuteA1 : FuncUnit;
def ExecuteA2 : FuncUnit;
def Memory : FuncUnit;
def WriteBack : FuncUnit;

//===----------------------------------------------------------------------===//
// Instruction Itinerary classes used for XTC
//===----------------------------------------------------------------------===//
def IIC_ALU1   : InstrItinClass;
def IIC_ALU2   : InstrItinClass;
def IIC_ALU2MEM: InstrItinClass;
def IIC_Pseudo : InstrItinClass;

//===----------------------------------------------------------------------===//
// XTC instruction itineraries for three stage pipeline.
//===----------------------------------------------------------------------===//
include "XTCSchedule3.td"
