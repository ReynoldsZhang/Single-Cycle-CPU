/*
 * File: sim4.c
 * Author: Zehang Zhang
 * Description: This file contains the implementation of a basic MIPS CPU simulator.
 *              It simulates a subset of the MIPS instruction set and includes functions for
 *              fetching instructions, decoding, filling control signals, executing ALU operations,
 *              performing memory access, and updating the program counter (PC).
 *              This program aims to provide a simplified model of MIPS CPU functionality.
 */

#include "sim4.h"
#include <string.h>

typedef unsigned char BYTE;  // Define BYTE as unsigned char for simplicity in memory operations

// Global registers to hold results of division operations
WORD LO = 0, HI = 0;

// Function to retrieve an instruction from instruction memory based on the current program counter (PC)
WORD getInstruction(WORD curPC, WORD *instructionMemory) {
    return instructionMemory[curPC / 4];
}

// Function to decode a MIPS instruction and extract its fields into an InstructionFields struct
void extract_instructionFields(WORD instruction, InstructionFields *fieldsOut) {
    fieldsOut->opcode = (instruction >> 26) & 0x3F;          // Extract opcode (bits 26-31)
    fieldsOut->rs = (instruction >> 21) & 0x1F;              // Extract source register (bits 21-25)
    fieldsOut->rt = (instruction >> 16) & 0x1F;              // Extract target register (bits 16-20)
    fieldsOut->rd = (instruction >> 11) & 0x1F;              // Extract destination register (bits 11-15)
    fieldsOut->shamt = (instruction >> 6) & 0x1F;            // Extract shift amount (bits 6-10)
    fieldsOut->funct = instruction & 0x3F;                   // Extract function code (bits 0-5)
    fieldsOut->imm16 = instruction & 0xFFFF;                 // Extract 16-bit immediate (bits 0-15)
    fieldsOut->imm32 = signExtend16to32(fieldsOut->imm16);   // Sign-extend the 16-bit immediate to 32 bits
    fieldsOut->address = instruction & 0x3FFFFFF;            // Extract 26-bit jump address (bits 0-25)
}

// Function to populate the CPU control signals based on the opcode and function fields of the instruction
int fill_CPUControl(InstructionFields *fields, CPUControl *controlOut) {
    memset(controlOut, 0, sizeof(CPUControl));  // Initialize control signals to zero
    switch (fields->opcode) {
        case 0x00: // R-type instruction
            switch (fields->funct) {
                case 0x20: case 0x21:  // add, addu
                    controlOut->ALUsrc = 0;
                    controlOut->ALU.op = 2;
                    controlOut->ALU.bNegate = 0;
                    controlOut->regDst = 1;
                    controlOut->regWrite = 1;
                    break;
                case 0x22: case 0x23:  // sub, subu
                    controlOut->ALUsrc = 0;
                    controlOut->ALU.op = 2;
                    controlOut->ALU.bNegate = 1;
                    controlOut->regDst = 1;
                    controlOut->regWrite = 1;
                    break;
                case 0x24:  // and
                    controlOut->ALUsrc = 0;
                    controlOut->ALU.op = 0;
                    controlOut->regDst = 1;
                    controlOut->regWrite = 1;
                    break;
                case 0x25:  // or
                    controlOut->ALUsrc = 0;
                    controlOut->ALU.op = 1;
                    controlOut->regDst = 1;
                    controlOut->regWrite = 1;
                    break;
                case 0x26:  // xor
                    controlOut->ALUsrc = 0;
                    controlOut->ALU.op = 4;
                    controlOut->regDst = 1;
                    controlOut->regWrite = 1;
                    break;
                case 0x2A:  // slt
                    controlOut->ALUsrc = 0;
                    controlOut->ALU.op = 3;
                    controlOut->ALU.bNegate = 1;
                    controlOut->regDst = 1;
                    controlOut->regWrite = 1;
                    break;
                case 0x1A:  // div
                    controlOut->ALUsrc = 0;
                    controlOut->ALU.op = 5;
                    controlOut->regWrite = 0;  // Division does not write directly to a register
                    break;
                case 0x12:  // mflo
                    controlOut->ALUsrc = 0;
                    controlOut->ALU.op = 6;
                    controlOut->regDst = 1;
                    controlOut->regWrite = 1;
                    break;
                default:
                    return 0;  // Unsupported function code for R-type instructions
            }
            break;
        case 0x08: case 0x09:  // addi, addiu
            controlOut->ALUsrc = 1;
            controlOut->ALU.op = 2;
            controlOut->regDst = 0;
            controlOut->regWrite = 1;
            break;
        case 0x23:  // lw
            controlOut->ALUsrc = 1;
            controlOut->ALU.op = 2;
            controlOut->memRead = 1;
            controlOut->memToReg = 1;
            controlOut->regDst = 0;
            controlOut->regWrite = 1;
            break;
        case 0x2B:  // sw
            controlOut->ALUsrc = 1;
            controlOut->ALU.op = 2;
            controlOut->memWrite = 1;
            break;
        case 0x04:  // beq
            controlOut->ALUsrc = 0;
            controlOut->ALU.op = 2;
            controlOut->ALU.bNegate = 1;
            controlOut->branch = 1;
            controlOut->jump = 0;
            break;
        case 0x02:  // jump
            controlOut->jump = 1;
            controlOut->branch = 0;
            break;
        case 0x0A:  // slti
            controlOut->ALUsrc = 1;
            controlOut->ALU.op = 3;
            controlOut->ALU.bNegate = 1;
            controlOut->regDst = 0;
            controlOut->regWrite = 1;
            break;
        case 0x20:  // lb
            controlOut->ALUsrc = 1;
            controlOut->ALU.op = 8;  // ALU operation for lb instruction
            controlOut->memRead = 1;
            controlOut->memToReg = 1;
            controlOut->regDst = 0;
            controlOut->regWrite = 1;
            break;
        case 0x0F:  // lui
            controlOut->ALUsrc = 1;
            controlOut->ALU.op = 7;
            controlOut->regDst = 0;
            controlOut->regWrite = 1;
            break;
        default:
            return 0;  // Unsupported opcode
    }
    return 1;
}

// Retrieves the first ALU input, which is typically the value of rs
WORD getALUinput1(CPUControl *controlIn, InstructionFields *fieldsIn, WORD rsVal, WORD rtVal, WORD reg32, WORD reg33, WORD oldPC) {
    return rsVal;
}

// Determines the second ALU input based on ALUsrc control signal (immediate or rt value)
WORD getALUinput2(CPUControl *controlIn, InstructionFields *fieldsIn, WORD rsVal, WORD rtVal, WORD reg32, WORD reg33, WORD oldPC) {
    return controlIn->ALUsrc ? fieldsIn->imm32 : rtVal;
}

// Executes the ALU operation as specified by the ALU control signal and stores the result
void execute_ALU(CPUControl *controlIn, WORD input1, WORD input2, ALUResult *aluResultOut) {
    switch (controlIn->ALU.op) {
        case 0: // AND operation
            aluResultOut->result = input1 & input2;
            break;
        case 1: // OR operation
            aluResultOut->result = input1 | input2;
            break;
        case 2: // ADD or SUB operation
            aluResultOut->result = controlIn->ALU.bNegate ? input1 - input2 : input1 + input2;
            break;
        case 3: // SLT (set on less than)
            aluResultOut->result = (input1 < input2) ? 1 : 0;
            break;
        case 4: // XOR operation
            aluResultOut->result = input1 ^ input2;
            break;
        case 5: // DIV operation, updating LO and HI registers
            if (input2 != 0) {
                LO = input1 / input2;
                HI = input1 % input2;
            } else {
                LO = HI = 0;
            }
            aluResultOut->result = 0;
            break;
        case 6: // MFLO operation (move from LO)
            aluResultOut->result = LO;
            break;
        case 7: // LUI operation
            aluResultOut->result = input2 << 16;
            break;
        case 8: // LB operation (loading a byte with sign extension)
            aluResultOut->result = controlIn->ALU.bNegate ? input1 - input2 : input1 + input2;
            break;
        default:
            aluResultOut->result = 0;
    }
    aluResultOut->zero = (aluResultOut->result == 0);
}

// Executes memory operations based on control signals, performing read or write as necessary
void execute_MEM(CPUControl *controlIn, ALUResult *aluResultIn, WORD rsVal, WORD rtVal, WORD *memory, MemResult *resultOut) {
    resultOut->readVal = 0;
    if (controlIn->memRead) {  // Read from memory
        if (controlIn->ALU.op == 8) {  // LB instruction, reading a single byte
            BYTE byteVal = (memory[aluResultIn->result / 4] >> ((aluResultIn->result % 4) * 8)) & 0xFF;
            resultOut->readVal = (int)((char)byteVal);  // Sign-extend the byte to 32 bits
        } else {
            resultOut->readVal = memory[aluResultIn->result / 4];  // Regular word read
        }
    }
    if (controlIn->memWrite) {  // Write to memory
        memory[aluResultIn->result / 4] = rtVal;
    }
}

// Determines the next PC based on control signals, handling branches and jumps
WORD getNextPC(InstructionFields *fields, CPUControl *controlIn, int aluZero, WORD rsVal, WORD rtVal, WORD oldPC) {
    WORD nextPC = oldPC + 4;
    if (controlIn->jump) {
        nextPC = (oldPC & 0xF0000000) | (fields->address << 2);
    } else if (controlIn->branch && aluZero) {
        nextPC += (fields->imm32 << 2);
    }
    return nextPC;
}

// Updates the register file based on control signals, selecting between ALU result or memory read
void execute_updateRegs(InstructionFields *fields, CPUControl *controlIn, ALUResult *aluResultIn, MemResult *memResultIn, WORD *regs) {
    if (controlIn->regWrite) {
        WORD writeData = controlIn->memToReg ? memResultIn->readVal : aluResultIn->result;
        if (controlIn->regDst && fields->rd != 0) {
            regs[fields->rd] = writeData;
        } else if (!controlIn->regDst && fields->rt != 0) {
            regs[fields->rt] = writeData;
        }
    }
}
