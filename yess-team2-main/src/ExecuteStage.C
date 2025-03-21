#include <cstdint>
#include "PipeRegArray.h"
#include "ExecuteStage.h"
#include "E.h"
#include "M.h"
#include "W.h"
#include <Instruction.h>
#include "Tools.h"
#include "Status.h"

/*
 * doClockLow
 *
 * Performs the Fetch stage combinational logic that is performed when
 * the clock edge is low.
 *
 * @param: pipeRegs - array of the pipeline register 
                      (F, D, E, M, W instances)
 */
bool ExecuteStage::doClockLow(PipeRegArray *pipeRegs) {
    PipeReg *ereg = pipeRegs->getExecuteReg();
    PipeReg *mreg = pipeRegs->getMemoryReg();
    PipeReg *wreg = pipeRegs->getWritebackReg();

    uint64_t stat = ereg->get(E_STAT);
    uint64_t iCode = ereg->get(E_ICODE);
    uint64_t iFun = ereg->get(E_IFUN);
    uint64_t valA = ereg->get(E_VALA);
    uint64_t dstM = ereg->get(E_DSTM);

    
    
    bool set_cc = setcc(iCode, wreg);

    uint64_t aluA = ExecuteStage::setaluA(iCode, ereg);
    uint64_t aluB = ExecuteStage::setaluB(iCode, ereg);
    uint64_t alufun = ExecuteStage::alufun(iCode, ereg);

    Stage::e_Cnd = cond(iCode, iFun);

    Stage::e_dstE = ExecuteStage::setdstE(iCode, e_Cnd, ereg);
    ExecuteStage::CC(set_cc, alufun, aluA, aluB);
    Stage::e_valE = ExecuteStage::ALU(alufun, aluA, aluB);

    ExecuteStage::setMInput(mreg, stat, iCode, valA, dstM, e_dstE, e_Cnd, e_valE);

    calculateControlSignals(pipeRegs);

    return false;
}

/* 
 * doClockHigh
 *
 * Applies the appropriate control signal to the M register instance.
 * 
 * @param: pipeRegs - array of the pipeline register (F, D, E, M, W instances)
 */
void ExecuteStage::doClockHigh(PipeRegArray *pipeRegs)
{
    PipeReg *mreg = pipeRegs->getMemoryReg();
    if (M_bubble) 
    {
        ((M *)mreg)->bubble();
    } 
    else 
    {
        mreg->normal();
    }
}

/* setMInput
 *
 * Updates the input values for the memory stage (M) by setting the corresponding fields in the M pipeline register.
 *
 * @param mreg - Pointer to the memory stage pipeline register.
 * @param stat - The status of the instruction (e.g., OK, error, etc.).
 * @param icode - The instruction code (type of operation being executed).
 * @param valA - The value of the A register to be forwarded to the memory stage.
 * @param dstM - The destination register for memory operations.
 * @param dstE - The destination register for execution stage results.
 * @param Cnd - The condition flag indicating if a conditional branch is taken.
 * @param valE - The result of the ALU computation in the execute stage.
 *
 */
void ExecuteStage::setMInput(PipeReg *mreg, uint64_t stat, uint64_t icode,
                                uint64_t valA, uint64_t dstM, uint64_t dstE,
                                uint64_t Cnd, uint64_t valE)
{
    mreg->set(M_STAT, stat);
    mreg->set(M_ICODE, icode);
    mreg->set(M_VALA, valA);
    mreg->set(M_DSTM, dstM);
    mreg->set(M_DSTE, dstE);
    mreg->set(M_CND, Cnd);
    mreg->set(M_VALE, valE);
}


/* setaluA
 *
 * Determines the appropriate value to be used as the `aluA` input for the ALU (Arithmetic Logic Unit) 
 * in the execute stage based on the instruction code and pipeline register values.
 *
 * @param iCode - The instruction code indicating the type of operation being executed.
 * @param ereg - Pointer to the execute stage pipeline register.
 *
 * @return The value to be used as the `aluA` input for the ALU:
 *         - For `IRRMOVQ` or `IOPQ`, returns the value from `E_VALA`.
 *         - For `IIRMOVQ`, `IRMMOVQ`, `IMRMOVQ`, or `IADDQ`, returns the value from `E_VALC`.
 *         - For `ICALL` or `IPUSHQ`, returns `-8` (adjust stack pointer down).
 *         - For `IRET` or `IPOPQ`, returns `8` (adjust stack pointer up).
 *         - For all other instructions, returns `0` as the default.
 *
 */
uint64_t ExecuteStage::setaluA(uint64_t iCode, PipeReg * ereg)
{
    if(iCode == Instruction::IRRMOVQ || iCode == Instruction:: IOPQ)
    {
        return ereg->get(E_VALA);
    }
    else if(iCode == Instruction:: IIRMOVQ || iCode == Instruction:: IRMMOVQ
                || iCode == Instruction::IMRMOVQ || iCode == Instruction::IADDQ)
    {
        return ereg->get(E_VALC);
    }
    else if(iCode == Instruction::ICALL || iCode == Instruction:: IPUSHQ)
    {
        return -8;
    }
    else if(iCode == Instruction::IRET || iCode == Instruction::IPOPQ)
    {
        return 8;
    }
    else 
    {
        return 0;
    }
}



/* setaluB
 *
 * Determines the appropriate value to be used as the `aluB` input for the ALU (Arithmetic Logic Unit)
 * in the execute stage based on the instruction code and pipeline register values.
 *
 * @param iCode - The instruction code indicating the type of operation being executed.
 * @param ereg - Pointer to the execute stage pipeline register.
 *
 * @return The value to be used as the `aluB` input for the ALU:
 *         - For `IRMMOVQ`, `IMRMOVQ`, `IOPQ`, `ICALL`, `IPUSHQ`, `IRET`, `IPOPQ`, or `IADDQ`, 
 *           returns the value from `E_VALB`.
 *         - For `IRRMOVQ` or `IIRMOVQ`, returns `0` (default operand for these instructions).
 *         - For all other instructions (not explicitly listed), returns `0` as a fallback default.
 */
uint64_t ExecuteStage::setaluB(uint64_t iCode, PipeReg * ereg)
{
    if(iCode == Instruction::IRMMOVQ || iCode == Instruction::IMRMOVQ || iCode == Instruction::IOPQ 
         || iCode == Instruction::ICALL || iCode == Instruction::IPUSHQ || iCode == Instruction::IRET
            || iCode == Instruction::IPOPQ || iCode == Instruction::IADDQ)
    {
        return ereg->get(E_VALB);
    }
    else if(iCode == Instruction::IRRMOVQ || iCode == Instruction::IIRMOVQ)
    {
        return 0;
    }
    else
    {
        return 0;
    }
}



/* alufun
 *
 * Determines the ALU operation to be performed during the execute stage based on the instruction code.
 *
 * @param iCode - The instruction code indicating the type of operation being executed.
 * @param ereg - Pointer to the execute stage pipeline register.
 *
 * @return The ALU function to perform:
 *         - If `iCode` is `Instruction::IOPQ`, retrieves the function code from the pipeline register's 
 *           `E_IFUN` field to specify the ALU operation.
 *         - For all other instructions, returns `Instruction::ADDQ` (default ALU operation for addition).
 *
 */
uint64_t ExecuteStage::alufun(uint64_t iCode, PipeReg * ereg)
{
    if(iCode == Instruction::IOPQ)
    {
        return ereg->get(E_IFUN);
    }
    else
    {
        return Instruction::ADDQ;
    }
}


/* setdstE
 *
 * Determines the appropriate destination register for the execute stage.
 * 
 * @param: iCode - The instruction code indicating the type of operation.
 * @param: Cnd - The condition flag indicating whether the instruction is conditional.
 * @param: ereg - Pointer to the execute pipeline register holding intermediate values.
 * 
 * @return: The destination register for the execute stage, either `RegisterFile::RNONE` 
 *          or the value of `E_DSTE` from the execute pipeline register.
 */
uint64_t ExecuteStage::setdstE(uint64_t iCode, uint64_t Cnd, PipeReg * ereg)
{
    if (iCode == Instruction::IRRMOVQ && !Cnd)
    {
        return RegisterFile::RNONE; 
    }
    else
    {
        return ereg->get(E_DSTE); 
    }
}


/* CC
 *
 * Sets the condition codes (CC) based on the result of an ALU operation.
 * The function checks the ALU operation type (alufun) and updates the 
 * condition codes (`OF`, `SF`, `ZF`) accordingly.
 * 
 * The condition codes represent the following:
 * - OF: Overflow flag (set if an overflow occurs during the operation).
 * - SF: Sign flag (set based on the sign of the result).
 * - ZF: Zero flag (set if the result is zero).
 * 
 * @param: set_cc - Flag indicating whether the condition codes should be set.
 * @param: alufun - The ALU operation function, determining the operation type (ADDQ, SUBQ, etc.).
 * @param: aluA - The first operand to the ALU operation.
 * @param: aluB - The second operand to the ALU operation.
 * 
 * This function updates the condition codes stored in the `Stage::cc` object.
 */
void ExecuteStage::CC(uint64_t set_cc, uint64_t alufun, uint64_t aluA, uint64_t aluB)
{
    bool error;
    if(!set_cc)
    {
        return;
    }
    switch(alufun)
    {
        case Instruction::ADDQ:
        {
            Stage::cc->setConditionCode(Tools::addOverflow(aluA, aluB), ConditionCodes::OF, error);
            Stage::cc->setConditionCode(Tools::sign(aluA + aluB), ConditionCodes::SF, error);
            Stage::cc->setConditionCode(aluA + aluB == 0, ConditionCodes::ZF, error);
            break;
        }
        case Instruction::SUBQ:
        {
            Stage::cc->setConditionCode(Tools::subOverflow(aluA, aluB), ConditionCodes::OF, error);
            Stage::cc->setConditionCode(Tools::sign(aluB - aluA), ConditionCodes::SF, error);
            Stage::cc->setConditionCode(aluA - aluB == 0, ConditionCodes::ZF, error);
            break;
        }
        case Instruction::ANDQ:
        {
            Stage::cc->setConditionCode(false, ConditionCodes::OF, error);
            Stage::cc->setConditionCode(Tools::sign(aluA & aluB), ConditionCodes::SF, error);
            Stage::cc->setConditionCode((aluA & aluB) == 0, ConditionCodes::ZF, error);
            break;
        }
        case Instruction::XORQ:
        {
            Stage::cc->setConditionCode(false, ConditionCodes::OF, error);
            Stage::cc->setConditionCode(Tools::sign(aluA ^ aluB), ConditionCodes::SF, error);
            Stage::cc->setConditionCode((aluA ^ aluB) == 0, ConditionCodes::ZF, error);
            break;
        }
    }
}


/* ALU
 *
 * Performs the ALU (Arithmetic Logic Unit) operation based on the provided ALU function type (alufun).
 * The function performs one of four operations: addition, subtraction, bitwise AND, or bitwise XOR.
 *
 *
 * @param: alufun - The ALU operation type (ADDQ, SUBQ, ANDQ, XORQ).
 * @param: aluA - The first operand for the ALU operation.
 * @param: aluB - The second operand for the ALU operation.
 * 
 * @return: The result of the ALU operation.
 */
uint64_t ExecuteStage::ALU(uint64_t alufun, uint64_t aluA, uint64_t aluB)
{
    switch(alufun)
    {
        case Instruction::ADDQ:
        {
            return (aluA + aluB);
        }
        case Instruction::SUBQ:
        {
            return (aluB - aluA);
        }
        case Instruction::ANDQ:
        {
            return (aluA & aluB);
        }
        case Instruction::XORQ:
        {
            return (aluA ^ aluB);
        }
    }
    return 0;
}


/* cond
 *
 * Evaluates the condition based on the provided instruction and condition function (ifun).
 * This function is used to determine if a jump or conditional move should be taken
 * based on the flags stored in the condition codes register (SF, OF, ZF).
 *
 *
 * @param: icode - The instruction code indicating the type of instruction.
 * @param: ifun - The function code specifying the condition to check (EQUAL, NOTEQUAL, etc.).
 * 
 * @return: Returns a boolean (1 for true, 0 for false) indicating whether the condition is satisfied.
 */
uint64_t ExecuteStage::cond(uint64_t icode, uint64_t ifun) 
{
    if (icode != Instruction::IJXX && icode != Instruction::ICMOVXX) 
    {
        return 0;
    }

    bool error;
    bool sf = Stage::cc->getConditionCode(ConditionCodes::SF, error);
    bool of = Stage::cc->getConditionCode(ConditionCodes::OF, error);
    bool zf = Stage::cc->getConditionCode(ConditionCodes::ZF, error);
    
    switch (ifun) 
    {
        case Instruction::EQUAL:
            return zf;
        case Instruction::NOTEQUAL:
            return !zf;
        case Instruction::LESS:
            return (sf ^ of);
        case Instruction::LESSEQ:
            return (sf ^ of) | zf;
        case Instruction::GREATER:
            return !(sf ^ of) & !zf;
        case Instruction::GREATEREQ:
            return !(sf ^ of);
        case Instruction::UNCOND:
            return 1;
        default:
            break;
    }
    return 0;
}


/* calculateControlSignals
 *
 * This function calculates the control signals that determine whether a bubble
 * is inserted into the pipeline. A bubble is inserted when there are errors such 
 * as address errors (SADR), invalid instructions (SINS), or halt conditions (SHLT)
 * in the pipeline stages.
 *
 * @param: pipeRegs - A pointer to the PipeRegArray object containing the pipeline registers.
 * 
 * The function uses the `m_stat` from the Memory stage and the `W_stat` from the 
 * Writeback stage to check for error conditions. If either stage has an error status
 * (SADR, SINS, SHLT), a bubble is inserted in the pipeline.
 */
void ExecuteStage::calculateControlSignals(PipeRegArray *pipeRegs) 
{
    PipeReg *wreg = pipeRegs->getWritebackReg();

    uint64_t m_stat = Stage::m_stat;
    uint64_t W_stat = wreg->get(W_STAT);

    M_bubble = (m_stat == Status::SADR || m_stat == Status::SINS || m_stat == Status::SHLT) ||
               (W_stat == Status::SADR || W_stat == Status::SINS || W_stat == Status::SHLT);
}


/* setcc
 *
 * This function determines whether the condition codes (CC) should be set for the
 * current instruction based on the opcode (iCode) and the status of the Memory (M)
 * and Writeback (W) stages. It checks for errors such as address errors (SADR), 
 * invalid instructions (SINS), and halt conditions (SHLT), and only allows condition 
 * code setting if there are no errors in these stages.
 *
 * @param: iCode - The opcode of the current instruction.
 * @param: wreg - A pointer to the Writeback pipeline register containing the status 
 *                of the Writeback stage.
 * 
 * @return: A boolean value indicating whether the condition codes can be set. 
 *          Returns `true` if the instruction is valid (iCode matches `IOPQ` or `IADDQ`)
 *          and there are no errors in the Memory or Writeback stages. Returns `false` 
 *          otherwise.
 */
bool ExecuteStage::setcc(uint64_t iCode, PipeReg * wreg)
{
    uint64_t m_stat = Stage::m_stat;
    uint64_t W_stat = wreg->get(W_STAT);
   return ((iCode == Instruction::IOPQ || iCode == Instruction::IADDQ) 
                && (m_stat != Status::SADR && m_stat != Status::SINS && m_stat != Status::SHLT) 
                && (W_stat != Status::SADR && W_stat != Status::SINS && W_stat != Status::SHLT));
}
