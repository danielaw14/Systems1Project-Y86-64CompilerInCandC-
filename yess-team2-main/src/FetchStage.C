#include <cstdint>
#include "PipeRegArray.h"
#include "PipeReg.h"
#include "Memory.h"
#include "FetchStage.h"
#include "Instruction.h"
#include "RegisterFile.h"
#include "Status.h"
#include "F.h"
#include "D.h"
#include "E.h"
#include "M.h"
#include "W.h"
#include <Tools.h>
/*
 * doClockLow
 *
 * Performs the Fetch stage combinational logic that is performed when
 * the clock edge is low.
 *
 * @param: pipeRegs - array of the pipeline register 
                      (F, D, E, M, W instances)
 */
bool FetchStage::doClockLow(PipeRegArray * pipeRegs)
{
    PipeReg * freg = pipeRegs->getFetchReg();
    PipeReg * ereg = pipeRegs->getExecuteReg();
    PipeReg * dreg = pipeRegs->getDecodeReg();
    PipeReg * mreg = pipeRegs->getMemoryReg();
    bool mem_error = false;
    uint64_t icode = Instruction::INOP, ifun = Instruction::FNONE;
    uint64_t rA = RegisterFile::RNONE, rB = RegisterFile::RNONE;
    uint64_t valC = 0, valP = 0, stat = Status::SAOK, predPC = 0;
    bool needvalC = false;
    bool needregId = false;


    uint64_t f_pc = selectPC(pipeRegs);
    uint8_t memByte = mem->getByte(f_pc, mem_error);
    icode =  Tools::getBits(memByte, 4, 8);
    ifun = Tools::getBits(memByte, 0, 3);
    if(mem_error)
    {
        icode = Instruction::INOP;
        ifun = Instruction::FNONE;
    }

    bool instr_valid = checkValid(icode);

    stat = setFStat(instr_valid, mem_error, icode);


    calculateControllSignals(ereg, dreg, mreg);

    needvalC = needValC(icode);
    needregId = needRegIds(icode);


    getRegIds(f_pc, needregId,rA, rB);

    getValC(f_pc, needvalC, valC, needregId);

    valP = PCincrement(f_pc, needregId, needvalC); 

 
    predPC = predictPC(icode, valC, valP);

    freg->set(F_PREDPC, predPC);

    setDInput(dreg, stat, icode, ifun, rA, rB, valC, valP);
    return false;
}

/* doClockHigh
 *
 * applies the appropriate control signal to the F
 * and D register intances
 * 
 * @param: pipeRegs - array of the pipeline register (F, D, E, M, W instances)
*/
void FetchStage::doClockHigh(PipeRegArray * pipeRegs)
{
    PipeReg * freg = pipeRegs->getFetchReg();  
    PipeReg * dreg = pipeRegs->getDecodeReg();

    if(!F_stall)
    {
        freg->normal();
    }
    
    if(D_bubble)
    {
        ((D *)dreg) ->bubble();
    }
    else if(!D_stall)
    {
        dreg->normal();
    }
    
}

/* setDInput
 * provides the input to potentially be stored in the D register
 * during doClockHigh
 *
 * @param: dreg - pointer to the D register instance
 * @param: stat - value to be stored in the stat pipeline register within D
 * @param: icode - value to be stored in the icode pipeline register within D
 * @param: ifun - value to be stored in the ifun pipeline register within D
 * @param: rA - value to be stored in the rA pipeline register within D
 * @param: rB - value to be stored in the rB pipeline register within D
 * @param: valC - value to be stored in the valC pipeline register within D
 * @param: valP - value to be stored in the valP pipeline register within D
*/
void FetchStage::setDInput(PipeReg * dreg, uint64_t stat, uint64_t icode,
                           uint64_t ifun, uint64_t rA, uint64_t rB,
                           uint64_t valC, uint64_t valP)
{
    dreg->set(D_STAT, stat);
    dreg->set(D_ICODE, icode);
    dreg->set(D_IFUN, ifun);
    dreg->set(D_RA, rA);
    dreg->set(D_RB, rB);
    dreg->set(D_VALC, valC);
    dreg->set(D_VALP, valP);
}



/* selectPC
 *
 * This function selects the program counter (PC) value for the next instruction 
 * based on the current pipeline stage instructions. The function evaluates 
 * conditions for control flow instructions (like conditional jumps and returns)
 * and returns the appropriate PC value. If no special conditions are met, 
 * it returns the predicted PC value.
 *
 * @param: pipeRegs - A pointer to the PipeRegArray containing the pipeline registers,
 *                    which includes registers for Memory, Writeback, and Fetch stages.
 *
 * @return: uint64_t - The selected PC value. The returned value depends on the
 *                     conditions of the instructions in the Memory and Writeback stages:
 *                     - If the instruction in the Memory stage is a conditional jump 
 *                       (`IJXX`) and the condition is not met (`M_CND` is false), the 
 *                       PC is set to `M_VALA` (jump target).
 *                     - If the instruction in the Writeback stage is a return (`IRET`),
 *                       the PC is set to `W_VALM` (return address).
 *                     - If no special conditions are met, the PC is set to the predicted 
 *                       PC value from the Fetch stage (`F_PREDPC`).
 */
uint64_t FetchStage::selectPC(PipeRegArray *pipeRegs) {
    PipeReg * mreg = pipeRegs->getMemoryReg();
    PipeReg * wreg = pipeRegs->getWritebackReg();
    
    if (mreg->get(M_ICODE) == Instruction::IJXX && !mreg->get(M_CND)) 
    {
        return mreg->get(M_VALA);
    }
    if (wreg->get(W_ICODE) == Instruction::IRET) 
    {
        return wreg->get(W_VALM);
    }
    return pipeRegs->getFetchReg()->get(F_PREDPC);
}


/* needRegIds
 *
 * This function determines whether a given instruction requires register IDs for its execution.
 * It checks the instruction code (`icode`) to identify if the instruction needs register operands,
 * which are required for operations like register-to-register moves, arithmetic operations, and memory 
 * operations involving registers.
 *
 *
 * @param: icode - The instruction code representing the current instruction.
 *
 * @return: bool - Returns true if the instruction requires register IDs 
 */
bool FetchStage::needRegIds(uint64_t icode) 
{
    return (icode == Instruction::IRRMOVQ || icode == Instruction::IOPQ || icode == Instruction::IPUSHQ 
            || icode == Instruction::IPOPQ || icode == Instruction::IIRMOVQ 
            || icode == Instruction::IRMMOVQ || icode == Instruction::IMRMOVQ
            || icode == Instruction::IADDQ);
}




/* needValC
 *
 * This function determines whether a given instruction requires the immediate value (ValC) for its execution.
 * Some instructions, such as those involving memory operations or conditional jumps, require an immediate
 * value to specify an address or offset.
 *
 *
 * @param: icode - The instruction code representing the current instruction.
 *
 * @return: bool - Returns true if the instruction requires an immediate value (ValC) to be processed.
 *                 Otherwise, returns false. 
 */
bool FetchStage::needValC(uint64_t icode) 
{
    return (icode == Instruction::IIRMOVQ || icode == Instruction::IRMMOVQ || icode == Instruction::IMRMOVQ 
           || icode == Instruction::IJXX || icode == Instruction::ICALL || icode == Instruction::IADDQ);
}



/* predictPC
 *
 * This function predicts the next value for the program counter (PC) based on the instruction type and the
 * immediate value (ValC) or the next instruction address (ValP).
 *
 *
 * @param icode - The instruction code for the current instruction.
 * @param valC  - The immediate value (target address) for jump or call instructions.
 * @param valP  - The address of the next instruction (typically sequential).
 *
 * @return uint64_t - The predicted value for the next program counter (PC).
 *                     Returns the target address (ValC) for jumps and calls, 
 *                     otherwise returns the next instruction address (ValP).
 */
uint64_t FetchStage::predictPC(uint64_t icode, uint64_t valC, uint64_t valP) 
{
    if (icode == Instruction::IJXX || icode == Instruction::ICALL) 
    {
        return valC;  
    }
    return valP;  
}



/* PCincrement
 *
 * This function calculates the next value for the program counter (PC) based on the current value of the 
 * program counter (`f_pc`) and two flags: `needRegIds` and `needValC`. 
 *
 * 
 * @param f_pc      The current program counter value.
 * @param needRegIds A boolean indicating if register IDs are needed, adding 1 byte to the instruction length.
 * @param needValC   A boolean indicating if an immediate value (ValC) is needed, adding 8 bytes to the instruction length.
 * 
 * @return uint64_t The next program counter value after adding the appropriate instruction length.
 */
uint64_t FetchStage::PCincrement(uint64_t f_pc, bool needRegIds, bool needValC) 
{
    uint64_t length = 1; 
    if (needValC) 
    {
        length += 8;  
    }
    if (needRegIds) 
    {
        length += 1; 
    }
    return f_pc + length; 
}


/* getRegIds
 *
 * This function extracts the register IDs (rA and rB) from the instruction at the specified program 
 * counter (`f_pc`) if the `need_regId` flag is set to true. The register IDs are encoded in a single byte 
 * following the opcode in the instruction.
 *
 * @param f_pc         The current program counter (PC) value, pointing to the current instruction.
 * @param need_regId   A boolean flag indicating whether register IDs are needed to be extracted.
 * @param rA           The register ID for the first register (output parameter).
 * @param rB           The register ID for the second register (output parameter).
 */
void FetchStage::getRegIds(uint64_t f_pc, bool need_regId, uint64_t &rA, uint64_t &rB) 
{
    if (need_regId)
    {
        bool error;
        uint8_t regByte = mem->getByte(f_pc + 1, error); 
        rA = Tools::getBits(regByte, 4, 7);  
        rB = Tools::getBits(regByte, 0, 3); 
    }
}

/* getValC
 *
 * This function retrieves the value of the constant (`valC`) from the instruction at the specified 
 * program counter (`f_pc`) if the `need_valC` flag is set to true. The constant is stored as an 8-byte 
 * value and is located at `f_pc + 1` or `f_pc + 2`, depending on whether register IDs are also needed.
 *
 *
 * @param f_pc         The current program counter (PC) value, pointing to the current instruction.
 * @param need_valC    A boolean flag indicating whether `valC` needs to be fetched.
 * @param valC         The 64-bit constant value fetched from memory (output parameter).
 * @param need_regid   A boolean flag indicating whether register IDs are present in the instruction
 *                     (affects the starting address of `valC`).
 */
void FetchStage::getValC(uint64_t f_pc, bool need_valC, uint64_t &valC, bool need_regid) 
{
    if (need_valC) 
    {
        uint64_t valC_address;
        uint8_t byte[8];
        bool error;
        
        if (need_regid)
        {
            valC_address = f_pc + 2;  
        }
        else
        {
            valC_address = f_pc + 1;  
        }

        for (int i = 0; i < 8; i++) 
        {
            byte[i] = mem->getByte(valC_address + i, error);
        }
        
        valC = Tools::buildLong(byte);
    }
}


/* setFStat
 *
 * This function sets the status based on the instruction type (`icode`) and any memory errors (`mem_error`).
 * The status returned indicates whether the current instruction execution is successful, has encountered
 * a memory access error, or has an invalid instruction.
 *
 *
 * @param iv           A boolean flag indicating if the instruction is valid.
 * @param mem_error    A boolean flag indicating if there was a memory error.
 * @param icode        The instruction type (opcode) to determine special cases like halt.
 * 
 * @return The status code indicating the result of the instruction execution:
 *         - `SADR` for memory access error
 *         - `SINS` for invalid instruction
 *         - `SHLT` for halt instruction
 *         - `SAOK` for successful execution
 */
uint64_t FetchStage::setFStat(bool iv, bool mem_error, uint64_t icode)
{
    if (mem_error) 
    {
        return Status::SADR; 
    } 
    else if (!iv) 
    {
        return Status::SINS; 
    } 
    else if (icode == Instruction::IHALT) 
    {
        return Status::SHLT;
    } 
    else 
    {
        return Status::SAOK; 
    }
}


/* checkValid
 *
 * This function checks if the given instruction code (`icode`) is valid according to the defined set of instructions.
 * It returns true if the `icode` corresponds to a valid instruction, and false otherwise.
 *
 *
 * @param icode        The instruction code (opcode) to check.
 * 
 * @return true if the instruction code is valid, false otherwise.
 */
bool FetchStage::checkValid(uint64_t icode)
{
    return (icode == Instruction::INOP || icode == Instruction::IHALT ||
                    icode == Instruction::IRRMOVQ || icode == Instruction::IIRMOVQ ||
                    icode == Instruction::IRMMOVQ || icode == Instruction::IMRMOVQ ||
                    icode == Instruction::IOPQ || icode == Instruction::IJXX ||
                    icode == Instruction::ICALL || icode == Instruction::IRET ||
                    icode == Instruction::IPUSHQ || icode == Instruction::IPOPQ ||
                    icode == Instruction:: IADDQ);
}


/* setFStall
 *
 * This function determines if a stall is needed in the Fetch stage based on the status of the Execute, Decode, and Memory stages.
 * A stall occurs if certain conditions involving instruction types and dependencies between the stages are met.
 *
 *
 * @param ereg    The Execute stage pipe register.
 * @param dreg    The Decode stage pipe register.
 * @param mreg    The Memory stage pipe register.
 * 
 * @return None (modifies the `F_stall` member variable to indicate whether a stall is needed).
 */
void FetchStage::setFStall(PipeReg * ereg, PipeReg * dreg, PipeReg * mreg)
{
    bool A = ((ereg->get(E_ICODE) == Instruction::IMRMOVQ) || (ereg->get(E_ICODE) == Instruction::IPOPQ));

    bool B = ((ereg->get(E_DSTM) == Stage::d_srcA) || (ereg->get(E_DSTM) == Stage::d_srcB));
 
    bool C = ((dreg->get(D_ICODE) == Instruction::IRET) || (ereg->get(E_ICODE) == Instruction::IRET) || (mreg->get(M_ICODE) == Instruction::IRET));
    
    F_stall = (A && B) || C;
}


/* setDStall
 *
 * This function determines if a stall is needed in the Decode stage based on the instruction in the Execute stage.
 * A stall in the Decode stage occurs if an instruction in the Execute stage is a memory-related operation
 * and there is a register dependency on either source A or source B in the Decode stage.
 *
 *
 * @param ereg    The Execute stage pipe register.
 * 
 * @return None (modifies the `D_stall` member variable to indicate whether a stall is needed).
 */
void FetchStage::setDStall(PipeReg * ereg)
{
    D_stall = ((ereg->get(E_ICODE) == Instruction::IMRMOVQ || ereg->get(E_ICODE) == Instruction::IPOPQ) 
                    && (ereg->get(E_DSTM) == Stage::d_srcA || (ereg->get(E_DSTM) == Stage::d_srcB)));
}


/* set_DBubble
 *
 * This function determines if a bubble (stall) is needed in the Decode stage based on the conditions in the Execute, Decode, and Memory stages.
 * A bubble occurs when there is a hazard or conflict between instructions in the pipeline that requires the Decode stage to wait.
 *
 *
 * @param ereg    The Execute stage pipe register.
 * @param dreg    The Decode stage pipe register.
 * @param mreg    The Memory stage pipe register.
 * 
 * @return None (modifies the `D_bubble` member variable to indicate whether a bubble is needed).
 */
void FetchStage::set_DBubble(PipeReg * ereg, PipeReg * dreg, PipeReg * mreg)
{
    bool A = ((ereg->get(E_ICODE) == Instruction::IJXX) && (!Stage::e_Cnd));

    bool B = (((ereg->get(E_ICODE) == Instruction::IMRMOVQ) || (ereg->get(E_ICODE) == Instruction::IPOPQ)) &&
                    ((ereg->get(E_DSTM) == Stage::d_srcA) || (ereg->get(E_DSTM) == Stage::d_srcB)));
                    
    bool C = ((dreg->get(D_ICODE) == Instruction::IRET) ||
                            (ereg->get(E_ICODE) == Instruction::IRET) ||
                            (mreg->get(M_ICODE) == Instruction::IRET));

    D_bubble = A || (!B && C);
}


/* calculateControlSignals
 *
 * This function calculates and sets the control signals for the pipeline stages (Fetch, Decode, and Execute).
 * These control signals are used to manage the flow of data and handle stalls or bubbles in the pipeline.
 *
 * @param ereg    The Execute stage pipe register.
 * @param dreg    The Decode stage pipe register.
 * @param mreg    The Memory stage pipe register.
 * 
 * @return None (modifies the `F_stall`, `D_stall`, and `D_bubble` member variables).
 */
void FetchStage::calculateControllSignals(PipeReg * ereg, PipeReg * dreg, PipeReg * mreg)
{
    setFStall(ereg, dreg, mreg);
    setDStall(ereg);
    set_DBubble(ereg, dreg, mreg);
}