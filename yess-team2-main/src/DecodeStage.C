#include <cstdint>
#include "PipeRegArray.h"
#include "DecodeStage.h"
#include "D.h"
#include "M.h"
#include "W.h"
#include "ExecuteStage.h"
#include "E.h"
#include "Stage.h"
#include <Instruction.h>
#include "RegisterFile.h"
#include "Memory.h"
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
bool DecodeStage::doClockLow(PipeRegArray * pipeRegs)
{
   PipeReg * ereg = pipeRegs->getExecuteReg();
   PipeReg * dreg = pipeRegs->getDecodeReg();
   PipeReg * mreg = pipeRegs->getMemoryReg();
   PipeReg * wreg = pipeRegs->getWritebackReg();

   uint64_t stat = dreg->get(D_STAT);
   uint64_t iCode = dreg->get(D_ICODE);
   uint64_t iFun = dreg->get(D_IFUN);
   uint64_t valC = dreg->get(D_VALC);

   Stage::d_srcA = setSRCA(dreg, iCode);
   Stage::d_srcB = setSRCB(dreg, iCode);
   uint64_t dstE = setDstE(dreg, iCode);
   uint64_t dstM = setDstM(dreg, iCode);
   calculateControlSignals(ereg -> get(E_ICODE), ereg -> get(E_DSTM));
   uint64_t valA = setSelFwdA(dreg, mreg,  wreg, ereg, iCode, d_srcA);
   uint64_t valB = setFwdB(dreg, mreg,  wreg, ereg, iCode, d_srcB);
   DecodeStage::setEInput(ereg, stat, iCode, iFun, valC, valA, valB, dstE, dstM, Stage::d_srcA, d_srcB);

   return false;
}

/* doClockHigh
 *
 * applies the appropriate control signal to the F
 * and D register intances
 * 
 * @param: pipeRegs - array of the pipeline register (F, D, E, M, W instances)
*/
void DecodeStage::doClockHigh(PipeRegArray * pipeRegs)
{
   PipeReg *ereg = pipeRegs->getExecuteReg();

   if(E_bubble)
   {
      ((E*)ereg)->bubble();
   }
   else
   {
      ereg->normal();
   }
}

/* setEInput
 *
 * Sets the values for the execute register (E register) with the given inputs.
 * The values are used to update various fields in the E register that are involved
 * in the execution stage of the pipeline.
 *
 * @param ereg - Pointer to the execute register instance (E).
 * @param stat - Status value to be set in the E register.
 * @param icode - Instruction code to be set in the E register.
 * @param ifun - Function code to be set in the E register.
 * @param valC - Constant value to be set in the E register.
 * @param valA - First value to be set in the E register.
 * @param valB - Second value to be set in the E register.
 * @param dstE - Destination register for the E stage.
 * @param dstM - Destination register for the M stage.
 * @param srcA - Source register A to be set in the E register.
 * @param srcB - Source register B to be set in the E register.
 */
void DecodeStage::setEInput(PipeReg * ereg, uint64_t stat, uint64_t icode,
                              uint64_t ifun, uint64_t valC, uint64_t valA, uint64_t valB,
                              uint64_t dstE, uint64_t dstM, uint64_t srcA, uint64_t srcB)
{
   ereg->set(E_STAT, stat);
   ereg->set(E_ICODE, icode);
   ereg->set(E_IFUN, ifun);
   ereg->set(E_VALC, valC);
   ereg->set(E_VALA, valA);
   ereg->set(E_VALB, valB);
   ereg->set(E_DSTE, dstE);
   ereg->set(E_DSTM, dstM);
   ereg->set(E_SRCA, srcA);
   ereg->set(E_SRCB, srcB);
}


/* setSRCA
 *
 * Determines the source register A based on the instruction code (icode).
 * The source register is selected according to the type of instruction.
 * 
 * @param dreg - Pointer to the decode register instance (D register).
 * @param icode - Instruction code used to determine the source register A.
 * 
 * @return - The source register A, which could be a register from the D register,
 *           the stack pointer (rsp), or RNONE depending on the instruction type.
 */
uint64_t DecodeStage::setSRCA(PipeReg * dreg, uint64_t icode)
{
   if (icode == Instruction::IRRMOVQ || icode == Instruction::IRMMOVQ || icode == Instruction::IOPQ 
         || icode == Instruction::IPUSHQ)
   {
      return dreg->get(D_RA);
   }
   else if(icode == Instruction::IPOPQ || icode == Instruction::IRET)
   {
      return RegisterFile::rsp;
   }
   else
   {
      return RegisterFile::RNONE;
   }
}

/* setSRCB
 *
 * Determines the source register B based on the instruction code (icode).
 * The source register is selected according to the type of instruction.
 * 
 * @param dreg - Pointer to the decode register instance (D register).
 * @param icode - Instruction code used to determine the source register B.
 * 
 * @return - The source register B, which could be a register from the D register,
 *           the stack pointer (rsp), or RNONE depending on the instruction type.
 */
uint64_t DecodeStage::setSRCB(PipeReg * dreg, uint64_t icode)
{
   if (icode == Instruction::IMRMOVQ || icode == Instruction::IRMMOVQ || icode == Instruction::IOPQ || icode == Instruction::IADDQ)
   {
      return dreg->get(D_RB);
   }
   else if(icode == Instruction::IPUSHQ || icode == Instruction::IPOPQ 
            || icode == Instruction::ICALL || icode == Instruction::IRET)
   {
      return RegisterFile::rsp;
   }
   else
   {
      return RegisterFile::RNONE;
   }
}


/* setDstE
 *
 * Determines the destination register E based on the instruction code (icode).
 * The destination register is selected according to the type of instruction.
 * 
 * @param dreg - Pointer to the decode register instance (D register).
 * @param icode - Instruction code used to determine the destination register E.
 * 
 * @return - The destination register E, which could be a register from the D register,
 *           the stack pointer (rsp), or RNONE depending on the instruction type.
 */
uint64_t DecodeStage::setDstE(PipeReg * dreg, uint64_t icode)
{
   if (icode == Instruction::IRRMOVQ || icode == Instruction::IIRMOVQ || icode == Instruction::IOPQ || icode == Instruction::IADDQ)
   {
      return dreg->get(D_RB);
   }
   else if(icode == Instruction::IPUSHQ || icode == Instruction::IPOPQ 
            || icode == Instruction::ICALL || icode == Instruction::IRET)
   {
      return RegisterFile::rsp;
   }
   else
   {
      return RegisterFile::RNONE;
   }
}


/* setDstM
 *
 * Determines the destination register M based on the instruction code (icode).
 * The destination register is selected according to the type of instruction.
 * 
 * @param dreg - Pointer to the decode register instance (D register).
 * @param icode - Instruction code used to determine the destination register M.
 * 
 * @return - The destination register M, which could be a register from the D register,
 *           or RNONE depending on the instruction type.
 */
uint64_t DecodeStage::setDstM(PipeReg * dreg, uint64_t icode)
{
   if (icode == Instruction::IMRMOVQ || icode == Instruction::IPOPQ)
   {
      return dreg->get(D_RA);
   }
   else
   {
      return RegisterFile::RNONE;
   }
}


/* setSelFwdA
 *
 * Determines the value to forward to the source register A based on the instruction code (icode),
 * the source register (srcA), and the values from the pipeline registers. The function checks if
 * forwarding is necessary based on the pipeline stage and returns the appropriate value.
 *
 * @param dreg - Pointer to the decode register instance (D register).
 * @param mreg - Pointer to the memory register instance (M register).
 * @param wreg - Pointer to the writeback register instance (W register).
 * @param ereg - Pointer to the execute register instance (E register).
 * @param icode - Instruction code used to determine the forwarding logic.
 * @param srcA - Source register A for which the forwarding logic is applied.
 * 
 * @return - The value to forward to the source register A, which could be a register value, 
 *           a value from the pipeline stages, or the result of reading the register file.
 */
uint64_t DecodeStage::setSelFwdA(PipeReg * dreg, PipeReg *mreg, PipeReg * wreg, PipeReg * ereg, uint64_t icode, uint64_t srcA)
{
   if(icode == Instruction::ICALL || icode == Instruction::IJXX)
   {
      return dreg->get(D_VALP);
   }
   else if(srcA == RegisterFile::RNONE){
      return 0;
   }
   else if (srcA == Stage::e_dstE)
   {
      return Stage::e_valE;
   }
   else if(srcA == mreg->get(M_DSTM))
   {
      return Stage::m_valM;
   }
   else if(srcA == mreg->get(M_DSTE))
   {
      return mreg->get(M_VALE);
   }
   else if(srcA == wreg->get(W_DSTM))
   {
      return wreg->get(W_VALM);
   }
   else if(srcA == wreg->get(W_DSTE))
   {
      return wreg->get(W_VALE);
   }
   else
   {
      bool error;
      return Stage::rf->readRegister(srcA, error);
   }
}



/* setFwdB
 *
 * Determines the value to forward to the source register B based on the instruction code (icode),
 * the source register (srcB), and the values from the pipeline registers. The function checks if
 * forwarding is necessary based on the pipeline stage and returns the appropriate value.
 *
 * @param dreg - Pointer to the decode register instance (D register).
 * @param mreg - Pointer to the memory register instance (M register).
 * @param wreg - Pointer to the writeback register instance (W register).
 * @param ereg - Pointer to the execute register instance (E register).
 * @param icode - Instruction code used to determine the forwarding logic.
 * @param srcB - Source register B for which the forwarding logic is applied.
 * 
 * @return - The value to forward to the source register B, which could be a register value, 
 *           a value from the pipeline stages, or the result of reading the register file.
 */
uint64_t DecodeStage::setFwdB(PipeReg * dreg, PipeReg *mreg, PipeReg * wreg, PipeReg * ereg, uint64_t icode, uint64_t srcB)
{
   if(srcB == RegisterFile::RNONE){
      return 0;
   }
   else if (srcB == e_dstE)
   {
      return e_valE;
   }
   else if(srcB == mreg->get(M_DSTM))
   {
      return Stage::m_valM;
   }
   else if(srcB == mreg->get(M_DSTE))
   {
      return mreg->get(M_VALE);
   }
   else if(srcB == wreg->get(W_DSTM))
   {
      return wreg->get(W_VALM);
   }
   else if(srcB == wreg->get(W_DSTE))
   {
      return wreg->get(W_VALE);
   }
   else
   {
      bool error;
      return (Stage::rf->readRegister(srcB, error));
   }
}



/* calculateControlSignals
 *
 * Calculates the control signals for the decode stage based on the instruction code (iCode) and the destination register (dstM).
 * The control signals are used to handle potential bubbles or hazards in the pipeline.
 *
 * @param iCode - The instruction code (iCode) that determines the control logic.
 * @param dstM - The destination register in the memory stage (M), used to detect data hazards.
 * 
 * This function updates the E_bubble flag based on the conditions
 */
void DecodeStage::calculateControlSignals(uint64_t iCode, uint64_t dstM) 
{
    E_bubble = (iCode == Instruction::IJXX && !Stage::e_Cnd) || 
               ((iCode == Instruction::IMRMOVQ || iCode == Instruction::IPOPQ) && 
               (dstM == Stage::d_srcA || dstM == Stage::d_srcB));
}


