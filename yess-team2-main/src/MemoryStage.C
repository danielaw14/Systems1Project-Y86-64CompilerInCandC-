#include <cstdint>
#include "PipeRegArray.h"
#include "MemoryStage.h"
#include "M.h"
#include "Memory.h"
#include "W.h"
#include <Instruction.h>
#include <Status.h>

/*
 * doClockLow
 *
 * Performs the Fetch stage combinational logic that is performed when
 * the clock edge is low.
 *
 * @param: pipeRegs - array of the pipeline register 
                      (F, D, E, M, W instances)
 */
bool MemoryStage::doClockLow(PipeRegArray * pipeRegs)
{
   PipeReg *mreg = pipeRegs->getMemoryReg();
   PipeReg *wreg = pipeRegs->getWritebackReg();
   
   uint64_t iCode = mreg->get(M_ICODE);
   uint64_t valE = mreg->get(M_VALE);
   uint64_t valA = mreg->get(M_VALA);
   uint64_t dstM = mreg->get(M_DSTM);
   uint64_t dstE = mreg->get(M_DSTE);
   uint64_t mem_addr = addr(mreg, iCode);
 
   bool mem_read = (iCode == Instruction::IMRMOVQ || iCode == Instruction::IPOPQ || iCode == Instruction:: IRET);
   bool mem_write = (iCode == Instruction::IRMMOVQ || iCode == Instruction::IPUSHQ || iCode == Instruction::ICALL);
   bool mem_error = false;
   if(mem_read)
   {
      Stage::m_valM = Stage::mem->getLong(mem_addr, mem_error);
   }
   else
   {
      Stage::m_valM = 0;
   }
   if(mem_write)
   {
      Stage::mem->putLong(valA, mem_addr, mem_error);
   }

   Stage::m_stat = setMStat(mem_error, mreg);
   

   MemoryStage::setWInput(wreg, m_stat, iCode, valE, dstM, dstE, Stage::m_valM);

   return false;
}

/* doClockHigh
 *
 * applies the appropriate control signal to the F
 * and D register intances
 * 
 * @param: pipeRegs - array of the pipeline register (F, D, E, M, W instances)
*/
void MemoryStage::doClockHigh(PipeRegArray * pipeRegs)
{
   PipeReg *wreg = pipeRegs->getWritebackReg();
   wreg->normal();  
}

/* setWInput
 *
 * This function sets the values in the Writeback (W) pipeline register to prepare
 * it for the next stage in the pipeline. It stores the status, instruction code,
 * values for the destination registers, and the memory value into the corresponding
 * fields of the Writeback register. This is typically done after the Memory stage
 * to pass the appropriate data to the Writeback stage for final processing.
 *
 * @param: wreg - A pointer to the Writeback pipeline register that will be updated.
 * @param: stat - The status of the current stage (such as normal operation, error, etc.).
 * @param: icode - The instruction code indicating the type of operation being performed.
 * @param: valE - The value computed during the Execute stage (for instructions like ALU operations).
 * @param: dstM - The destination register for values written back from the Memory stage.
 * @param: dstE - The destination register for values computed during the Execute stage.
 * @param: m_valM - The value from memory, which is passed to the Writeback stage.
 * 
 * This function does not return any value. It directly modifies the given `wreg` pipeline register.
 */
void MemoryStage::setWInput(PipeReg *wreg, uint64_t stat, uint64_t icode,
                                uint64_t valE, uint64_t dstM, uint64_t dstE,uint64_t  m_valM)
{
    wreg->set(W_STAT, stat);
    wreg->set(W_ICODE, icode);
    wreg->set(W_VALE, valE);
    wreg->set(W_DSTM, dstM);
    wreg->set(W_DSTE, dstE);
    wreg->set(W_VALM, m_valM);
}


/* addr
 *
 * This function calculates the memory address for memory-related instructions
 * based on the given instruction code (`iCode`). It retrieves the appropriate
 * value from the Memory (M) stage register (`mreg`) to compute the address used
 * for memory read or write operations. Different instructions use different 
 * values from the register, so this function checks the instruction type 
 * and returns the corresponding address.
 *
 * @param: mreg - A pointer to the Memory pipeline register containing values
 *                for various stages of memory access.
 * @param: iCode - The instruction code, which determines the type of memory 
 *                 operation and thus the address to return.
 *
 * @return: uint64_t - The computed address for the memory operation based on
 *           the instruction type. If no memory operation is needed, it returns 0.
 */
uint64_t MemoryStage::addr(PipeReg *mreg, uint64_t iCode)
{
   if (iCode == Instruction::IRMMOVQ || iCode == Instruction::IPUSHQ || iCode == Instruction::ICALL || iCode == Instruction::IMRMOVQ)
   {
      return mreg->get(M_VALE);
   }
   else if (iCode == Instruction::IPOPQ || iCode == Instruction::IRET)
   {
      return mreg->get(M_VALA);
   }
   else
   {
      return 0;
   }
}



/* setMStat
 *
 * This function sets the memory status based on whether a memory error has occurred
 * during the memory stage of the pipeline. If a memory error is detected, the status
 * is set to `SADR` (indicating an address error). Otherwise, the function retrieves
 * the current memory status from the Memory stage register (`mreg`) and returns it.
 *
 * @param: mem_error - A boolean flag indicating whether a memory error has occurred. 
 *                      If true, an address error status is returned.
 * @param: mreg - A pointer to the Memory stage pipeline register, which contains
 *                the current memory status.
 *
 * @return: uint64_t - The memory status. If a memory error occurred, it returns
 *           `SADR`; otherwise, it returns the current status from the `mreg` register.
 */
uint64_t MemoryStage::setMStat(bool mem_error, PipeReg *mreg)
{
   if(mem_error)
   {
      return Status::SADR;
   }
   else
   {
      return mreg->get(M_STAT);
   }
}
