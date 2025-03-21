#include "PipeRegArray.h"
#include "Stage.h"


#ifndef EXECUTESTAGE_H
#define EXECUTESTAGE_H
class ExecuteStage: public Stage
{
   private:
      //TODO: provide declarations for new methods
      bool M_bubble;
   public:
      //These are the only methods called outside of the class
      bool doClockLow(PipeRegArray * pipeRegs);
      void doClockHigh(PipeRegArray * pipeRegs);
      void setMInput(PipeReg *mreg, uint64_t stat, uint64_t icode, uint64_t valA, uint64_t dstM, uint64_t dstE, uint64_t Cnd, uint64_t valE);
      uint64_t setaluA(uint64_t iCode, PipeReg * ereg);
      uint64_t setaluB(uint64_t iCode, PipeReg * ereg);
      uint64_t alufun(uint64_t iCode, PipeReg * ereg);
      uint64_t setdstE(uint64_t iCode, uint64_t Cnd, PipeReg * ereg);
      void CC(uint64_t cc, uint64_t alufun, uint64_t aluA, uint64_t aluB);
      uint64_t ALU(uint64_t alufun, uint64_t aluA, uint64_t aluB);
      uint64_t cond(uint64_t icode, uint64_t ifun);
      void calculateControlSignals(PipeRegArray *pipeRegs);
      bool setcc(uint64_t iCode, PipeReg *wreg);
};
#endif