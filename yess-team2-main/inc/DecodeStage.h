#include "PipeRegArray.h"
#include "Stage.h"

#ifndef DECODESTAGE_H
#define DECODESTAGE_H
class DecodeStage: public Stage
{
   private:
      //TODO: provide declarations for new methods
      bool E_bubble;
   public:
      //These are the only methods called outside of the class
      bool doClockLow(PipeRegArray * pipeRegs);
      void doClockHigh(PipeRegArray * pipeRegs);
      void setEInput(PipeReg *ereg, uint64_t stat, uint64_t icode, uint64_t ifun, uint64_t valC, uint64_t valA, uint64_t valB, uint64_t dstE, uint64_t dstM, uint64_t srcA, uint64_t srcB);
      uint64_t setSRCA(PipeReg *dreg, uint64_t icode);
      uint64_t setSRCB(PipeReg *dreg, uint64_t icode);
      uint64_t setDstE(PipeReg *dreg, uint64_t icode);
      uint64_t setDstM(PipeReg *dreg, uint64_t icode);
      uint64_t setSelFwdA(PipeReg *dreg, PipeReg *mreg, PipeReg *wreg, PipeReg *ereg, uint64_t icode, uint64_t srcA);
      uint64_t setFwdB(PipeReg *dreg, PipeReg *mreg, PipeReg *wreg, PipeReg *ereg, uint64_t icode, uint64_t srcB);
      void calculateControlSignals(uint64_t iCode, uint64_t dstM);
};
#endif
