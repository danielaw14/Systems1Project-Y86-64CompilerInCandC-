#include "PipeRegArray.h"
#include "PipeReg.h"
#include "Stage.h"

#ifndef FETCHSTAGE_H
#define FETCHSTAGE_H
class FetchStage: public Stage
{
   private:
      //TODO: provide declarations for new methods
      bool F_stall;
      bool D_stall;
      bool D_bubble;
      
      void setDInput(PipeReg * dreg, uint64_t stat, uint64_t icode, uint64_t ifun, 
                     uint64_t rA, uint64_t rB,
                     uint64_t valC, uint64_t valP);
      uint64_t selectPC(PipeRegArray *pipeRegs);

      bool needRegIds(uint64_t icode);

      bool needValC(uint64_t icode);

      uint64_t predictPC(uint64_t icode, uint64_t valC, uint64_t valP);

      uint64_t PCincrement(uint64_t f_pc, bool needRegIds, bool needValC);

      void getRegIds(uint64_t f_pc, bool need_regId, uint64_t &rA, uint64_t &rB);

      void getValC(uint64_t f_pc, bool need_valC, uint64_t &valC, bool need_regid);

      uint64_t setFStat(bool iv, bool mem_error, uint64_t icode);

      bool checkValid(uint64_t icode);

      void setFStall(PipeReg *ereg, PipeReg *dreg, PipeReg *mreg);

      void setDStall(PipeReg *ereg);

      void set_DBubble(PipeReg *ereg, PipeReg *dreg, PipeReg *mreg);

      void calculateControllSignals(PipeReg *ereg, PipeReg *dreg, PipeReg *mreg);

      uint64_t setICode(bool mem_error, uint64_t f_pc);

      uint64_t setIFun(bool mem_error, uint64_t f_pc);

  public:
      //These are the only methods called outside of the class
      bool doClockLow(PipeRegArray * pipeRegs);
      void doClockHigh(PipeRegArray * pipeRegs);
};
#endif
