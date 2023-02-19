  MODULE('CWFIN')
    AMORTIZE(REAL,REAL,REAL,USHORT,*DECIMAL,*DECIMAL,*DECIMAL),NAME('AMORTIZE')
    APR(REAL,USHORT),REAL,NAME('APR')
    COMPINT(REAL,REAL,USHORT),REAL,NAME('COMPINT')
    CONTINT(REAL,REAL),REAL,NAME('CONTINT')
    DAYS360(LONG,LONG),LONG,NAME('DAYS360')
    FV(REAL,REAL,REAL,REAL),REAL,NAME('FV')
    IRR(REAL,*DECIMAL[],*DECIMAL[],REAL),REAL,NAME('IRR')
    NPV(REAL,*DECIMAL[],*DECIMAL[],REAL),REAL,NAME('NPV')
    PMT(REAL,REAL,REAL,REAL),REAL,NAME('PMT')
    PREFV(REAL,REAL,REAL,REAL),REAL,NAME('PREFV')
    PREPERS(REAL,REAL,REAL,REAL),REAL,NAME('PREPERS')
    PREPMT(REAL,REAL,REAL,REAL),REAL,NAME('PREPMT')
    PREPV(REAL,REAL,REAL,REAL),REAL,NAME('PREPV')
    PRERATE(REAL,REAL,REAL,REAL),REAL,NAME('PRERATE')
    PERS(REAL,REAL,REAL,REAL),REAL,NAME('PERS')
    PV(REAL,REAL,REAL,REAL),REAL,NAME('PV')
    RATE(REAL,REAL,REAL,REAL),REAL,NAME('RATE')
    SIMPINT(REAL,REAL),REAL,NAME('SIMPINT')
  END

