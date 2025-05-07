//CUSTPROC JOB (ACCT),'CUSTOMER MANAGEMENT',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//* JCL PROCEDURE FOR CUSTOMER MANAGEMENT BATCH PROCESSING             *
//*                                                                    *
//* THIS JCL RUNS THE CUSTOMER MANAGEMENT PROGRAM (CUSTMGMT) TO        *
//* PROCESS CUSTOMER TRANSACTIONS FROM A BATCH INPUT FILE.             *
//*                                                                    *
//* DATE-WRITTEN: 2023-05-15                                           *
//* DATE-UPDATED: 2023-09-20                                           *
//*********************************************************************
//*
//STEPLIB  DD DSN=PROD.LOAD.LIBRARY,DISP=SHR
//*
//********************************************************************
//* STEP 1: VALIDATE INPUT TRANSACTION FILE                           *
//********************************************************************
//VALIDATE EXEC PGM=FILEVRFY
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DUMMY
//INFILE   DD DSN=&TRANFILE,
//            DISP=SHR
//OUTFILE  DD DSN=&&VALIDTRN,
//            DISP=(NEW,PASS),
//            SPACE=(CYL,(1,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=32720)
//*
//********************************************************************
//* STEP 2: SORT TRANSACTION FILE BY CUSTOMER ID                      *
//********************************************************************
//SORT     EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTWK01 DD SPACE=(CYL,(5,2)),UNIT=SYSDA
//SORTWK02 DD SPACE=(CYL,(5,2)),UNIT=SYSDA
//SORTWK03 DD SPACE=(CYL,(5,2)),UNIT=SYSDA
//SORTIN   DD DSN=&&VALIDTRN,
//            DISP=(OLD,DELETE)
//SORTOUT  DD DSN=&&SORTTRN,
//            DISP=(NEW,PASS),
//            SPACE=(CYL,(1,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=32720)
//SYSIN    DD *
  SORT FIELDS=(5,10,CH,A)
  SUM FIELDS=NONE
/*
//*
//********************************************************************
//* STEP 3: BACKUP CUSTOMER FILE BEFORE PROCESSING                    *
//********************************************************************
//BACKUP   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  REPRO -
    INFILE(CUSTFILE) -
    OUTFILE(BAKFILE)
/*
//CUSTFILE DD DSN=PROD.CUSTOMER.MASTER,
//            DISP=SHR
//BAKFILE  DD DSN=PROD.CUSTOMER.BACKUP.&YYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5)),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//*
//********************************************************************
//* STEP 4: RUN CUSTOMER MANAGEMENT PROGRAM                           *
//********************************************************************
//CUSTMGMT EXEC PGM=CUSTMGMT
//STEPLIB  DD DSN=PROD.LOAD.LIBRARY,DISP=SHR
//CUSTFILE DD DSN=PROD.CUSTOMER.MASTER,
//            DISP=SHR
//TRANFILE DD DSN=&&SORTTRN,
//            DISP=(OLD,DELETE)
//RPTFILE  DD DSN=&&CUSTRPT,
//            DISP=(NEW,PASS),
//            SPACE=(CYL,(1,1)),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=0)
//SYSOUT   DD SYSOUT=*
//CEEDUMP  DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//********************************************************************
//* STEP 5: GENERATE CUSTOMER MANAGEMENT REPORT                       *
//********************************************************************
//REPORT   EXEC PGM=RPTGEN
//STEPLIB  DD DSN=PROD.LOAD.LIBRARY,DISP=SHR
//INFILE   DD DSN=&&CUSTRPT,
//            DISP=(OLD,DELETE)
//RPTOUT   DD DSN=PROD.CUSTOMER.REPORT.&YYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(1,1)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//SYSOUT   DD SYSOUT=*
//*
//********************************************************************
//* STEP 6: GENERATE EXCEPTION REPORT IF NEEDED                       *
//********************************************************************
//EXCEPT   EXEC PGM=EXCEPTRP,
//            COND=(4,GT,CUSTMGMT)
//STEPLIB  DD DSN=PROD.LOAD.LIBRARY,DISP=SHR
//EXCINP   DD DSN=&&CUSTRPT,
//            DISP=SHR
//EXCOUT   DD DSN=PROD.CUSTOMER.EXCEPT.&YYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(1,1)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//SYSOUT   DD SYSOUT=*
//*
//********************************************************************
//* STEP 7: SEND EMAIL NOTIFICATION IF ERRORS OCCURRED                *
//********************************************************************
//NOTIFY   EXEC PGM=NOTIFY,
//            COND=(4,GT,CUSTMGMT)
//STEPLIB  DD DSN=PROD.LOAD.LIBRARY,DISP=SHR
//EMLIN    DD *
TO: operations@example.com
SUBJECT: Customer Management Process Errors
MESSAGE: The customer management batch process completed with errors.
         Please review the exception report for details.
/*
//EMLOUT   DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//********************************************************************
//* STEP 8: GENERATE STATISTICS AND UPDATE PROCESS LOG                *
//********************************************************************
//STATS    EXEC PGM=STATRPT
//STEPLIB  DD DSN=PROD.LOAD.LIBRARY,DISP=SHR
//STATINP  DD DSN=&&CUSTRPT,
//            DISP=SHR
//STATOUT  DD DSN=PROD.CUSTOMER.STATS,
//            DISP=MOD
//LOGFILE  DD DSN=PROD.PROCESS.LOG,
//            DISP=MOD
//SYSOUT   DD SYSOUT=*
//*
//* END OF JOB 