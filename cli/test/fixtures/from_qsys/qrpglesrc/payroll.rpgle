**free
ctl-opt Dftactgrp(*no);
 // ***********************************************************
 //   PROGRAM NAME - Payroll
 //   DESCRIPTION - Time reporting master file maintenance using
 //                 externally described workstation processing.
 // ***********************************************************
 //   INDICATORS USED
 //   50 - No record found on CHAIN operation
 //   60 - General error condition
 //   90 - Protect display on delete request
 //   KC - End of job requested
 //   KD - Return to application selection
 //   KE - Return to employee selection
 //   KF - Return to project selection
 //   KG - Return to reason code selection
 //   LR - Last record
 // ***********************************************************
 //   SUBROUTINES USED
 //   ValidateFileToMaintainSelection - Edit application selection display (SELECT)
 //   ValidateActionCode - Edit action code for all maintenance requests
 // ***********************************************************
 //   This program uses all externally described files.  Files
 //   used are - MSTDSP  - maintenance display file
 //            - EMPMST  - employee master file
 //            - PRJMST  - project master file
 //            - RSNMST  - reason code master file
 // ***********************************************************
Dcl-F MSTDSP     WORKSTN;
Dcl-F EMPMST     Usage(*Update:*Delete:*Output) Keyed;
Dcl-F PRJMST     Usage(*Update:*Delete:*Output) Keyed;
Dcl-F RSNMST     Usage(*Update:*Delete:*Output) Keyed;

Dcl-S EMESS           Char(50);

/include QPROTOSRC,ERRORTABLE

 //
 // *****************************************************
 //   MAINLINE CALCULATIONS
 // *****************************************************
 //   This mainline routine controls the display file processing and
 //   editting.  Using the function keys described on each display
 //   format, you can transfer from one maintenance application to
 //   another.  The action code you select on the selection formats
 //   determines if the program will add a new record to the file or
 //   update an existing record in the file.
 // *****************************************************
 //   Housekeeping, clear display fields and reset indicators.
 //
CALLP MAIN();

 //  If MAIN is done program ends
*INLR = *on;
 // *********************************************************
 //   End of job requested.  Control is passed to here when you press
 //   F3 (*INKC).  The last record indicator *INLR is set on and the
 //   program ends.
 // *******************************************************


 //  Main screen - select file to maintain
 //
Dcl-Proc MAIN;
  DoU *INKC;
    *IN60  = *OFF;
    EMESS  = *BLANK;
    EMPAPL = *BLANK;
    PRJAPL = *BLANK;
    RSNAPL = *BLANK;
     //
     //   Write the SELECT format to display until end of job requested,
     //
    DoU not *IN60;
       //   IF the general error indicator *IN60 is on (equal to 1), the
       //   program continues to loop
      Exfmt SELECT;
      If *INKC = '1'; // F3 = end of job
        Return;
      Else;
        ValidateFileToMaintainSelection();
      EndIf;
    EndDo;
 //
 //   The application selection fields from the SELECT format are
 //   tested and the program will branch to the section specific to
 //   that application.
 //   If EMPAPL (employee maintenance) equals X, the program
 //   branches to  EmployeeMaintenance() proc.
 //   If PRJAPL (project maintenance) equals X, the program
 //   branches to ProjectMaintenance() proc
 //   If the prior two tests were not successful, you have chosen
 //   reason code maintenance.  The program will continue with the
 //   next executable operation.
 //
    Select;
      When EMPAPL = 'X';
        CallP EmployeeMaintenance();
 //
      When PRJAPL = 'X';
        CallP ProjectMaintenance();
 //
      When RSNAPL = 'X';
        ReasonMaintenance();
    EndSl;
 //  if KC end program
    If *INKC;
      Leave;
    EndIf;
  EndDo;
End-Proc;
 //
 // *********************************************************
 //   Reason Code Maintenance.
 // *********************************************************
Dcl-Proc ReasonMaintenance;
  DoU *INKC;
 //
 //   Housekeeping, clear display fields and reset indicators.
 //
    EMESS = *Blanks;
    RSCDE = *Blanks;
    ACODE = *Blanks;
     //  start of error loop
    DoU not *IN60;
      RSDSC = *Blanks;
 //
 //   Display reason code selection format
 //
      Exfmt RSNSEL;
      If *INKD;
        Return;
      EndIf;
//
//   Access reason code master to validate action code request
//
      If not *INKC;
        Chain RSCDE RSNMST;
        *IN50 = not %Found;
        CallP ValidateActionCode();
      Else;
        Return;
      EndIf;
//  end of error loop
    EndDo;
//
//   Display reason code maintenance format
//
    Exfmt RSNMNT;
//
    If *INKD;
      Return;
    EndIf;
    If *INKG;
      Iter;
    EndIf;
//
//   Determine update mode and perform record add or update
//
    If not *inkc;
//
      Select;
        When ACODE = 'A' and *IN50;
          ACREC = 'A';
          Write RCRSN;
//
        When ACODE = 'A' and not *IN50 and ACREC = 'D';
          ACREC = 'A';
          Update RCRSN;
//
        When ACODE = 'D';
          ACREC = 'D';
          Update RCRSN;
//
        When ACODE = 'C';
          Update RCRSN;
      EndSl;
    Else;
      Return;
    EndIf;
//
//   Your maintenance request has now been completed and the
//   program branches back to the ReasonMaintenance loop
//
  EndDo;
End-Proc;
// *********************************************************
//   Employee master maintenance routine.
// *********************************************************
Dcl-Proc EmployeeMaintenance;
  DoU *INKC;
//
//   Housekeeping, clear display fields and reset indicators.
//
    *IN60 = '0';
    EMESS = *Blanks;
    EMPNO = 0;
    ACODE = *Blanks;
//  error loop start
    DoU not *IN60;
      ENAME = *Blanks;
      EMCAT = *Blanks;
      EDEPT = *Blanks;
      ELOCN = *Blanks;
      EUSRI = *Blanks;
      ENHRS = 0;
//
//   Display employee selection format
//
      Exfmt EMPSEL;
      // Process keys
      If *INKC;     //F3 = exit program
        Return;
      Elseif *INKD; //F4 = return to main screen
        Return;
      EndIf;
      //
      //   Access employee master to validate action code request
      //
      Chain EMPNO EMPMST;
      *IN50 = not %Found;
      ValidateActionCode();
    EndDo;   //  end of error loop
//
//   Display employee maintenance format
//
    Exfmt EMPMNT;
    If *INKC;     //F3 = exit program
      Return;
    Elseif *INKD; //F4 = return to main screen
      Return;
    Elseif *INKE; //F5 = return to employee maintenance screen
      Iter;
    EndIf;
//
//   Determine update mode and perform record add or update
//
    Select;
      When isAddNewRecordRequest();
        ACREC = 'A';
        Write RCEMP;
      When isAddPreviouslyDeletedRecordRequest();
        ACREC = 'A';
        Update RCEMP;
// Mark record deleted
      When ACODE = 'D';
        ACREC = 'D';
        Update RCEMP;
// Change record
      When ACODE = 'C';
        Update RCEMP;
    EndSl;
  EndDo;  // EmployeeMainenance screen loop
End-Proc;

// *********************************************************
//   Project master maintenance routine.
// *********************************************************
Dcl-Proc ProjectMaintenance;
  DoU *INKC;
//
//   Housekeeping, clear display fields and reset indicators.
//
    *IN60 = '0';
    EMESS = *Blanks;
    PRCDE = *Blanks;
    ACODE = *Blanks;
    DoU not *IN60;
      PRDSC = *Blanks;
      PRRSP = *Blanks;
      PRSTR = 0;
      PREND = 0;
      PRCMP = 0;
      PREST = 0;
      //
      //   Display project selection format
      //
      Exfmt PRJSEL;
      If *INKD;
        Return;
      EndIf;
      //
      //   Access project master to validate action code request
      //
      If not *INKC;
        Chain PRCDE PRJMST;
        *IN50 = not %Found;
        CallP ValidateActionCode();
      Else;
        Return;
      EndIf;
    EndDo;
    //
    //   Display project maintenance format
    //
    Exfmt PRJMNT;
    If *INKD; // F4 = return to main selection
      Return;
    EndIf;
    If *INKF; // F7 = return to project maintenance screen
      Iter;
    EndIf;
    //
    //   Determine update mode and perform record add or update
    //
    If *INKC = '0';
      Select;
        // Add of new record
        When isAddNewRecordRequest();
          ACREC = 'A';
          Write RCPRJ;
        // Add of previously deleted
        When isAddPreviouslyDeletedRecordRequest();
          ACREC = 'A';
          Update RCPRJ;
        //  Delete OP = just mark record deleted
        When ACODE = 'D';
          ACREC = 'D';
          Update RCPRJ;
        //  Change OP
        When ACODE = 'C';
          Update RCPRJ;
      EndSl;
    Else;
      Return; // F3 = exit
    EndIf;
  EndDo; // Project maintenance loop
End-Proc;

// *******************************************************
// Return true iff a new record should be added to file
Dcl-Proc isAddNewRecordRequest;
  Dcl-Pi *n ind End-Pi;
  Return  ACODE = 'A'  and *IN50;
End-Proc;

// *******************************************************
// Return true iff a previously deleted record
// should be readded to file
Dcl-Proc isAddPreviouslyDeletedRecordRequest;
  Dcl-Pi *n ind End-Pi;
  Return  ACODE = 'A' and *IN50 and ACREC = 'D';
End-Proc;

// *******************************************************
//   ValidateFileToMaintainSelection procedure verifies the time reporting
//    application selection display input.
Dcl-Proc ValidateFileToMaintainSelection;
  //
  //   Housekeeping, clear display fields and reset indicators.
  //
  EMESS = *Blanks;
  *IN60 = *OFF;
  //
  //   The following IF AND OR combination checks the application
  //   selection fields to ensure that only one application has been
  //   selected.
  //
  If EMPAPL = 'X' and PRJAPL = 'X'
  or EMPAPL = 'X' and RSNAPL = 'X'
  or RSNAPL = 'X' and PRJAPL = 'X';
    DisplayError(2);
  EndIf;
  //
  //   The following IF AND combination ensures that at least one
  //   application has been selected.
  //
  If    EMPAPL = ' '
    and PRJAPL = ' '
    and RSNAPL = ' ';
    DisplayError(3);
  EndIf;
  //
  //   The following code checks each application selection field to
  //   ensure that it is either ' ' (blank) or equal to 'X'.
  //
  If    EMPAPL <> ' '
    and EMPAPL <> 'X'
  or
        PRJAPL <> ' '
    and PRJAPL <> 'X'
  or
        RSNAPL <> ' '
    and RSNAPL <> 'X';
    DisplayError(INVALID_MAINTENANCE_SELECTION);
  EndIf;
End-Proc;
//
// *******************************************************
//   ValidateActionCode procedure verifies the time reporting action codes for
//   all maintenance selections.
// *******************************************************
Dcl-Proc ValidateActionCode;
//
//   Housekeeping, clear display fields and reset indicators.
//
  EMESS = *BLANKS;
  *IN60 = *OFF;
  *IN90 = *OFF;
  //
  //   The following  statements perform
  //   two functions.  First they determine the type of maintenance
  //   requested and branche to the appropriate subroutine and secondly
  //   they determine if the maintenance code entered is invalid.
  //
  Select;
    When ACODE = 'A';
      ValidateAdd();
    When ACODE = 'C';
      ValidateChange();
    When ACODE = 'D';
      ValidateDelete();
    Other;
      DisplayError(INVALID_ACTION_CODE);
  EndSl;
End-Proc;
//
//   The following code verifies the add request.
//   Input:
//     ACREC is the recorded state of the employee
//       'A' means added
//       'D' means deleted
//     *IN50 indicates the record is not found
//   Output:
//     If an error occurs, the EMESS field contains the appropriate message
Dcl-Proc ValidateAdd;
  If NOT *IN50 AND ACREC = 'A';
    DisplayError(5);
  Else;
    If NOT *IN50 AND ACREC = 'D';
      EMESS = ERR(6);
    EndIf;
  EndIf;
End-Proc;
//
//   The following code verifies the change request.
//
Dcl-Proc ValidateChange;
  If *IN50;
    DisplayError(7);

    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
    EMESS  = *BLANK;
  Else;
    If NOT *IN50 AND ACREC = 'D';
      DisplayError(8);
    EndIf;
  EndIf;
End-Proc;
//*
//*  The following code verifies the delete request.  The field
//*  protect indicator *IN90 is first set on (equal to 1) to not
//*  allow changes to existing data on a delete request.
//*
Dcl-Proc ValidateDelete;
  *IN90 = *ON;
  IF *IN50;
    DisplayError(9);
  ELSE;
    IF NOT  *IN50 AND ACREC = 'D';
      DisplayError(10);
    ENDIF;
  ENDIF;
End-Proc;
//
// Display error at the bottom of the screen with the given error code
Dcl-Proc DisplayError;
  Dcl-Pi *n;
    errCode uns(3) const;
  End-Pi;
  *IN60 = *ON;
  EMESS = ERR(errCode);
End-Proc;
//
//   The compile time array ERR is entered below.  The array is
//   precceded by "** " to denote the beginning of the array.
//
**  Array ERR - Error descriptions
   MAINTENANCE SELECTION CODE NOT EQUAL TO "X"
MORE THAN ONE APPLICATION SELECTED FOR MAINTENANCE
     NO APPLICATION SELECTED FOR MAINTENANCE
    ACTION CODE NOT EQUAL TO "A", "C" OR "D"
 ADD REQUESTED BUT RECORD ALREADY EXISTS IN FILE
    WARNING - RECORD WAS PREVIOUSLY DELETED
   CHANGE REQUESTED BUT RECORD DOES NOT EXIST
     CHANGE REQUESTED BUT RECORD IS DELETED
   DELETE REQUESTED BUT RECORD DOES NOT EXIST
   DELETE REQUESTED BUT RECORD ALREADY DELETED
