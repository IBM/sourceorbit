     H NoMain

      *  Field Definitions.
      * ~~~~~~~~~~~~~~~~~~~~~~~~
     D ObjNam          s             10a
     D ObjLib          s             10a
     D ObjTyp          s             10a

     d GENDS           ds
     d  OffsetHdr              1      4B 0
     d  NbrInList              9     12B 0
     d  SizeEntry             13     16B 0

      *
      *  Field Definitions.
      *
     d Count           s              4  0
     d Format          s              8
     d GenLen          s              8
     d InLibrary       s             10
     d InType          s             10
     d ObjectLib       s             20
     d SpaceVal        s              1    inz(*BLANKS)
     d SpaceAuth       s             10    inz('*CHANGE')
     d SpaceText       s             50    inz(*BLANKS)
     d SpaceRepl       s             10    inz('*YES')
     d SpaceAttr       s             10    inz(*BLANKS)
     d UserSpaceOut    s             20
     d Worktype        s             10    inz('*OUTQ')

      *
      * API Error Data Structure
      *
     d ErrorDs         DS                  INZ
     d  BytesPrv               1      4B 0
     d  BytesAvl               5      8B 0
     d  MessageId              9     15
     d  ERR###                16     16
     d  MessageDta            17    116

      *
     d                 DS
     d  StartPosit             1      4B 0
     d  StartLen               5      8B 0
     d  SpaceLen               9     12B 0
     d  ReceiveLen            13     16B 0
     d  MessageKey            17     20B 0
     d  MsgDtaLen             21     24B 0
     d  MsgQueNbr             25     28B 0

      *
      * Date structure for retriving userspace info
      *
     d InputDs         DS
     d  UserSpace              1     20
     d  SpaceName              1     10
     d  SpaceLib              11     20
     d  InpFileLib            29     48
     d  InpFFilNam            29     38
     d  InpFFilLib            39     48
     d  InpRcdFmt             49     58

     **-- Retrieve object description:  -------------------------------
     d RtvObjD         Pr                  ExtPgm( 'QUSROBJD' )
     d  RoRcvVar                  32767a         Options( *VarSize )
     d  RoRcvVarLen                  10i 0 Const
     d  RoFmtNam                      8a   Const
     d  RoObjNamQ                    20a   Const
     d  RoObjTyp                     10a   Const
     d  RoError                   32767a         Options( *VarSize )

     **-- List objects:   ---------------------------------------------
     d $ListObjects    Pr                  ExtPgm( 'QUSLOBJ' )
     d  userspace                    20a   Const
     d  format                        8a   Const
     d  objectlib                    20a   Const
     d  type                         10a   Const

     **-- Userspace pointer: ------------------------------------------
     d $Userspace      Pr                  ExtPgm( 'QUSRTVUS' )
     d  userspace                    20a   Const
     d  start                        10i 0 Const
     d  Length                       10i 0 Const
     d  Returned                  32767a         Options( *VarSize )

     **-- Create Space:   ---------------------------------------------
     d $CreateSpace    Pr                  ExtPgm( 'QUSCRTUS' )
     d  UserSpaceOut                 20a   Const
     d  SpaceAttr                    10    Const
     d  SpaceLen                     10i 0 Const
     d  SpaceVal                      1a   Const
     d  SpaceAuth                    10a   Const
     d  SpaceText                    50a   Const
     d  SpaceRepl                    10a   Const
     d  ErrorDs                   32767a         Options( *VarSize )

       // Procedures below

       // -----------------------

      /copy 'qrpgleref/system.rpgleinc'
      /copy 'qrpgleref/objects.rpgleinc'

     P Obj_List        B                   Export
     D Obj_List        PI
     D    pLibrary                   10A   Const
     D    pObject                    10A   Const
     D    pType                      10A   Const

      /Free

          exsr $QUSCRTUS;
          ObjectLib =  pObject + pLibrary;
          WorkType = pType;

          Format = 'OBJL0200';
          $ListObjects( Userspace : Format : ObjectLib : WorkType);
          //
          // Retrive header entry and process the user space
          //
          StartPosit = 125;
          StartLen   = 16;
          $UserSpace( Userspace : StartPosit : StartLen : GENDS);

          StartPosit = OffsetHdr + 1;
          StartLen = %size(ObjectDS);

          Return;

          //--------------------------------------------------------
          // $QUSCRTUS - create userspace
          //--------------------------------------------------------
          begsr $QUSCRTUS;

             system('DLTOBJ OBJ(QTEMP/LISTOUTQS) OBJTYPE(*USRSPC)');

             BytesPrv = 116;
             Spacename = 'LISTOUTQS';
             SpaceLib = 'QTEMP';

             // Create the user space
             $CreateSpace( Userspace : SpaceAttr : 4096 :
                           SpaceVal : SpaceAuth : SpaceText : SpaceRepl:
                           ErrorDs);
          endsr;
      /End-Free
     P                 E

       // -----------------------

     P Obj_Count       B                   Export
     D Obj_Count       PI             5i 0
     D Result          s              5i 0

      /Free
          Result = NbrInList;
          Return Result;
      /End-Free

     P                 E

       // -----------------------

     P Obj_Next        B                   Export
     D Obj_Next        PI                  LikeDS(ObjectDs)

      /Free
          $UserSpace( Userspace : StartPosit : StartLen : ObjectDs);
          StartPosit += SizeEntry;

          Return ObjectDs;
      /End-Free

     P                 E

