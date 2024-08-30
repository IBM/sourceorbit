**free
      //%METADATA                                                      *
      // %TEXT API main validation procedure (IWS)                     *
      //%EMETADATA                                                     *
///
// @Program APIVAL01S
//
// @Purpose IWS API validation procedures
//
// @author JHEI
// @Date 22 May 2024
//
///
ctl-opt option(*srcstmt:*nodebugio);
ctl-opt debug(*retval:*constants);
ctl-opt reqprexp(*require);

// includes
/copy 'apival01s.rpgleinc'

dsply 'hello';

return;