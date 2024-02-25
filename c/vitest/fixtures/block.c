switch(c) {
   case '<' : OutBuf += cpy (OutBuf , "&lt;")   ; break;
   case '>' : OutBuf += cpy (OutBuf , "&gt;")   ; break;
   case '&' : OutBuf += cpy (OutBuf , "&amp;")  ; break;
   case '\'': OutBuf += cpy (OutBuf , "&apos;") ; break;
   case '\"': OutBuf += cpy (OutBuf , "&quot;") ; break;
   default  : *(OutBuf++) = c;
}