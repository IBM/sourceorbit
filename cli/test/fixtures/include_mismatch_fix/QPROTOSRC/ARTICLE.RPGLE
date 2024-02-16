      *%METADATA                                                      *
      * %TEXT Function Article                                        *
      *%EMETADATA                                                     *
      *=============================================
      * Get ARTICLE DESCRPTION
      *=============================================
     DGetArtDesc       PR            50A
     D ARID                           6A    value
      *=============================================
      * Get REF SALE PRICE
      *=============================================
     DGetArtRefSalPrice...
     D                 pr             7P 2
     D ARID                           6A    value
      *=============================================
      * Get STOCK PRICE
      *=============================================
     DGetArtStockPrice...
     D                 PR             7P 2
     D ARID                           6A    value
      *=============================================
      * Get FAMILLY ID
      *=============================================
     DGetArtFam        PR             3A
     D ARID                           6A    value
      *=============================================
      * Get STOCK
      *=============================================
     DGetArtStock      PR             5P 0
     D ARID                           6A    value
      *=============================================
      * Get MINIMUM STOCK
      *=============================================
     DGetArtMinStock   PR             5P 0
     D ARID                           6A    value
      *=============================================
      * Get VAT code
      *=============================================
     DGetArtVatCode    Pr             1A
     D P_ARID                         6A    value
      *=============================================
      * Check if article exist
      *=============================================
     D ExistArt        PR              n
     D ARID                           6A    value
      *=============================================
      * Get DELETE CODE X=DELETED
      *=============================================
     DIsArtDeleted     PR              n
     D ARID                           6A    value
      *=============================================
      * Select an article
      *=============================================
     D SltArticle      PR             6
     D ARID                           6A    value
      *=============================================
      * Get ARTICLE Info
      *=============================================
     DGetArtInfo       PR          1520A
     D ARID                           6A    value
      *=============================================
      * Close ARTICLE1
      *=============================================
     D CloseARTICLE1   PR
