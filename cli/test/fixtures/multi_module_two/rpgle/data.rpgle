**free

ctl-opt nomain;

dcl-s names char(15) dim(78) ctdata;
dcl-s surnames char(15) dim(27) ctdata;
dcl-s streetTypes char(3) dim(9) ctdata;
dcl-s states char(2) dim(50) ctdata;
dcl-s cities char(15) dim(15) ctdata;

dcl-proc random export;
  dcl-pi *n int(10);
    low packed(7) const;
    high packed(7) const;
  end-pi;

  dcl-s result float(8);
  dcl-s range packed(7);
  dcl-s seed int(10);
  
  dcl-pr ceeran0 extproc('CEERAN0');
    *n int(10);
    *n float(8);
    *n char(12) options(*omit);
  end-pr;

  range = (high - low) + 1;
  ceeran0(seed:result:*omit);

  return %int(result * range);
end-proc;

dcl-proc getFirst export;
  dcl-pi *n char(15);
  end-pi;

  return names(random(1:%elem(names)));
end-proc;

dcl-proc getSurname export;
  dcl-pi *n char(15);
  end-pi;

  return surnames(random(1:%elem(surnames)));
end-proc;

dcl-proc getStreetType export;
  dcl-pi *n char(3);
  end-pi;

  return streetTypes(random(1:%elem(streetTypes)));
end-proc;

dcl-proc getState export;
  dcl-pi *n char(2);
  end-pi;

  return states(random(1:%elem(states)));
end-proc;

dcl-proc getCity export;
  dcl-pi *n char(15);
  end-pi;

  return cities(random(1:%elem(cities)));
end-proc;

**CTDATA names
DAVID
JAMES
JOHN
ROBERT
MICHAEL
WILLIAM
MARY
PATRICIA
LINDA
BARBARA
ELIZABETH
JENNIFER
MARIA
SUSAN
MARGARET
DOROTHY
LISA
NANCY
KAREN
BETTY
HELEN
SANDRA
DONNA
CAROL
RUTH
SHARON
MICHELLE
LAURA
SARAH
KIMBERLY
DEBORAH
JESSICA
SHIRLEY
CYNTHIA
ANGELA
MELISSA
BRENDA
AMY
ANNA
REBECCA
VIRGINIA
KATHLEEN
PAMELA
MARTHA
DEBRA
AMANDA
STEPHANIE
CAROLYN
CHRISTINE
MARIE
JANET
CATHERINE
FRANCES
ANN
JOYCE
DIANE
ALICE
JULIE
HEATHER
TERESA
DORIS
GLORIA
EVELYN
JEAN
CHERYL
MILDRED
KATHERINE
JOAN
ASHLEY
JUDITH
ROSE
JANICE
KELLY
NICOLE
JUDY
CHRISTINA
KATHY
THERESA
**CTDATA surnames
SMITH
JOHNSON
WILLIAMS
JONES
BROWN
DAVIS
MILLER
WILSON
MOORE
TAYLOR
ANDERSON
THOMAS
JACKSON
WHITE
HARRIS
MARTIN
THOMPSON
GARCIA
MARTINEZ
ROBINSON
CLARK
RODRIGUEZ
LEWIS
LEE
WALKER
HALL
ALLEN
**CTDATA streetTypes
ST
AVE
RD
DR
CIR
BLVD
WAY
CT
LANE
**CTDATA states
AL
AK
AZ
AR
CA
CO
CT
DE
FL
GA
HI
ID
IL
IN
IA
KS
KY
LA
ME
MD
MA
MI
MN
MS
MO
MT
NE
NV
NH
NJ
NM
NY
NC
ND
OH
OK
OR
PA
RI
SC
SD
TN
TX
UT
VT
VA
WA
WV
WI
WY
**CTDATA cities
SPARTANBURG
GREENVILLE
COLUMBIA
CHARLESTON
MYRTLE BEACH
ROCK HILL
FLORENCE
ASHEVILLE
HENDERSONVILLE
ANDERSON
GREENWOOD
GAFFNEY
EASLEY
GREENWOOD