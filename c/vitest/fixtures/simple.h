#ifndef OSTYPES_H
#define OSTYPES_H
#endif

#ifndef   __decimal_h
 #include <decimal.h>
#else
 #define ENUM(a,b) (b)
#endif

#ifdef OSTYPES_H
#define SICKMATE
#else
#define BAD
#endif

#ifndef OSTYPES_H
#define NAUGHTY
#else
#define COOL
#endif

#ifdef OSTYPES_H
#ifdef COOL
#define MADEIT
#endif
#endif

#ifdef OSTYPES_H
#ifdef NOTCOOL
#define NOTGOOD
#else
#define GOOD
#endif
#endif