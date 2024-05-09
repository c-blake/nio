#include <stdio.h>      /*  C89 program by Charles Blake, May 14, 2022 based */
#include <stdlib.h>     /*..upon the similar c2tsv Nim program by same. */
#include <string.h>
#if defined(_WIN32)||defined(_WIN64)||defined(__WIN32__)||defined(__WINDOWS__)
#  include <io.h>
#  include <fcntl.h>
#  define READ_SIN(buf, len) _read(_fileno(stdin), buf, len)
char const *const el = "\r\n";
#elif defined(__unix)
#  include <unistd.h>
#  define READ_SIN(buf, len) read(0, buf, len)
char const *const el = "\n";
#else
#  define READ_SIN(buf, len) fread(buf, 1, len, stdin)
char const *const el = "\n";
#endif

char const *const use = "Usage:\n"
"    %s REQUIRED-STDIN-NAME [INPUT-BUFFER-SIZE-IN-BYTES] < StdIn > StdOut\n\n"
"Convert RFC4180++ quoted CSV UTF8 on stdin to DSV=STRICTLY Delim-Separated TSV\n"
"stdout where either output delimiter inside fields is \\-escaped, AS IS \\ (not\n"
"simply forbidden as IANA TSV). Output is soundly NewLine-HardTAB-split-parsable.\n\n"
"Parsing ease&pipeline|file parallelism imply A) higher level ideas like headers\n"
"& table structure are best done by `popen(\"c2dsv\")` | by temp files & B) that\n"
"{\\\\,\\t,\\n}-unescaping should be deferred until field framing is unneeded.\n\n"
"Traits here that vary among tools accepting RFC4180-ish CSV:\n"
"  '\\n' & ',' inside quoted fields are ok in input.\n"
"  {'\\r',\"\\r\\n\",'\\n'} are ok line ends; ALWAYS emits '\\n' (incl@EOF).\n"
"  \" is ok in unquoted fields; Fields STARTING w/\" MUST obey quoting rules.\n\n"
"To suppress informational stderr logging, use \"\" for STDIN-NAME.  \"++\" here\n"
"refers to accepting more line end variety than RFC4180 (which requires CRLF).\n"
"c2tsv.nim has a better CLI experience.%s";
/*
  State machine from github.com/eBay/tsv-utils.  The idea is to read into buffer
  usually making 1 byte changes in the same buffer.  A flush (shifting origin)
  @buffer end or when removing CR/"|subbing tab/NL lessens copies.  2-byte CRLF
  can be split across reads, handled by 'CR' DFA states. c2tsv has a better CLI.
*/
char const tab ='\t', nl ='\n';
char const *const TabSub="\\t", *const NlSub="\\t", *const BsSub = "\\\\";
typedef enum { sFEnd, sNonQuo, sQuo, sQuoInQuo, sCRAtEnd, sCRInQuo } DFAState_t;

int main(int ac, char **av) {
  char  *si  = ac > 1 ? av[1]       : "stdin";
  size_t bSz = ac > 2 ? atoi(av[2]) : 65536;
  DFAState_t s = sFEnd;         /* Current parser state */
  size_t rNo = 1, fNo = 0,      /* [10]-origin input row/field numbers */
         totNl = 0, totTab = 0, totBs = 0, /* Cnters for in-field dlm stats */
         n;                     /* number of bytes read */
  char *buffr = malloc(bSz);    /* Input buffer */
  char *inBuf = &buffr[0];
  if (ac < 2) return fprintf(stderr, use, av[0], el), 1;
#if defined(_WIN32)||defined(_WIN64)||defined(__WIN32__)||defined(__WINDOWS__)
  _setmode(_fileno(stdin), _O_BINARY); _setmode(_fileno(stdout), _O_BINARY);
#endif
  setvbuf(stdout, NULL,0, bSz); /* Match output buffer size w/input's */
# define EOR() rNo++; fNo = 0 /* End Of Row/record */

# define FLUSH(sub) do {      /* Called @EOBlock; Does not emit ending byte*/\
  fwrite(&inBuf[i0], i - i0, 1, stdout); /* inBuf[i0..<i] */ \
  if (strlen(sub) > 0) fwrite(&sub[0], strlen(sub), 1, stdout); \
  i0 = i + 1; } while (0)     /* Next blk always 1 past current block end */

#define SUB(x) FLUSH(x ## Sub); ++(tot ## x) /* Substitution */
  while ((n = READ_SIN(inBuf, bSz)) > 0) {
    size_t i0 = 0, i = 0;     /* Block start: Ix where next write starts */
    while (i < n) {
      char c = inBuf[i];      /* Current block is inBuf[i0..<i] */
      switch (s) {            /* ODD CODE FMT FOR EASY DFA-DIAGRAM READ-OFF */
      case sFEnd:             /* DFA: startOfInput|after eating field term */
        ++fNo;
        if (c == '"') { FLUSH("")        ; s = sQuo; }
        else {                             s = sNonQuo; continue; }
        break;
      case sNonQuo:           /* DFA: doing a non-quoted field */
        switch (c) {
          case ',' : inBuf[i] = tab      ; s = sFEnd;    break;
          case '\t': SUB(Tab);                           break;
          case '\n': EOR()               ; s = sFEnd;    break;
          case '\r': inBuf[i]=nl; EOR()  ; s = sCRAtEnd; break;
          case '\\': SUB(Bs); break; /* single \ in input --> \\ in output */
        }
        break;
      case sQuo:              /* DFA: doing a quoted field */
        switch (c) {          /* flush block w/o ". New `s` decides if emit " */
          case '"' : FLUSH("")           ; s = sQuoInQuo; break;
          case '\t': SUB(Tab);                            break;
          case '\n': SUB(Nl);                             break;
          case '\r': SUB(Nl)             ; s = sCRInQuo;  break;
          case '\\': SUB(Bs); break; /* single \ in input --> \\ in output */
        }
        break;
      case sQuoInQuo:         /* DFA: just did '"' in a quoted field; Buf w/o */
        switch (c) {          /* ..'"' just flushed.  Legal bytes now: ",\n */
          case '"' :                       s = sQuo    ; break;
          case ',' : inBuf[i] = tab      ; s = sFEnd   ; break;
          case '\n': EOR()               ; s = sFEnd   ; break;
          case '\r': inBuf[i] = nl; EOR(); s = sCRAtEnd; break;
          default:fprintf(stderr,"%s:%lu bad quoting @char '%c'%s",si,rNo,c,el);
                  return 1;
        }
        break;
      case sCRInQuo:          /* DFA: last char was a CR in a quoted field */
        if (c == '\n') { FLUSH("")       ; s = sQuo; }
        else {                             s = sQuo; continue; } /* NakedCR */
        break;
      case sCRAtEnd:          /* DFA: last char was a CR terming a row/line */
        if (c == '\n') { FLUSH("")       ; s = sFEnd; }
        else {                             s = sFEnd; continue; } /* NakedCR */
        break;
      }
      ++i;
    }
    fwrite(&inBuf[i0], n - i0, 1, stdout);
    i0 = 0;                   /* inBuf[i0..<n] @EOBuf above slightly slower */
  }
  if (fNo > 0) putchar('\n'); /* ensure newline termination */
  if (strlen(si) > 0)
    fprintf(stderr, "%s: %lu \\n subs %lu \\t subs %lu \\\\ subs%s",
            si, totNl, totTab, totBs, el);
  if (s == sQuo)
    return fprintf(stderr, "%s:%lu unterminated quote @EOF%s", si, rNo, el), 1;
  return 0;
}
