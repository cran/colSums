#include <R.h>
#include <Rdefines.h>

/* Fast !ISNAN(x), from Thomas Lumley.  Requires IEEE754 arithmetic. */
#define ISNUM(x) x==x

SEXP colSums(SEXP m, SEXP narm) {
  register int i, j, NaRm;
  int *mdims, n, p;
  double *mm, sum;
  SEXP val, nms;

  mdims = INTEGER(GET_DIM(m));
  n = mdims[0]; p = mdims[1];
  PROTECT(val  = NEW_NUMERIC(p));
  PROTECT(nms  = GET_COLNAMES(GET_DIMNAMES(m)));
  PROTECT(  m  = AS_NUMERIC(m));                         /* Force to numeric */
  NaRm = asLogical(narm);
  mm = REAL(m);
  for (j = 0; j < p; j++) {
    for (sum = 0., i = 0; i < n; i++) if (!NaRm || ISNUM(mm[i])) sum += mm[i];
    REAL(val)[j] = sum;
    mm += n;
  }
  if (!isNull(nms)) namesgets(val, nms);
  UNPROTECT(3);
  return val;
}

SEXP rowSums(SEXP m, SEXP narm) {
  register int i, j, NaRm;
  int *mdims, n, p;
  double *mm;
  SEXP val, nms;

  mdims = INTEGER(GET_DIM(m));
  n = mdims[0]; p = mdims[1];
  PROTECT(val = NEW_NUMERIC(n));
  PROTECT(nms = GET_ROWNAMES(GET_DIMNAMES(m)));
  PROTECT(  m =  AS_NUMERIC(m));                         /* Force to numeric */
  NaRm = asLogical(narm);
  mm = REAL(m);
  for (i = 0; i < n; i++) REAL(val)[i] = 0.;          /* Yes, it's necessary */
  for (j = 0; j < p; j++) {
    for (i = 0; i < n; i++) if (!NaRm || ISNUM(mm[i])) REAL(val)[i] += mm[i];
    mm += n;
  }
  if (!isNull(nms)) namesgets(val, nms);
  UNPROTECT(3);
  return val;
}
