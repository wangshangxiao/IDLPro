;********************************************************************
;从熊隽处拷来的程序,用于进行集合的运算,如并/交/异或等.
;没有具体去研究相应的功能
;********************************************************************

function jdl_set_op, a, op0, b, not1=not1, not2=not2, count=count, $
              empty1=empty1, empty2=empty2, maxarray=ma, index=index
;;;集合操作，演化自rsi的cmset_op程序
;a=INDGEN(10)
;b=INDGEN(10)-5
;PRINT,jdl_set_op(a,'and',b,/not2)
;;;
  on_error, 2 ;; return on error
  count = 0L
  index0 = -1L
  ;; Histogram technique is used for array sizes < 32,000 elements
  if n_elements(ma) EQ 0 then ma = 32L*1024L

  ;; Check the number of arguments
  if n_params() LT 3 then begin
      ARG_ERR:
      message, 'USAGE: SET = jdl_set_op(A, OP, B [, COUNT=ct])', /info
      message, '  KEYWORDS: /NOT1, /NOT2, /EMPTY1, /EMPTY2, INDEX', /info
      return, -1L
  endif
  if n_elements(op0) EQ 0 then goto, ARG_ERR
  kind = keyword_set(index)
  fst = 1L
  if keyword_set(last) then fst = 0L
  if keyword_set(first) then fst = 1L

  ;; Check the operation
  sz = size(op0)
  if sz(sz(0)+1) NE 7 then begin
      OP_ERR:
      message, "ERROR: OP must be 'AND', 'OR' or 'XOR'"
      return, -1L
  endif
  op = strupcase(op0)
  if op NE 'AND' AND op NE 'OR' AND op NE 'XOR' then goto, OP_ERR

  ;; Check NOT1 and NOT2
  if keyword_set(not1) AND keyword_set(not2) then begin
      message, "ERROR: NOT1 and NOT2 cannot be set simultaneously"
      return, -1L
  endif
  if (keyword_set(not1) OR keyword_set(not2)) AND $
    (op EQ 'OR' OR op EQ 'XOR') then begin
      message, "ERROR: NOT1 and NOT2 cannot be set with 'OR' or 'XOR'"
      return, -1L
  endif

  ;; Special cases for empty set
  n1 = n_elements(a) & n2 = n_elements(b)
  if keyword_set(empty1) then n1 = 0L
  if keyword_set(empty2) then n2 = 0L
  if n1 EQ 0 OR n2 EQ 0 then begin
      ;; Eliminate duplicates
      if n1 GT 0 then a1 = jdl_set_op_uniq(a)
      if n2 GT 0 then b1 = jdl_set_op_uniq(b)
      n1 = n_elements(a1) < n1 & n2 = n_elements(b1) < n2
      case op of
          'OR': if n1 EQ 0 then goto, RET_A1 else goto, RET_B1
         'XOR': if n1 EQ 0 then goto, RET_B1 else goto, RET_A1
         'AND': begin
             if keyword_set(not1) AND n1 EQ 0 then goto, RET_B1
             if keyword_set(not2) AND n2 EQ 0 then goto, RET_A1
             return, -1L
         end
     endcase
     return, -1L
     RET_A1:
     count = n1
     if kind then begin
         if count GT 0 then return, a1 else return, -1L
     endif
     if count GT 0 then return, a(a1) else return, -1L
     RET_B1:
     count = n2
     if kind then begin
         if count GT 0 then return, b1+n1 else return, -1L
     endif
     if count GT 0 then return, b(b1) else return, -1L
 endif

  ;; Allow data to have different types, but they must be at least of
  ;; the same "base" type.  That is, you can't combine a number with a
  ;; string, etc.
  ;; basetype 0:undefined 1:real number 6:complex number 7:string
  ;;     8:structure 10:pointer 11:object

  ;;          0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
  basetype = [0, 1, 1, 1, 1, 1, 6, 7, 8, 6,10,11, 1, 1, 1, 1]

  ;; Check types of operands
  sz1 = size(a) & tp1 = sz1(sz1(0)+1)
  sz2 = size(b) & tp2 = sz2(sz2(0)+1)
  if tp1 LT 0 OR tp1 GE 16 OR tp2 LT 0 OR tp2 GE 16 then begin
      message, 'ERROR: unrecognized data types for operands'
      return, -1
  endif
  if basetype(tp1) NE basetype(tp2) then begin
      TYPE1_ERR:
      message, 'ERROR: both A and B must be of the same type'
      return, -1L
  endif
  if tp1 EQ 8 OR tp1 EQ 10 AND tp1 EQ 11 then begin
      TYPE2_ERR:
      message, 'ERROR: operands must be a numeric or string type'
      return, -1L
  endif

  ;; Now use two different kinds of algorithms: a slower but more
  ;; general algorithm for generic types, and the histogram technique
  ;; for integer types.  Even for integer types, if there is too much
  ;; dynamic range, then the slow method is used.

  if tp1 GE 4 AND tp1 LE 9 then begin
      ;; String and real types, or large int arrays
      SLOW_SET_OP:
      case op of
          'OR': begin
              uu = [a,b]    ;; OR is simple; just take unique values
              index0 = jdl_set_op_uniq(uu)
              count = n_elements(index0)
              if kind then return, index0
              return, uu(index0)
          end

          'XOR': begin
              ;; Make ordered list of set union
              ai = jdl_set_op_uniq(a) & na = n_elements(ai)
              bi = jdl_set_op_uniq(b) & nb = n_elements(bi)
              ui = [ai, bi+n1]
              uu = [a,b]    & uu = uu(ui) ;; Raw union...
              us = sort(uu) & uu = uu(us) ;; ...and sort
              if kind then ui = ui(temporary(us)) else ui = 0

              ;; Values in one set only will not have duplicates
              wh1 = where(uu NE shift(uu, -1), count1)
              if count1 EQ 0 then return, -1L
              wh = where(wh1(1:*)-wh1 EQ 1, count)
              if wh1(0) EQ 0 then begin
                  if count GT 0 then wh = [-1L, wh] else wh = [-1L]
                  count = n_elements(wh)
              endif
              if count EQ 0 then return, -1
              if kind then return, ui(wh1(wh+1))
              return, uu(wh1(wh+1))
          end

          'AND': begin
              ;; Make ordered list of set union
              ai = jdl_set_op_uniq(a) & na = n_elements(ai)
              bi = jdl_set_op_uniq(b) & nb = n_elements(bi)
              ui = [ai, bi+n1]
              uu = [a,b]    & uu = uu(ui)  ;; Raw union...
              us = sort(uu) & uu = uu(us)  ;; ...and sort
              if kind then ui = ui(us) else ui = 0

              if NOT keyword_set(not1) AND NOT keyword_set(not2) then begin

                  ;; Special case: if there are one in each set, and
                  ;; they are equal, then the SHIFT() technique below
                  ;; fails.  Do this one by hand.
                  if na EQ 1 AND nb EQ 1 AND uu(0) EQ uu(1) then begin
                      count = 1L
                      if kind then return, 0L
                      return, [uu(0)]
                  endif

                  ;; If neither "NOT" is set, then find duplicates
                  us = 0L  ;; Save memory
                  wh = where(uu EQ shift(uu, -1L), count) ;; Find non-unique
                  if count EQ 0 then return, -1L
                  ;; This should always select the element from A
                  ;; rather than B (the smaller of the two)
                  if kind then return, (ui(wh) < ui(wh+1))
                  return, uu(wh)
              endif

              ;; For "NOT" cases, we need to identify by set
              ii = make_array(na+nb, value=1b)
              if keyword_set(not1) then ii(0:na-1) = 0
              if keyword_set(not2) then ii(na:*)   = 0
              ii = ii(temporary(us))

              ;; Remove any duplicates
              wh1 = where(uu EQ shift(uu, -1L), count1) ;; Find non-unique
              if count1 GT 0 then ii([wh1, wh1+1]) = 0
              ;; Remainder is the desired set
              wh = where(ii, count)
              if count EQ 0 then return, -1L
              if kind then return, ui(wh)
              return, uu(wh)
          end

      endcase
      return, -1L  ;; DEFAULT CASE
  endif else begin

      ;; INDEX keyword forces the "slow" operation
      if kind then goto, SLOW_SET_OP

      ;; Integer types - use histogram technique if the data range
      ;; is small enough, otherwise use the "slow" technique above
      min1 = min(a, max=max1) & min2 = min(b, max=max2)
      minn = min1 < min2 & maxx = max1 > max2
      nbins = maxx-minn+1
      if (maxx-minn) GT floor(ma(0)) then goto, SLOW_SET_OP

      ;; Following operations create a histogram of the integer values.
      ha = histogram(a, min=minn, max=maxx) < 1
      hb = histogram(b, min=minn, max=maxx) < 1

      ;; Compute NOT cases
      if keyword_set(not1) then ha = 1b - ha
      if keyword_set(not2) then hb = 1b - hb
      case op of
          ;; Boolean operations
          'AND': mask = temporary(ha) AND temporary(hb)
           'OR': mask = temporary(ha)  OR temporary(hb)
          'XOR': mask = temporary(ha) XOR temporary(hb)
      endcase

      wh = where(temporary(mask), count)
      if count EQ 0 then return, -1L

      result = temporary(wh+minn)
      if tp1 NE tp2 then return, result
      szr = size(result) & tpr = szr(szr(0)+1)

      ;; Cast to the original type if necessary
      if tpr NE tp1 then begin
          fresult = make_array(n_elements(result), type=tp1)
          fresult(0) = temporary(result)
          result = temporary(fresult)
      endif

      return, result

  endelse

  return, -1L  ;; DEFAULT CASE
end