function SetDifference, a, b

  ; = a and (not b) = elements in A but not in B

  mina = min(a, Max=maxa)
  minb = min(b, Max=maxb)
  if (minb gt maxa) or (maxb lt mina) then return, a ;No intersection...
  r = where((histogram(a, Min=mina, Max=maxa) ne 0) and $
    (histogram(b, Min=mina, Max=maxa) eq 0), count)
  if count eq 0 then return, -999 else return, r + mina
end

function SetIntersection, a, b

  ; Find the intersection of the ranges.
  mina = min(a, Max=maxa)
  minb = min(b, Max=maxb)
  minab = mina > minb
  maxab = maxa < maxb

  ; If the set ranges don't intersect, then result = NULL.
  if ((maxa lt minab) and (minb gt maxab)) or $
    ((maxb lt minab) and (mina gt maxab)) then return, -999

  r = where((histogram(a, Min=minab, Max=maxab) ne 0) and  $
    (histogram(b, Min=minab, Max=maxab) ne 0), count)

  if count eq 0 then return, -999 else return, r + minab
end