
let optionVale u d r snot opt = 
   let psquig = ( 1 + r - d ) / (u - d)
   let qsquig = 1 - psquig
   let vh = opt (snot * u)
   let vt = opt (snot * d)
   let delta = (vh - vt) / (snot * (u -d))
   in 
      ( 1 / ( 1 + r )) * ((psquig * vh) +  (qsquig * vt))
      
      
