
(defun SMR (L) ; returns wavelength (um) dependent refractive index 
   (let ((LL (sqr L)) ;Sellmeier coefficients for Schott K crown glass
       (B [1.1273555     0.124412303    0.827100531])
       (C [0.00720341707 0.0269835916 100.384588]))
        (sqrt (+ 1 (r '+ (/ (* B LL) (- LL C)))))))

(setq FR (/ 
[1013.98   ;t infrared mercury line Hg
  852.11   ;s infrared cesium line Cs
  706.5188 ;r red helium line He 
  656.2725 ;C red hydrogen line H
  643.8469 ;C' red cadmium line Cd
  632.8    ;   helium-neon-gas-laser He-Ne
  589.2938 ;D center of double sodium line Na
  587.5618 ;d yellow helium line He
  546.074  ;e green mercury line Hg
  486.1327 ;F blue hydrogen line H
  479.9914 ;F' blue cadmium line Cd
  435.8343 ;g blue mercury line Hg
  404.6561 ;h violet mercury line Hg
  365.0146 ;i ultraviolet mercury line Hg
] 1000)) ; wavelengths in um


(defun snell (n1 n2 alpha1) (asin (/ (* n1 (sin alpha1)) n2)))

(defun SnellIn (IncAng N) (asind (/ (sind IncAng) N))) ; air to N  N>air

(defun SnellOut (IncAng N) (asind (* (sind IncAng) N))) ; N to air N>air

(setq VisFR (/ [
		706.5188 ;r red helium line He                 2
		643.8469 ;C' red cadmium line Cd               3
		589.2938 ;D center of double sodium line Na    4 (std Yellow)
		587.5618 ;d yellow helium line He              5
		546.074  ;e green mercury line Hg              6
		516      ; bullshit cyan                       7
		486.1327 ;F blue hydrogen line H               8
		479.9914 ;F' blue cadmium line Cd              9
		435.8343 ;g blue mercury line Hg               10
		404.6561 ;h violet mercury line Hg             11
		] 1000))
