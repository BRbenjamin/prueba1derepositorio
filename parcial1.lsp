
(idsensor (tempMin tempMax) actualTemp actualAlim radio) 

(defun volumen (lista)
	(* pi (expt (car (last lista)) 2) (cadddr lista))
)

(defun predicado (lista)
	(cond
	      (and (< (caadr lista) (caddr lista)) (> (cadadr lista)) (nth 2 lista)) 'T 'NIL)
)


(defun estadoDescriptivo (lista)
	(cond 
		((predicado lista) (list (car lista) 'optimo))
		((> (caadr lista) (caddr lista)) (list (car lista) 'bajo))
		((< (cadadr lista) (caddr lista)) (list (car lista) 'alto))
		
	)
)

PUNTO 4:

(defun listaEstados(lista)
	(mapcar estadoDescriptivo lista) // devuelve lista de sublistas
)
 o de esta otra forma
 (mapcar (lambda (x) (estadoDescriptivo x) lista))


(defun totalAlim (lista)
	(cond
		((null lista) 0)
		((consp(car lista)) (+ (volumen(car lista)) (totalAlim(cdr lista)))) 
		(T (totalAlim(cdr lista)))
	)
)

(defun rango (lista)
	(cond 
		((null lista) nil)
		((not (predicado(car lista))) T)
		(T (rango(cdr lista)))
	)
)
o
(defun rango(lista)
   (cond
      (null (lista) nil)
      ((consp(car lista)) ((= predicado(car lista) 'NIL) "Existe un sensor fuera de rango"))
      (T rango(cdr lista)) 
   	)
	)

(defun ingresoDatos(lista)
	(let (tempMin tempMax actualTemp idsensor actualAlim radio)
		(print "Ingresar tiempo minimo: ")
		(setq tempMin(read))
		(print "Ingresar tiempo maximo: ")
		(setq tempMax (read))
		(print "ingresar actual tiempo: ")
		(setq actualTemp(read))
		(print "Ingresar ID sensor: ")
		(setq idsensor(read))
		(print "Ingresar actual altura de agua: ")
		(setq actualAlim (read))
		(print "ingresar radio: ")
		(setq radio(read))
		)

	 (rango(lista))
	 (listaEstados(lista))
	 (totalAlim(lista))
)