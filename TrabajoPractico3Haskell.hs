
	data Color = Azul | Rojo deriving Show
	data Celda = Bolita Color Celda | CeldaVacia deriving Show
	
	
	nroBolitas :: Color -> Celda -> Int
	nroBolitas c CeldaVacia = 0
	nroBolitas c (Bolita co cel) = if esElMismoColor c co 
											then 1 + nroBolitas c cel
											else nroBolitas c cel
												
	poner :: Color -> Celda -> Celda
	poner c CeldaVacia = Bolita c CeldaVacia
	poner c (Bolita co cel) = Bolita co (poner c cel)
	
	
	sacar :: Color -> Celda -> Celda
	sacar c CeldaVacia = CeldaVacia
	sacar c cel = if esElMismoColor c (colorDeBolita cel)
											then CeldaVacia
											else cel
	sacar c (Bolita co cel) = Bolita co (sacar c cel)
	
	esElMismoColor:: Color -> Color -> Bool
	esElMismoColor Azul Azul = True
	esElMismoColor Rojo Rojo = True
	esElMismoColor _ _ = False
	
	colorDeBolita :: Celda -> Color 
	colorDeBolita CeldaVacia = error "la celda esta vacia"
	colorDeBolita (Bolita c cel) = c
	
	ponerN :: Int -> Color -> Celda -> Celda
	ponerN 0 c CeldaVacia = CeldaVacia
	ponerN 1 c (Bolita co CeldaVacia) = Bolita co (Bolita c CeldaVacia)
	ponerN i c (Bolita co cel) = poner c (Bolita co (ponerN (i-1) c cel))
	
	
	rojas= (Bolita Rojo (Bolita Rojo (Bolita Rojo CeldaVacia)))
	
	
	
	
	data Objeto = Cacharro | Tesoro deriving Show
	data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show
	
	hayTesoro :: Camino -> Bool
	hayTesoro Fin = False
	hayTesoro (Cofre xs cam) = hayAlMenosUnTesoro xs || hayTesoro cam
	hayTesoro (Nada cam) = hayTesoro cam
	
	hayAlMenosUnTesoro :: [Objeto] -> Bool
	hayAlMenosUnTesoro [] = False
	hayAlMenosUnTesoro (x:xs) = esTesoro x || hayAlMenosUnTesoro xs
	
	esTesoro :: Objeto -> Bool
	esTesoro Cacharro = False
	esTesoro Tesoro = True
	
	pasosHastaTesoro :: Camino -> Int
	pasosHastaTesoro cam = if hayTesoro cam 
										then cuantosPasosHastaElTesoro cam
										else 0
	
	cuantosPasosHastaElTesoro :: Camino -> Int
	cuantosPasosHastaElTesoro Fin = 0
	cuantosPasosHastaElTesoro (Cofre xs cam) =  if hayAlMenosUnTesoro xs
															then 1
															else 1+cuantosPasosHastaElTesoro cam
															
															
	cuantosPasosHastaElTesoro (Nada cam) = cuantosPasosHastaElTesoro cam
	
	
	
	hayTesoroEn :: Int -> Camino -> Bool
	hayTesoroEn n Fin = False
	hayTesoroEn n (Cofre xs cel) = if n==0
										then hayAlMenosUnTesoro xs
										else hayTesoroEn (n-1) cel									
	hayTesoroEn n (Nada cam) = hayTesoroEn (n-1) cam 
	
	
	
	alMenosNTesoros :: Int -> Camino -> Bool
	alMenosNTesoros i cam = tesorosEnUnCamino cam >= i
	
	cantidadDeTesoros :: [Objeto] -> Int
	cantidadDeTesoros [] = 0
	cantidadDeTesoros (x:xs) = if esTesoro x 
											 then 1 + cantidadDeTesoros xs
											 else cantidadDeTesoros xs

	tesorosEnUnCamino :: Camino -> Int
	tesorosEnUnCamino Fin = 0
	tesorosEnUnCamino (Cofre xs cam) = cantidadDeTesoros xs + (tesorosEnUnCamino cam)
	tesorosEnUnCamino (Nada cam) = tesorosEnUnCamino cam
 	
	
	--cantTesorosEntre :: Int -> Int -> Camino -> Int
	
	
	
	--if n mayor 0 entonces avanzar y resto n, cuando es igual sumar mientras sea menor al rango mayor 
	
	
	
	data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
	
	sumarT :: Tree Int -> Int
	sumarT EmptyT = 0
	sumarT (NodeT e t1 t2) = e + (sumarT t1)+(sumarT t2)
	
	sizeT :: Tree a -> Int
	sizeT EmptyT = 0
	sizeT (NodeT e t1 t2) = 1 + (sizeT t1)+(sizeT t2)
	
	mapDobleT :: Tree Int -> Tree Int
	mapDobleT EmptyT = EmptyT
	mapDobleT (NodeT e t1 t2) = NodeT (e*2) (mapDobleT t1) (mapDobleT t2)
	
	perteneceT :: Eq a => a -> Tree a -> Bool
	perteneceT o EmptyT = False
	perteneceT o (NodeT e t1 t2) = o == e || perteneceT o t1|| perteneceT o t2
	
	aparicionesT :: Eq a => a -> Tree a -> Int
	aparicionesT o EmptyT = 0
	aparicionesT o (NodeT e t1 t2) = if o==e then 1 + aparicionesT o t1 +
													  aparicionesT o t2
											 else aparicionesT o t1 +
												  aparicionesT o t2
	
	leaves :: Tree a -> [a]
	leaves EmptyT = []
	leaves (NodeT e EmptyT EmptyT) = [e]
	leaves (NodeT e t1 t2) = leaves t1 ++ leaves t2
	
	heightT :: Tree a -> Int
	heightT EmptyT = 0 
	heightT (NodeT e t1 t2) = 1 + max (heightT t1)(heightT t2)
	
	mirrorT :: Tree a -> Tree a
	mirrorT EmptyT = EmptyT
	mirrorT (NodeT e t1 t2) = NodeT e (mirrorT t2) (mirrorT t1)
	
	toList :: Tree a -> [a]
	toList EmptyT = []
	toList (NodeT e t1 t2) = (toList t1)++[e]++(toList t2)
	
	levelN :: Int -> Tree a -> [a]
	levelN n EmptyT = []
	levelN 0 (NodeT e t1 t2) = [e]
	levelN n (NodeT e t1 t2) = levelN (n-1) t1 ++  levelN (n-1) t2
	
	listPerLevel :: Tree a -> [[a]]
	listPerLevel t = r (heightT t - 1 ) t
	
	r :: Int -> Tree a -> [[a]]
	r n t = if n >= 1 then (r (n-1) t)++[levelN n t]
						 else [levelN n t]
	

	ramaMasLarga :: Tree a -> [a]
	ramaMasLarga EmptyT = []
	ramaMasLarga (NodeT e t1 t2) = if sizeT t1 > sizeT t2 
														  then e : ramaMasLarga t1
														  else e : ramaMasLarga t2
	
	todosLosCaminos :: Tree a -> [[a]]
	todosLosCaminos EmptyT = []
	todosLosCaminos (NodeT e t1 EmptyT) = [([e]++ramaMasLarga t1)]
	todosLosCaminos (NodeT e EmptyT t2) = [([e]++ramaMasLarga t2)]
	todosLosCaminos (NodeT e t1 t2) = [[e]]++ todosLosCaminos t1 ++ todosLosCaminos t2
	

	
	data ExpA = Valor Int |Sum ExpA ExpA |Prod ExpA ExpA|Neg ExpA deriving Show
	
	eval :: ExpA ->Int 
	eval (Valor i) = i
	eval (Sum v1 v2) = eval v1 + eval v2 
	eval (Prod v1 v2) = eval v1 * eval v2 
	eval (Neg v) = -(eval v)
	
	
	simplificar :: ExpA -> ExpA
	simplificar (Sum v1 v2 ) = simplificarSum v1 v2
	simplificar (Prod v1 v2 ) = simplificarProd v1 v2 
	simplificar (Neg v) = simplificarNeg v 
	simplificar e = e
	
	
	simplificarSum :: ExpA -> ExpA -> ExpA
	simplificarSum (Valor 0) e = e
	simplificarSum e (Valor 0) = e 
	simplificarSum e1 e2 = Sum e1 e2 
	
	simplificarNeg :: ExpA -> ExpA
	simplificarNeg (Neg v) = v 
	simplificarNeg e = e 
	
	simplificarProd :: ExpA -> ExpA -> ExpA
	simplificarProd (Valor 0) e = Valor 0
	simplificarProd e (Valor 0) = Valor 0
	simplificarProd (Valor 1) e = e 
	simplificarProd e (Valor 1) = e 
	simplificarProd e1 e2 = Prod e1 e2
	
	
	
	
	expreSum = Sum (Valor 1) (Valor 3)
	expreNeg = Neg (Valor 1)
	expreProd = Prod (Valor 1)(expreSum)
	expreNegNeg = Neg (expreNeg)
	
	
	
	
	
	
	
	
	arbol= NodeT 1 ( NodeT 2( NodeT 3 EmptyT EmptyT)(EmptyT)) ( NodeT 8 (EmptyT)(NodeT 6 EmptyT (NodeT 9 EmptyT EmptyT)))
	arbol2 = NodeT 2 (NodeT 5 EmptyT EmptyT) (EmptyT)
	
	cam1 = Cofre [Cacharro] (Cofre [Cacharro, Tesoro] Fin)
	
	cam2 = Cofre [Tesoro] (Cofre [Cacharro, Cacharro] Fin)
	
	cam3 = Cofre [Cacharro] (Cofre [Tesoro,Tesoro] (Cofre [Cacharro] (Cofre [Tesoro] Fin)))
	
