

	--Enteros

	sucesor :: Int -> Int
	sucesor n = n + 1 
	
	sumar :: Int -> Int -> Int 
	sumar n m = n + m 
	
	divisionYResto :: Int -> Int -> ( Int , Int)
	divisionYResto _ 0 = error "no se puede dividir por 0"
	divisionYResto n m =  ( div n m , mod n m)
	
	maxDelPar :: (Int ,Int) -> Int
	maxDelPar (x,y) = if x > y then x else y  
	
	data Dir  = Norte | Sur | Este | Oeste deriving Show 
	
	
	--Tipos enumerativos
	
	opuesto :: Dir -> Dir
	opuesto Norte = Sur 
	opuesto Sur = Norte
	opuesto Este = Oeste
	opuesto Oeste = Este
	
	iguales :: Dir -> Dir -> Bool
	iguales Norte Norte = True 
	iguales Sur Sur = True
	iguales Oeste Oeste = True
	iguales Este Este = True
	iguales _ _ = False
	
	siguiente :: Dir -> Dir
	siguiente Norte = Este 
	siguiente Este = Sur 
	siguiente Sur = Oeste
	siguiente Oeste = Norte
	
	data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show 
	
	primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
	primeroYUltimoDia = (Lunes,Domingo)
	
	empiezaConM :: DiaDeSemana -> Bool
	empiezaConM Martes = True 
	empiezaConM Miercoles = True
	empiezaConM _ = False
	
	vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
	vieneDespues dia1 dia2 = posicionDelDiaEnLaSemana dia1 > posicionDelDiaEnLaSemana dia2
	
	--proposito : Dado un DiaDeSemana retorna un Int que expresa su posicion en la semana 
	
	posicionDelDiaEnLaSemana :: DiaDeSemana -> Int
	posicionDelDiaEnLaSemana Lunes = 1 
	posicionDelDiaEnLaSemana Martes = 2 
	posicionDelDiaEnLaSemana Miercoles = 3
	posicionDelDiaEnLaSemana Jueves = 4
	posicionDelDiaEnLaSemana Viernes = 5
	posicionDelDiaEnLaSemana Sabado = 6
	posicionDelDiaEnLaSemana Domingo = 7
	
	
	
	estaEnElMedio :: DiaDeSemana -> Bool
	estaEnElMedio Lunes = False
	estaEnElMedio Domingo = False
	estaEnElMedio _ = True
	

	negar :: Bool -> Bool 
	negar True = False
	negar False = True
	
	implica :: Bool -> Bool -> Bool
	implica True False = False
	implica False _ = True 
	
	yTambien :: Bool -> Bool -> Bool
	yTambien False _ = False
	yTambien _ b = b
	
	oBien :: Bool -> Bool -> Bool
	oBien False b = b
	oBien b _ = b
	
	
	--Registros
	
	data Persona = P Nombre Edad deriving Show
	type Nombre = String
	type Edad = Int 
	
	nombre :: Persona -> String
	nombre (P n e) = n
	
	edad :: Persona -> Int 
	edad (P n e) = e
	
	crecer :: Persona -> Persona
	crecer (P n e) = (P n (e+1))
	
	cambioDeNombre :: String -> Persona -> Persona
	cambioDeNombre s (P n e) = (P s e)
	
	esMayorQueLaOtra :: Persona -> Persona -> Bool
	esMayorQueLaOtra p1 p2 = edad p1 > edad p2 
						
	jose = P "Jose" 18
	juan = P "Juan" 28
	
	
	laQueEsMayor :: Persona -> Persona -> Persona
	laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
							then p1 else p2
	
	
	
	
	data Pokemon = ConsP TipoDePokemon Ener deriving Show
	
	data TipoDePokemon = Agua|Fuego|Planta  deriving Show
	type Ener = Int
	 
	data Entrenador = ConsEn NombreE Pokemon Pokemon deriving Show
	type NombreE = String
	
	superaA :: Pokemon -> Pokemon -> Bool
	superaA pok1 pok2 = tipoSuperior (tipoDePokemon pok1) (tipoDePokemon pok2)
	    
	tipoSuperior :: TipoDePokemon ->TipoDePokemon -> Bool
	tipoSuperior Agua Fuego = True 
	tipoSuperior Fuego Planta = True 
	tipoSuperior Planta Agua = True 
	tipoSuperior _ _ = False 
	
	tipoDePokemon :: Pokemon -> TipoDePokemon
	tipoDePokemon (ConsP t e) = t
	
	
	cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
	cantidadDePokemonDe tip (ConsEn n p1 p2) = unoSiesDelMismoTipo tip (tipoDePokemon p1) +
											     unoSiesDelMismoTipo tip (tipoDePokemon p2)
	
	esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
	esDelMismoTipo Agua Agua = True
	esDelMismoTipo Planta Planta = True
	esDelMismoTipo Fuego Fuego = True
	esDelMismoTipo _ _ = False	
	
	unoSiesDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
	unoSiesDelMismoTipo t1 t2 = if esDelMismoTipo t1 t2 then 1 else 0
	
	

	juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
	juntarPokemon (e1,e2) = (pokemonDeEntrenador e1) ++ (pokemonDeEntrenador e2) 
	
	pokemonDeEntrenador :: Entrenador -> [Pokemon]
	pokemonDeEntrenador (ConsEn n p1 p2) = [p1,p2]
	
	
	--Funciones polimórficas
	
	loMismo :: a -> a
	loMismo x = x
	
	siempreSiete:: a -> Int 
	siempreSiete x = 7
	
	swap ::(a,b) -> (b,a)
	swap (x,y) = (y,x)
	
	-- Las funciones son polimórficas porque funcionan con argumentos de distintos tipos
	
	--Pattern matching sobre listas
	
	
	estaVacia :: [a] -> Bool
	estaVacia [] = True
	estaVacia _  = False
	
	elPrimero :: [a] -> a
	elPrimero [] = error "la lista esta vacia"
	elPrimero (x:xs)= x     
	
	sinElPrimero :: [a] -> [a]
	sinElPrimero [] = error "la lista esta vacia"
	sinElPrimero (x:xs) = xs
	
	splitHead :: [a] -> (a, [a])
	splitHead [] = error "la lista esta vacia"
	splitHead (x:xs) = (x , xs)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	