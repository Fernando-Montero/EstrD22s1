	
	sumatoria :: [Int] -> Int 
	sumatoria [] = 0
	sumatoria (x:xs) = x + sumatoria xs
	
	longitud :: [a] -> Int
	longitud [] = 0
	longitud (x:xs) = 1 + longitud xs
	
	sucesor :: [Int] -> [Int]
	sucesor [] = []
	sucesor (x:xs) = (x+1) : sucesor xs
	
	conjugacion :: [Bool] -> Bool
	conjugacion [] = True 
	conjugacion (b:bs) = b && conjugacion bs
	
	disyuncion :: [Bool] -> Bool
	disyuncion [] = False 
	disyuncion (b:bs) = b || disyuncion bs
	
	aplanar :: [[a]] -> [a]
	aplanar []=[]
	aplanar (x:xs) = x ++ aplanar xs
	
	pertenece :: Eq a => a -> [a] -> Bool
	pertenece e [] = False
	pertenece e (x:xs) = e==x || pertenece e xs
	
	apariciones :: Eq a => a -> [a] -> Int 
	apariciones e [] = 0
	apariciones e (x:xs) =  unoSiEsIgual e x + apariciones e xs
	
	unoSiEsIgual :: Eq a => a -> a -> Int
	unoSiEsIgual e1 e2 = if e1 == e2 then 1 else 0 
	
	losMenoresA :: Int -> [Int] -> [Int]
	losMenoresA n [] = []
	losMenoresA n (x:xs) = if x < n then x : losMenoresA n xs 
									else losMenoresA n xs
	
	lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
	lasDeLongitudMayorA n [] = []
	lasDeLongitudMayorA n (x:xs) = if longitud x > n 
												then [x] ++ lasDeLongitudMayorA n xs
												else lasDeLongitudMayorA n xs
	
	agregarAlFinal :: [a] -> a -> [a]
	agregarAlFinal [] e = [e]
	agregarAlFinal xs e = xs ++ [e]
	
	agregar :: [a] -> [a] -> [a]
	agregar [] ys = ys
	agregar xs [] = xs
	agregar xs (y:ys) = agregar (agregarAlFinal xs y) ys
	
	reversa :: [a] -> [a]
	reversa [] = []
	reversa (x:xs) = reversa xs++[x]
	
	zipMaximos :: [Int] -> [Int] -> [Int]
	zipMaximos [] ys = ys 
	zipMaximos xs [] = xs
	zipMaximos (x:xs) (y:ys) = if x > y then x : zipMaximos xs ys 
										else y : zipMaximos xs ys
	
	elMinimo :: Ord a => [a] -> a
	elMinimo [] = error "la lista esta vacia"
	elMinimo (x:xs) = if null xs || x < elMinimo xs then x else elMinimo xs
	
	factorial :: Int -> Int
	factorial n = if n > 0 then n * (factorial (n-1)) else 1 
	
	cuentaRegresiva :: Int -> [Int]
	cuentaRegresiva n = if n < 1 then [] else cuentaRegresiva (n-1) ++ [n]
	
	repetir  :: Int -> a -> [a] 
	repetir 0 a = []
	repetir n a = a:(repetir (n-1) a)
	
	losPrimeros :: Int -> [a] -> [a]
	losPrimeros n [] = []
	losPrimeros n (x:xs) = if n == 0 then  []
							   else x : losPrimeros (n-1) xs   
	
	
	sinLosPrimeros :: Int -> [a] -> [a]
	sinLosPrimeros 0 xs = xs
	sinLosPrimeros n (x:xs) = if n>=1 then sinLosPrimeros (n-1) xs else xs 
	
	
	
	
	data Persona = P Nombre Edad deriving Show
	type Nombre = String
	type Edad = Int 
	
	nombre :: Persona -> String
	nombre (P n e) = n
	
	edad :: Persona -> Int 
	edad (P n e) = e

	mayoresA :: Int -> [Persona] -> [Persona]
	mayoresA n [] = []
	mayoresA n (x:xs) = if edad x > n then x : mayoresA n xs 
									  else mayoresA n xs
	
	promedioEdad :: [Persona] -> Int
	promedioEdad [] = error "no hay ninguna persona"
	promedioEdad xs = div(edades xs) (longitud xs)
	
	edades :: [Persona] -> Int
	edades [] = 0
	edades (x:xs) = (edad x) + edades xs 
	
	elMasViejo :: [Persona] -> Persona
	elMasViejo [] = error "no hay ninguna persona"
	elMasViejo [x] = x
	elMasViejo (x:xs) = if edad x > edad (elMasViejo xs) then x else elMasViejo xs
	
	
	
	data TipoDePokemon = Agua | Fuego | Planta deriving (Eq,Show)
	data Pokemon = ConsPokemon TipoDePokemon Int  deriving Show
	data Entrenador = ConsEntrenador String [Pokemon]  deriving Show
	
	cantPokemon :: Entrenador -> Int
	cantPokemon e = length (pokemonDeEntrenador e)
	
	--proposito : dado un Entrenador retorna una lista con sus Pokemon
	pokemonDeEntrenador :: Entrenador -> [Pokemon]
	pokemonDeEntrenador (ConsEntrenador n ps) = ps
	
	cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
	cantPokemonDe t (ConsEntrenador n ps) = length (todosLosPokemonDeTipo t ps)
	
	todosLosPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
	todosLosPokemonDeTipo t [] = []
	todosLosPokemonDeTipo t (x:xs) = if esDelMismoTipo t (tipoDePokemon x) 
													then x : (todosLosPokemonDeTipo t xs)
												    else todosLosPokemonDeTipo t xs
	
	tipoDePokemon :: Pokemon -> TipoDePokemon
	tipoDePokemon (ConsPokemon t e) = t
	
	esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
	esDelMismoTipo t1 t2 = t1==t2 
	
	

	losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
	losQueLeGanan t e1 e2 = length (losQueLeGananAAlguno(pokemonDeTipo t (pokemonDeEntrenador e1)) (pokemonDeEntrenador e2))
							
	
	
	--proposito : dado dos Pokemon indica si el primero le gana al segundo
	superaA :: Pokemon -> Pokemon -> Bool
	superaA p1 p2 = tipoSuperior (tipoDePokemon p1) (tipoDePokemon p2)
	
	tipoSuperior :: TipoDePokemon ->TipoDePokemon -> Bool
	tipoSuperior Agua Fuego = True 
	tipoSuperior Fuego Planta = True 
	tipoSuperior Planta Agua = True 
	tipoSuperior _ _ = False 
	
	pokemonDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
	pokemonDeTipo t [] = []
	pokemonDeTipo t (x:xs) =  if (tipoDePokemon x) == t then x : pokemonDeTipo t xs
													    else  pokemonDeTipo t xs
	
	leGanaAAlguno :: Pokemon -> [Pokemon]->Bool
	leGanaAAlguno p [] = False
	leGanaAAlguno p (x:xs) = tipoSuperior (tipoDePokemon p)(tipoDePokemon x)
											|| leGanaAAlguno p xs
	
	losQueLeGananAAlguno :: [Pokemon] -> [Pokemon] -> [Pokemon]
	losQueLeGananAAlguno [] ys = []
	losQueLeGananAAlguno xs [] = []
	losQueLeGananAAlguno (x:xs) ys = if leGanaAAlguno x ys 
													then x : losQueLeGananAAlguno xs ys		
													else losQueLeGananAAlguno xs ys	
	
	cantidadDeTipo :: TipoDePokemon -> [Pokemon] -> Int
	cantidadDeTipo t [] = 0
	cantidadDeTipo t (x:xs) = if esDelMismoTipo (tipoDePokemon x) t then 1 + cantidadDeTipo t xs
										     else  cantidadDeTipo t xs
	
	esMaestroPokemon :: Entrenador -> Bool
	esMaestroPokemon p = poseeDeTodosLosTipos (pokemonDeEntrenador p)
	
	poseeUnoDeTipo:: TipoDePokemon -> [Pokemon]-> Bool
	poseeUnoDeTipo t [] = False
	poseeUnoDeTipo t (x:xs) = esDelMismoTipo t (tipoDePokemon x)||poseeUnoDeTipo t xs 
	
	poseeDeTodosLosTipos :: [Pokemon]->Bool
	poseeDeTodosLosTipos xs = poseeUnoDeTipo Agua xs && 
							  poseeUnoDeTipo Fuego xs && 
							  poseeUnoDeTipo Planta xs
	
	
	data Seniority = Junior | SemiSenior | Senior deriving Show
	data Proyecto = ConsProyecto String  deriving (Show,Eq)
	data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
	data Empresa = ConsEmpresa [Rol] deriving Show
	
	
	proyectos :: Empresa -> [Proyecto]
	proyectos (ConsEmpresa xs) = sinProyectosRepetidos(proyectosDeRoles xs)
	
	
	proyectosDeRoles :: [Rol] -> [Proyecto]
	proyectosDeRoles [] = []
	proyectosDeRoles (x:xs) = (proyectoDeDesarrollador x): proyectosDeRoles xs
	
	proyectoDeDesarrollador :: Rol -> Proyecto
	proyectoDeDesarrollador (Developer s p) = p
	proyectoDeDesarrollador (Management s p) = p
	
	sinProyectosRepetidos :: [Proyecto] -> [Proyecto]
	sinProyectosRepetidos [] = []
	sinProyectosRepetidos (x:xs) = if elem x (sinProyectosRepetidos xs)
											then sinProyectosRepetidos xs
											else x : sinProyectosRepetidos xs
	
	losDevSenior :: Empresa -> [Proyecto] -> Int
	losDevSenior (ConsEmpresa ps) [] = 0
	losDevSenior (ConsEmpresa ps) xs = length(pertenecenAlProyecto (desarrolladoresSenior ps) xs)
	
	
	--proposito : dado un Rol devuelve una lista con ese Rol si el desarrollador es Senior
	desarrolladorSenior :: Rol -> [Rol]
	desarrolladorSenior (Developer Senior p) = [Developer Senior p]
	desarrolladorSenior (Developer _ p) = []
	
	desarrolladoresSenior :: [Rol] -> [Rol]
	desarrolladoresSenior [] = []
	desarrolladoresSenior (x:xs) = (desarrolladorSenior x) ++ (desarrolladoresSenior xs)
	
	perteneceAlProyecto :: Rol ->[Proyecto] -> Bool
	perteneceAlProyecto (Developer s p) ps = elem p ps
	
	--proposito : dada una lista de desarrolladores y una de proyectos retorna 
	--la lista de desarrolladores que participan en al menos un proyecto
	pertenecenAlProyecto :: [Rol]->[Proyecto] -> [Rol]
	pertenecenAlProyecto [] ps = []
	pertenecenAlProyecto (x:xs) ps = if perteneceAlProyecto x ps 
													then x : pertenecenAlProyecto xs ps
													else pertenecenAlProyecto xs ps



	--cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
	--
	--
	--
	
	--asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
	--
	--
	--
	
	
	
	
	pro1 = ConsProyecto "cajero"
	pro2 = ConsProyecto "banco"
	pro3 = ConsProyecto "municipalidad"
	
	desa1= Developer Junior pro1 
	desa2= Developer SemiSenior pro1
	desa3= Developer Senior pro2
	desa4= Developer Senior pro3
	empresa = ConsEmpresa [desa1,desa2,desa3,desa4]
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	kyogre = ConsPokemon Agua 70
	moltres = ConsPokemon Fuego 50
	charmander = ConsPokemon Fuego 10 
	bulbasaur = ConsPokemon Planta 12
	squirtle = ConsPokemon Agua 7 
	
	ash = ConsEntrenador "Ash" [charmander,moltres]
	gary = ConsEntrenador "Gary" [squirtle,squirtle]
	maximo = ConsEntrenador "Maximo" [kyogre,moltres,bulbasaur]
	chano = ConsEntrenador "Chano" []
	
	jose = P "Jose" 18
	juan = P "Juan" 28
	miguel = P "Miguel" 38
	
	multitud = [jose,juan,miguel]
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	