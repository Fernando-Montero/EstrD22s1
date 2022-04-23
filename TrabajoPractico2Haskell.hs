    
    sumatoria :: [Int] -> Int 
    sumatoria [] = 0
    sumatoria (x:xs) = x + sumatoria xs
    
    longitud :: [a] -> Int
    longitud [] = 0
    longitud (x:xs) = 1 + longitud xs
    
    sucesor :: [Int] -> [Int]
    sucesor [] = []
    sucesor (x:xs) = (x+1) : sucesor xs
    
    conjuncion :: [Bool] -> Bool
    conjuncion [] = True 
    conjuncion (b:bs) = b && conjuncion bs
    
    disyuncion :: [Bool] -> Bool
    disyuncion [] = False 
    disyuncion (b:bs) = b || disyuncion bs
    
    aplanar :: [[a]] -> [a]
    aplanar []=[]
    aplanar (xs:xss) = xs ++ aplanar xss
    
    pertenece :: Eq a => a -> [a] -> Bool
    pertenece e [] = False
    pertenece e (x:xs) = e==x || pertenece e xs
    
    apariciones :: Eq a => a -> [a] -> Int 
    apariciones e [] = 0
    apariciones e (x:xs) = unoSiEsIgual e x + apariciones e xs
    
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
    agregarAlFinal  (x:xs) e = x:(agregarAlFinal xs e)
    
    
    agregar :: [a] -> [a] -> [a]
    agregar [] ys = ys
    agregar (x:xs) ys = x : agregar xs ys
    
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
    cuentaRegresiva n = if n < 1 then [] else n:cuentaRegresiva (n-1)
    
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
    todosLosPokemonDeTipo t (x:xs) = if t == (tipoDePokemon x) 
                                                    then x : (todosLosPokemonDeTipo t xs)
                                                    else todosLosPokemonDeTipo t xs
    
    tipoDePokemon :: Pokemon -> TipoDePokemon
    tipoDePokemon (ConsPokemon t e) = t
    

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
    leGanaAAlguno p (x:xs) = superaA  p x || leGanaAAlguno p xs
    
    losQueLeGananAAlguno :: [Pokemon] -> [Pokemon] -> [Pokemon]
    losQueLeGananAAlguno [] ys = []
    losQueLeGananAAlguno (x:xs) ys = if leGanaAAlguno x ys 
                                                    then x : losQueLeGananAAlguno xs ys        
                                                    else losQueLeGananAAlguno xs ys    
    
    cantidadDeTipo :: TipoDePokemon -> [Pokemon] -> Int
    cantidadDeTipo t [] = 0
    cantidadDeTipo t (x:xs) = if (tipoDePokemon x) == t then 1 + cantidadDeTipo t xs
                                             else  cantidadDeTipo t xs
    
    esMaestroPokemon :: Entrenador -> Bool
    esMaestroPokemon p = poseeDeTodosLosTipos (pokemonDeEntrenador p)
    
    poseeUnoDeTipo:: TipoDePokemon -> [Pokemon]-> Bool
    poseeUnoDeTipo t [] = False
    poseeUnoDeTipo t (x:xs) = t ==(tipoDePokemon x)||poseeUnoDeTipo t xs 
    
    poseeDeTodosLosTipos :: [Pokemon]->Bool
    poseeDeTodosLosTipos xs = poseeUnoDeTipo Agua xs && 
                              poseeUnoDeTipo Fuego xs && 
                              poseeUnoDeTipo Planta xs
    
---------------------------------------------------------------------------------------------------------
    
    data Seniority = Junior | SemiSenior | Senior deriving Show
    data Proyecto = ConsProyecto String  deriving (Show,Eq)
    data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
    data Empresa = ConsEmpresa [Rol] deriving Show
    
    
    proyectos :: Empresa -> [Proyecto]
    proyectos (ConsEmpresa xs) = sinRepetidos(proyectosDeRoles xs)
    
    
    proyectosDeRoles :: [Rol] -> [Proyecto]
    proyectosDeRoles [] = []
    proyectosDeRoles (x:xs) = (proyectoDeRol x): proyectosDeRoles xs
    
    proyectoDeRol :: Rol -> Proyecto
    proyectoDeRol (Developer s p) = p
    proyectoDeRol (Management s p) = p
    
                                      
    sinRepetidos :: Eq a => [a]-> [a]
    sinRepetidos [] = []
    sinRepetidos (x:xs) = if elem x xs then sinRepetidos xs else x:(sinRepetidos xs) 
    
    losDevSenior :: Empresa -> [Proyecto] -> Int
    losDevSenior (ConsEmpresa ps) xs = length(pertenecenAlProyecto (desarrolladoresSenior ps) xs)
    

    --proposito : dado un Rol devuelve una lista con ese Rol si el desarrollador es Senior
    desarrolladorSenior :: Rol -> [Rol]
    desarrolladorSenior r = if esDevSenior r then enlistarRol r 
                                             else []
    
    enlistarRol :: Rol -> [Rol]
    enlistarRol r = [r]
    
    esDevSenior:: Rol -> Bool
    esDevSenior (Developer s p) = esSenior s 
    
    esSenior :: Seniority -> Bool
    esSenior Senior = True
    esSenior _ = False
    
    desarrolladoresSenior :: [Rol] -> [Rol]
    desarrolladoresSenior [] = []
    desarrolladoresSenior (x:xs) = (desarrolladorSenior x) ++ (desarrolladoresSenior xs)
    
    perteneceAlProyecto :: Rol -> [Proyecto] -> Bool
    perteneceAlProyecto r ps = elem (proyectoDeRol r) ps

 
    pertenecenAlProyecto :: [Rol]->[Proyecto] -> [Rol]
    pertenecenAlProyecto [] ps = []
    pertenecenAlProyecto (x:xs) ps = if perteneceAlProyecto x ps 
                                                    then x : pertenecenAlProyecto xs ps
                                                    else pertenecenAlProyecto xs ps


    cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
    cantQueTrabajanEn ps (ConsEmpresa rs) = cantRolQueParticipan(losDeRolDesarrollador rs) ps


    losDeRolDesarrollador :: [Rol] -> [Rol]
    losDeRolDesarrollador [] = []
    losDeRolDesarrollador (r:rs) = desarrollador r ++ losDeRolDesarrollador rs 
    
    desarrollador :: Rol -> [Rol]
    desarrollador r = if esDev r then enlistarRol r 
                                    else []
    esDev :: Rol -> Bool
    esDev (Developer s p) = True
    esDev _ = False 
    
    
    cantRolQueParticipan :: [Rol] -> [Proyecto] -> Int
    cantRolQueParticipan [] ps = 0
    cantRolQueParticipan (r:rs) ps = if perteneceAlProyecto r ps 
                                then 1 + cantRolQueParticipan rs ps
                                else cantRolQueParticipan rs ps
    
    
    
    asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
    asignadosPorProyecto (ConsEmpresa rs) = parRolEnProyectos(proyectosDeRoles rs)rs 
    
 
    parCantRolEnProyecto :: Proyecto -> [Rol] -> (Proyecto,Int)
    parCantRolEnProyecto p [] = (p , 0)
    parCantRolEnProyecto p rs =  (p ,cantRolEnProyecto p rs)
    
    cantRolEnProyecto :: Proyecto -> [Rol] -> Int 
    cantRolEnProyecto p [] = 0
    cantRolEnProyecto p (x:xs) = if estaEnElProyecto x p
                                        then 1 + cantRolEnProyecto p xs 
                                        else cantRolEnProyecto p xs 
    
    estaEnElProyecto :: Rol -> Proyecto -> Bool
    estaEnElProyecto (Developer s pb) p = p == pb
   
    parRolEnProyectos :: [Proyecto] -> [Rol] -> [(Proyecto,Int)]
    parRolEnProyectos [] rs = []
    parRolEnProyectos (p:ps) rs = [parCantRolEnProyecto p rs] ++
                                            parRolEnProyectos ps rs 
    
    
    pr1 = ConsProyecto "cajero"
    pr2 = ConsProyecto "banco"
    pr3 = ConsProyecto "municipalidad"
    pr4 =  ConsProyecto "universidad"
    
    desa1= Developer Junior pr1 
    desa2= Developer SemiSenior pr1
    desa3= Developer Senior pr2
    desa4= Developer Senior pr4
    mana = Management Senior pr4
    desa5 = Developer SemiSenior pr4
    
    empresa = ConsEmpresa [desa1,desa2,desa3,desa4,mana,desa5]
    empresa2 = ConsEmpresa []
    variosProyectos = [pr4]
    
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

